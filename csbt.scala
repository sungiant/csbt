/*
 *                ___.    __   
 *    ____   _____\_ |___/  |_ 
 *  _/ ___\ /  ___/| __ \   __\
 *  \  \___ \___ \ | \_\ \  |  
 *   \___  >____  >|___  /__|  
 *       \/     \/     \/      
 *
 *   Copyright © Ash Pook 2018
 * 
 *   A simple build tool for C# projects.
 *   - Supports projects and solutions defined in simple json files, much like the first version of .net core.
 *   - Supports compilation using mcs or csc (where possible).
 *   - Supports text transform pre processing.
 *   - Supports dependencies using Nuget and/or local packages.
 */

package csbt


/* Json Model                                                                                                         */
/**********************************************************************************************************************/

object JsonModel {
  case class Package (
    id            : String,
    version       : String)

  case class Project (
    output        : String,
    target        : String,
    sources       : List[String],
    references    : List[String],
    unsafe        : Option[Boolean],
    clscheck      : Option[Boolean],
    doc           : Option[String])

  case class Config (
    compiler      : Option[String] = None,
    sdk           : Option[String] = None)

  case class Solution (
    id            : String,
    projects      : List[Project],
    version       : Option[String],
    description   : Option[String],
    copyright     : Option[String],
    config        : Option[Config],
    packages      : Option[List[Package]])
}


/* Memory Model                                                                                                       */
/**********************************************************************************************************************/

object Model {

  import java.util.UUID

  case class Package (
    id            : String,
    version       : String)
  object Package {
    def fromJson (j: JsonModel.Package): Package = Package (
      j.id,
      j.version)
  }

  case class Project (
    output        : String,
    target        : String,
    wildcards     : List[String],
    references    : List[String],
    unsafe        : Boolean,
    clscheck      : Boolean,
    doc           : Option[String]) {
    lazy val sources: List[String] = wildcards.flatMap (IO.wild (".", _))
    lazy val commonBase: String = IO.commonPath (sources)
    lazy val uuid: String = UUID.randomUUID.toString.toUpperCase
  }
  object Project {
    def fromJson (j: JsonModel.Project): Project = Project (
      j.output,
      j.target,
      j.sources,
      j.references,
      j.unsafe.getOrElse (false),
      j.clscheck.getOrElse (false),
      j.doc)
  }

  case class Config (
    compiler      : String,
    sdk           : String)
  object Config {
    def defaultCompiler = "mcs"
    def defaultSdk = "4.5"
    def fromJson (j: JsonModel.Config): Config = Config (
      j.compiler.getOrElse (defaultCompiler),
      j.sdk.getOrElse (defaultSdk))

    def default () = Config (defaultCompiler, defaultSdk)
  }

  case class Solution (
    id                      : String,
    projects                : List[Project],
    version                 : String,
    config                  : Config,
    packages                : List[Package])
  object Solution {
    def fromJson (j: JsonModel.Solution): Solution = Solution (
      j.id,
      j.projects.map (Project.fromJson),
      j.version.getOrElse ("0.0.1"),
      j.config.map (Config.fromJson).getOrElse (Config.default),
      j.packages.getOrElse (Nil).map (Package.fromJson))
  }
}


/* Program                                                                                                            */
/**********************************************************************************************************************/

object Program {

  import java.util.concurrent.Executors

  import scala.concurrent.ExecutionContext
  import scala.util.{Try, Success, Failure}
  import scala.io.StdIn
  import cats._
  import io.circe._
  import io.circe.jawn._
  import io.circe.generic.auto._
  import cats._
  import cats.implicits._

  case class Command (program: String, arguments: List[String]) {
    override def toString = program + " " + arguments.mkString (" ")
  }

  trait CompilerInterface {
    def command (project: Model.Project): Command
  }

  trait Action
  object Action {
    object Resolve extends Action
    object Generate extends Action
    object Compile extends Action
  }

  case class Args (action: Option[Action] = None, wd: Option[String] = None)

  def resolve (packages: List[Model.Package]) = { 
    println (s"*** Removing packages.")
    IO.deleteDirectory ("packages")
    IO.createDirectory ("packages")

    println (s"*** Writing nuget config file.")
    val packagesFileContent = (
      """<?xml version="1.0" encoding="utf-8"?>""" ::
      "<packages>" ::
      packages.map (n => s"""  <package id="${n.id}" version="${n.version}"/>""") :::
      "</packages>" ::
      Nil).mkString ("\n")

    IO.writeFile ("packages.config", packagesFileContent)

    println (s"*** Resolving nuget dependencies.")

    import sys.process._

    "nuget install -o packages/" !

    //IO.deleteFile ("packages.config")
  }

  def main (args: Array[String]): Unit = {
    println ("C# Build Tool")

    implicit val rs: scopt.Read[Option[String]] = scopt.Read.reads { Option (_) }
    implicit val ra: scopt.Read[Option[Action]] = scopt.Read.reads {
      case "resolve" => Some (Action.Resolve)
      case "generate" => Some (Action.Generate)
      case "compile" => Some (Action.Compile)
      case _ => None
    }
    val parser = new scopt.OptionParser[Args]("csbt") {
      head("csbt", "0.0.x")

      opt[Option[Action]] ('a', "action")
        .action ((x, c) => c.copy (action = x))
        .text ("The action to perform.")
        .required ()
        .withFallback (() => None)

      opt[Option[String]] ('p', "wd")
        .action ((x, c) => c.copy (wd = x))
        .text ("The working directory.")
        .required ()
        .withFallback (() => None)
    
    }

    val arguments: Args = parser.parse(args, Args()).getOrElse (Args())
    println (arguments)

    arguments.wd.map { wd =>
      System.setProperty ("user.dir", wd)
    }

    println ("*** Working directory: " + System.getProperty ("user.dir"))

    println ("*** Loading Solution")

    val solutionJson = IO.decodeJsonFile[JsonModel.Solution] ("build.csbt")

    val solution = Model.Solution.fromJson (solutionJson)

    println (s"*** Found Solution: ${solution.id} @ ${solution.version}")

    arguments.action.map {
      case Action.Resolve =>
        resolve (solution.packages)
      case Action.Generate =>
        IO.deleteDirectory ("projects")
        IO.createDirectory ("projects")

        val generator = Generator

        generator.targets.foreach { target =>
          IO.createDirectory (s"projects/$target")
          generator.run (solution, target).map { case (k, v) => IO.writeFile (s"projects/$target/$k", v) }
        }

      case Action.Compile =>
        IO.deleteDirectory ("bin")
        IO.createDirectory ("bin/" + solution.config.compiler)

        val buildOrder: List[String] = solution.projects.map (_.output)

        val compiler: CompilerInterface = solution.config.compiler match {
          case "csc" => Csc
          case _ => Mcs
        }

        buildOrder.foreach { o => 
          println (s"   >> Compiling $o")
          val p = solution.projects.find (x => x.output == o).get
          val cmd = compiler.command (p)
          println (cmd)
          import sys.process._
          import java.io.File

          Process (cmd.toString, new File (System.getProperty ("user.dir"))) !
          
        }
    }
  }
}


/* IO                                                                                                                 */
/**********************************************************************************************************************/

object IO { // All paths here are expected to use / not \\

  import java.util.concurrent.Executors

  import scala.concurrent.ExecutionContext
  import scala.util.{Try, Success, Failure}
  import scala.io.StdIn
  import scala.util.matching.Regex
  import io.circe._
  import io.circe.jawn._
  import java.io._
  import org.apache.commons.io.FileUtils
  import org.apache.commons.io.filefilter._

  implicit class StringExtensions (val t: String) {
    def | () = t.stripMargin ('|')
    def \ () = t.replace ('/', '\\')
    def / () = t.replace ('\\', '/')
    def ESC () = t.replace ("(", "%28").replace (")", "%29")
  }

  def workingDir: String = new File (System.getProperty ("user.dir")).getCanonicalPath./ + "/"

  private [IO] implicit def toFile (p: String): File = {
    val path = p./
    val f1 = new File (path)
    if (f1.getCanonicalPath./ == path) f1
    else new File (workingDir + path)
  }

  def deleteDirectory (path: String): Unit = FileUtils.deleteDirectory(path)

  def createDirectory (path: String): Unit = {
    val f = new File (new File (path).getCanonicalPath./)
    println ("creating: " + f.getCanonicalPath./)
    f.mkdirs()
  }

  def deleteFile (path: String): Unit = FileUtils.forceDelete (path)

  def writeFile (path: String, content: String): Unit = {
    val f = toFile(path)
    println ("About to write file @ " + f.getCanonicalPath./)
    FileUtils.writeStringToFile (f, content, "UTF-8")
  }

  def readFile (path: String): String = Try (FileUtils.readFileToString (path, "UTF-8")) match {
    case Success (text) => text
    case Failure (ex) => throw new Exception (s"Failed to read file @ $path >> $ex}")
  }

  def decodeJsonFile[T](path: String)(implicit d: Decoder[T]): T = decode[T] (readFile (path)) match {
    case Right (config) => config
    case Left (error) => throw new Exception (s"Failed to decode json: ${error.getMessage}")
  }

  def find (path: String, filter: Regex): List[String] = {
    def recursiveListFiles(f: File): List[File] = {
      val these = f.listFiles.toList
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }

    recursiveListFiles (path)
      .filter (f => filter.findFirstIn (f.getName).isDefined)
      .map (_.getCanonicalPath./.replace (workingDir, ""))
  }

  def wild (path: String, pattern: String): List[String] = {
     val (pathPattern, filePattern) = pattern.splitAt (pattern.lastIndexOf ('/') + 1)
     val fileFilter = new WildcardFileFilter(filePattern)
     val col = FileUtils.listFiles (pathPattern, fileFilter, TrueFileFilter.INSTANCE)
     FileUtils.convertFileCollectionToFileArray (col).toList.map (_.getCanonicalPath./.replace (workingDir, ""))
  }

  def touch (path: String) = FileUtils.touch (path)

  def commonPath (pathsX: List[String]): String = {
    val paths = pathsX.map (_.replace ("\\", "/"))
    val SEP = "/"
    val BOUNDARY_REGEX = s"(?=[$SEP])(?<=[^$SEP])|(?=[^$SEP])(?<=[$SEP])"
    def common(a: List[String], b: List[String]): List[String] = (a, b) match {
      case (a :: as, b :: bs) if a equals b => a :: common(as, bs)
      case _ => Nil
    }
    if (paths.length < 2) paths.headOption.getOrElse("")
    else paths.map(_.split(BOUNDARY_REGEX).toList).reduceLeft(common).mkString
  }
}


/* Mcs Compiler Wrapper                                                                                               */
/**********************************************************************************************************************/
/*
 * $ mcs --help
 * Mono C# compiler, Copyright 2001-2011 Novell, Inc., 2011-2016 Xamarin, Inc, 2016-2017 Microsoft Corp
 * mcs [options] source-files
 *
 *    --about              About the Mono C# compiler
 *     -help               Lists all compiler options (short: -?)
 *     -helpinternal       Shows internal and advanced compiler options
 *
 *    --fatal[=COUNT]      Makes error after COUNT fatal
 *    --lint               Enhanced warnings
 *    --metadata-only      Produced assembly will contain metadata only
 *    --parse              Only parses the source file
 *    --runtime:VERSION    Sets mscorlib.dll metadata version: v1, v2, v4
 *    --stacktrace         Shows stack trace at error location
 *    --timestamp          Displays time stamps of various compiler events
 *     -v                  Verbose parsing (for debugging the parser)
 *    --mcs-debug X        Sets MCS debugging level to X
 *.   --break-on-ice       Breaks compilation on internal compiler error
 * 
 * Options can be of the form -option or /option
 * 
 */

object Mcs extends Program.CompilerInterface {

  private trait Platform
  private object Platform {
    object anycpu               extends Platform { override def toString = "anycpu" }
    object anycpu32bitpreferred extends Platform { override def toString = "anycpu32bitpreferred" }
    object arm                  extends Platform { override def toString = "arm" }
    object x86                  extends Platform { override def toString = "x86" }
    object x64                  extends Platform { override def toString = "x64" }
    object itanium              extends Platform { override def toString = "itanium" }

    implicit def fromString (s: String): Platform = s match {
      case "anycpu" => anycpu
      case "anycpu32bitpreferred" => anycpu32bitpreferred
      case "arm" => arm
      case "x86" => x86
      case "x64" => x64
      case "itanium" => itanium
    }
  }

  private trait Sdk
  private object Sdk {
    object `2`                  extends Sdk { override def toString = "2" }
    object `4`                  extends Sdk { override def toString = "4" }
    object `4.5`                extends Sdk { override def toString = "4.5" }

    implicit def fromString (s: String): Sdk = s match {
      case "2" => `2`
      case "4" => `4`
      case "4.5" => `4.5`
    }
  }

  private trait Target
  private object Target {
    object exe                  extends Target { override def toString = "exe" }
    object winexe               extends Target { override def toString = "winexe" }
    object library              extends Target { override def toString = "library" }
    object module               extends Target { override def toString = "module" }

    implicit def fromString (s: String): Target = s match {
      case "exe" => exe
      case "winexe" => winexe
      case "library" => library
      case "module" => module
    }
  }

  //ISO-1, ISO-2, 3, 4, 5, 6, Default or Experimental
  private trait Language
  private object Language {
    object `ISO-1`              extends Language { override def toString = "ISO-1" }
    object `ISO-2`              extends Language { override def toString = "ISO-2" }
    object `3`                  extends Language { override def toString = "3" }
    object `4`                  extends Language { override def toString = "4" }
    object `5`                  extends Language { override def toString = "5" }
    object `6`                  extends Language { override def toString = "6" }
    object Default              extends Language { override def toString = "Default" }
    object Experimental         extends Language { override def toString = "Experimental" }

    implicit def fromString (s: String): Language = s match {
      case "ISO-1" => `ISO-1`
      case "ISO-2" => `ISO-2`
      case "3" => `3`
      case "4" => `4`
      case "5" => `5`
      case "6" => `6`
      case "Default" => Default
      case "Experimental" => Experimental
    }
  }


  private case class Args (
    fullpaths                   : Option[Unit] = None,                              // -fullpaths                 Any issued error or warning uses absolute file path
    noconfig                    : Option[Unit] = None,                              // -noconfig                  Disables implicitly referenced assemblies
    
    checked                     : Option[Boolean] = None,                           // -checked[+|-]              Sets default aritmetic overflow context
    clscheck                    : Option[Boolean] = None,                           // -clscheck[+|-]             Disables CLS Compliance verifications
    debug                       : Option[Boolean] = None,                           // -debug[+|-], -g            Generate debugging information
    delaysign                   : Option[Boolean] = None,                           // -delaysign[+|-]            Only insert the public key into the assembly (no signing)
    optimize                    : Option[Boolean] = None,                           // -optimize[+|-]             Enables advanced compiler optimizations (short: -o)
    nostdlib                    : Option[Boolean] = None,                           // -nostdlib[+|-]             Does not reference mscorlib.dll library
    unsafe                      : Option[Boolean] = None,                           // -unsafe[+|-]               Allows to compile code which uses unsafe keyword
    warnaserror                 : Option[Boolean] = None,                           // -warnaserror[+|-]          Treats all warnings as errors
    
    codepage                    : Option[String] = None,                            // -codepage:ID               Sets code page to the one in ID (number, utf8, reset)
    doc                         : Option[String] = None,                            // -doc:FILE                  Process documentation comments to XML file
    main                        : Option[String] = None,                            // -main:CLASS                Specifies the class with the Main method (short: -m)
    keycontainer                : Option[String] = None,                            // -keycontainer:NAME         The key pair container used to sign the output assembly
    keyfile                     : Option[String] = None,                            // -keyfile:FILE              The key file used to strongname the ouput assembly
    out                         : Option[String] = None,                            // -out:FILE                  Specifies output assembly name
    win32res                    : Option[String] = None,                            // -win32res:FILE             Specifies Win32 resource file (.res)
    win32icon                   : Option[String] = None,                            // -win32icon:FILE            Use this icon for the output

    warn                        : Option[Int] = None,                               // -warn:0-4                  Sets warning level, the default is 4 (short -w:)
    langversion                 : Option[Language] = None,                          // -langversion:TEXT          Specifies language version: ISO-1, ISO-2, 3, 4, 5, 6, Default or Experimental
    platform                    : Option[Platform] = None,                          // -platform:ARCH             Specifies the target platform of the output assembly ARCH can be one of: anycpu, anycpu32bitpreferred, arm, x86, x64 or itanium. The default is anycpu.
    sdk                         : Option[Sdk] = None,                               // -sdk:VERSION               Specifies SDK version of referenced assemblies VERSION can be one of: 2, 4, 4.5 (default) or a custom value
    target                      : Option[Target] = None,                            // -target:KIND               Specifies the format of the output assembly (short: -t) KIND can be one of: exe, winexe, library, module
    
    recurse                     : Option[List[String]] = None,                      // -recurse:SPEC              Recursively compiles files according to SPEC pattern

    addmodule                   : Option[List[String]] = None,                      // -addmodule:M1[,Mn]         Adds the module to the generated assembly
    define                      : Option[List[String]] = None,                      // -define:S1[;S2]            Defines one or more conditional symbols (short: -d)
    lib                         : Option[List[String]] = None,                      // -lib:PATH1[,PATHn]         Specifies the location of referenced assemblies
    nowarn                      : Option[List[String]] = None,                      // -nowarn:W1[,Wn]            Suppress one or more compiler warnings
    pkg                         : Option[List[String]] = None,                      // -pkg:P1[,Pn]               References packages P1..Pn
    reference                   : Option[List[String]] = None,                      // -reference:A1[,An]         Imports metadata from the specified assembly (short: -r)
    linkresource                : Option[List[String]] = None,                      // -linkresource:FILE[,ID]    Links FILE as a resource (short: -linkres)
    resource                    : Option[List[String]] = None,                      // -resource:FILE[,ID]        Embed FILE as a resource (short: -res)

    pathmap                     : Option[Map[String, String]] = None,               // -pathmap:K=V[,Kn=Vn]       Sets a mapping for source path names used in generated output
    reference_alias             : Option[Map[String, String]] = None,               // -reference:ALIAS=A         Imports metadata using specified extern alias (short: -r)
    
    warnaserror_specific        : Option[Map[String, Boolean]] = None)              // -warnaserror[+|-]:W1[,Wn]  Treats one or more compiler warnings as errors

  def command (project: Model.Project): Program.Command = command { Args ()
    .copy (fullpaths = Some (()))
    .copy (noconfig = Some (()))
    .copy (lib = Some ("bin/mcs/" :: Nil))
    .copy (sdk = Some ("4.5"))
    .copy (target = Some (project.target))
    .copy (debug = Some (true))
    .copy (unsafe = Some (project.unsafe))
    .copy (define = Some ("DEBUG" :: Nil))
    .copy (out = Some ("bin/mcs/" + project.output + ".dll"))
    .copy (recurse = Some (project.wildcards))
    .copy (reference = Some (project.references.map (x => x + ".dll")))
  }

  private def command (args: Args): Program.Command = {
    val program = if (System.getProperty("os.name").startsWith("Windows")) "mcs.bat" else "mcs"
    val arguments = (
      args.fullpaths.map           (_ => "-fullpaths") ::
      args.noconfig.map            (_ => "-noconfig") ::

      args.checked.map             { if (_) "+" else "-" }.map ("-checked" + _) ::
      args.clscheck.map            { if (_) "+" else "-" }.map ("-clscheck" + _) ::
      args.debug.map               { if (_) "+" else "-" }.map ("-debug" + _) ::
      args.delaysign.map           { if (_) "+" else "-" }.map ("-delaysign" + _) ::
      args.optimize.map            { if (_) "+" else "-" }.map ("-optimize" + _) ::
      args.nostdlib.map            { if (_) "+" else "-" }.map ("-nostdlib" + _) ::
      args.unsafe.map              { if (_) "+" else "-" }.map ("-unsafe" + _) ::
      args.warnaserror.map         { if (_) "+" else "-" }.map ("-warnaserror" + _) ::

      args.codepage.map            ("-codepage:" + _) ::
      args.doc.map                 ("-doc:" + _) ::
      args.main.map                ("-main:" + _) ::
      args.keycontainer.map        ("-keycontainer:" + _) ::
      args.keyfile.map             ("-keyfile:" + _) ::
      args.keyfile.map             ("-keyfile:" + _) ::
      args.out.map                 ("-out:" + _) ::
      args.win32res.map            ("-win32res:" + _) ::
      args.win32icon.map           ("-win32icon:" + _) ::

      args.warn.map                ("-warn:" + _) ::
      args.langversion.map         ("-langversion:" + _) ::
      args.platform.map            ("-platform:" + _) ::
      args.sdk.map                 ("-sdk:" + _) ::
      args.target.map              ("-target:" + _) ::

      args.recurse.map             { _.map (y => Some ("-recurse:" + y)) }.getOrElse (Nil) :::

      args.addmodule.flatMap       { case Nil => None; case x => Some ("-addmodule:" + x.mkString (",")) } ::
      args.define.flatMap          { case Nil => None; case x => Some ("-define:" + x.mkString (";")) } ::
      args.lib.flatMap             { case Nil => None; case x => Some ("-lib:" + x.mkString (",")) } ::
      args.nowarn.flatMap          { case Nil => None; case x => Some ("-nowarn:" + x.mkString (",")) } ::
      args.pkg.flatMap             { case Nil => None; case x => Some ("-pkg:" + x.mkString (",")) } ::
      args.reference.flatMap       { case Nil => None; case x => Some ("-reference:" + x.mkString (",")) } ::
      args.linkresource.flatMap    { case Nil => None; case x => Some ("-linkresource:" + x.mkString (",")) } ::
      args.resource.flatMap        { case Nil => None; case x => Some ("-resource:" + x.mkString (",")) } ::

      args.pathmap.map (_.toList).flatMap { case Nil => None; case x => Some ("-pathmap:" + x.map { case (k, v) => s"$k=$v" }.mkString (",")) } ::
      args.reference_alias.map (_.toList).getOrElse (Nil).map { case (k, v) => Some (s"-reference:$k=$v") } :::

      args.warnaserror_specific.map (_.toList).getOrElse (Nil).groupBy (_._2).toList.map { case (k, v) => (k, v.map (_._1)) }.map {
        case (true, w) => Some ("-warnaserror+" + w.mkString (","))
        case (false, w) => Some ("-warnaserror-:" + w.mkString (","))
      } :::
      Nil: List[Option[String]]
    ).collect { case Some (x) => x }

    Program.Command (program, arguments)
  }

}


/* Visual Studio .sln generator                                                                                       */
/**********************************************************************************************************************/

object Generator {

  implicit class SE (val t: String) {
    def | () = t.stripMargin ('|')
    def \ () = t.replace ('/', '\\')
    def / () = t.replace ('\\', '/')
    def ESC () = t.replace ("(", "%28").replace (")", "%29")
  }

  case class TargetConfig (id: String, projectTypeGuids: List[String], buildToolsPath: String)

  lazy val csProjectGuid = "FAE04EC0-301F-11D3-BF4B-00C04F79EFBC"

  private lazy val tagetConfigs: List[TargetConfig] =
    TargetConfig (
      "std",
      csProjectGuid :: Nil,
      "$(MSBuildToolsPath)\\Microsoft.CSharp.targets") ::
    TargetConfig (
      "ios",
      "FEACFBD2-3405-455C-9665-78FE426C6842" :: csProjectGuid :: Nil,
      "$(MSBuildExtensionsPath)\\Xamarin\\iOS\\Xamarin.iOS.CSharp.targets") ::
    TargetConfig (
      "mac",
      "948B3504-5B70-4649-8FE4-BDE1FB46EC69" :: csProjectGuid :: Nil,
      "$(MSBuildBinPath)\\Microsoft.CSharp.targets") ::
    Nil

  lazy val targets: List[String] = tagetConfigs.map (_.id)

  lazy val relative_path_to_root = "..\\..\\"


  def run (solution: Model.Solution, target: String): Map [String, String] = {

    val targetConfig = tagetConfigs.groupBy (_.id).get (target).flatMap (_.headOption).get
    
    val solution_projects = solution.projects.map (_.output)
    val sln_name: String = s"${solution.id} (${targetConfig.id}).sln"
    val sln_content: String =
      s"""|
          |Microsoft Visual Studio Solution File, Format Version 12.00
          |# Visual Studio 12
          |""".| +
      solution.projects.foldLeft ("")((a, i) => a +
      s"""|Project("{${csProjectGuid}}") = "${i.output}", "${i.output} (${targetConfig.id}).csproj", "{${i.uuid}}"
          |EndProject
          |""".|) +
      s"""|Global
          |  GlobalSection(SolutionConfigurationPlatforms) = preSolution
          |    Debug|Any CPU = Debug|Any CPU
          |    Release|Any CPU = Release|Any CPU
          |  EndGlobalSection
          |  GlobalSection(ProjectConfigurationPlatforms) = postSolution
          |""".| +
      solution.projects.foldLeft ("") ((a, i) => a +
      s"""|    {${i.uuid}}.Debug|Any CPU.ActiveCfg = Debug|Any CPU
          |    {${i.uuid}}.Debug|Any CPU.Build.0 = Debug|Any CPU
          |    {${i.uuid}}.Release|Any CPU.ActiveCfg = Release|Any CPU
          |    {${i.uuid}}.Release|Any CPU.Build.0 = Release|Any CPU
          |""".|) +
      s"""|  EndGlobalSection
          |  GlobalSection(SolutionProperties) = preSolution
          |    HideSolutionNode = FALSE
          |  EndGlobalSection
          |EndGlobal
          |""".|

    (
      (sln_name -> sln_content) ::
      solution.projects.map { p =>

        val internal_references = p.references.filter (solution_projects.contains)
        val external_references = p.references.filterNot (solution_projects.contains)
        val external_package_references = external_references.map (x => (x, IO.find ("packages/", x.r).headOption)).collect { case (x, Some (y)) => (x, y) }
        val external_system_references = external_references.filterNot (external_package_references.map (_._1).contains)


        val csproj_content =
          s"""|<?xml version="1.0" encoding="utf-8"?>
              |<Project ToolsVersion="4.0" DefaultTargets="Build" xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
              |  <PropertyGroup>
              |    <Platform Condition=" '$$(Platform)' == '' ">AnyCPU</Platform>
              |    <ProjectGuid>{${p.uuid}}</ProjectGuid>
              |    <ProjectTypeGuids>${targetConfig.projectTypeGuids.map (x => s"{$x}").mkString (";")}</ProjectTypeGuids>
              |    <OutputType>Library</OutputType>
              |    <AppDesignerFolder>Properties</AppDesignerFolder>
              |    <RootNamespace>${p.output}</RootNamespace>
              |    <AssemblyName>${p.output}</AssemblyName>
              |<!--<TargetFrameworkVersion>v4.5</TargetFrameworkVersion>-->
              |    <FileAlignment>512</FileAlignment>
              |  </PropertyGroup>
              |  <PropertyGroup Condition=" '$$(Configuration)|$$(Platform)' == 'Debug|AnyCPU' ">
              |    <DebugSymbols>true</DebugSymbols>
              |    <DebugType>full</DebugType>
              |    <Optimize>false</Optimize>
              |    <OutputPath>${relative_path_to_root}bin\\${targetConfig.id}\\Debug\\</OutputPath>
              |    <DefineConstants>DEBUG;TRACE</DefineConstants>
              |    <ErrorReport>prompt</ErrorReport>
              |    <WarningLevel>4</WarningLevel>
              |  </PropertyGroup>
              |  <PropertyGroup Condition=" '$$(Configuration)|$$(Platform)' == 'Release|AnyCPU' ">
              |    <DebugType>pdbonly</DebugType>
              |    <Optimize>true</Optimize>
              |    <OutputPath>${relative_path_to_root}bin\\${targetConfig.id}\\Release\\</OutputPath>
              |    <DefineConstants>TRACE</DefineConstants>
              |    <ErrorReport>prompt</ErrorReport>
              |    <WarningLevel>4</WarningLevel>
              |  </PropertyGroup>
              |  <ItemGroup>
              |""".| +
              external_system_references.foldLeft ("") ((a, i) => a +
          s"""|    <Reference Include="${i}" />
              |""".|) +
          s"""|  </ItemGroup>
              |  <ItemGroup>
              |""".| +
              external_package_references.foldLeft ("") { case (a, (x, y)) => a +
          s"""|    <Reference Include="${x}">
              |      <HintPath>${relative_path_to_root}${y.\}</HintPath>
              |    </Reference>
              |""".|} +
          s"""|  </ItemGroup>
              |  <ItemGroup>
              |""".| +
              p.sources.map(_.\).foldLeft ("") ((a, i) => a +
          s"""|    <Compile Include="${relative_path_to_root}${i}" >
              |      <Link>${i.replace (p.commonBase.\, "")}</Link>
              |    </Compile>
              |""".|) +
          s"""|  </ItemGroup>
              |  <ItemGroup>
              |""".| +
              internal_references.map(_.\).foldLeft ("") { (a, i) =>
                val ref_uuid = solution.projects.find (x => x.output == i).get.uuid
                val ref_escaped_csproj = s"$i (${targetConfig.id}).csproj".ESC
                a +
          s"""|    <ProjectReference Include="$ref_escaped_csproj">
              |      <Project>{$ref_uuid}</Project>
              |      <Name>$i</Name>
              |    </ProjectReference>
              |""".| } +
          s"""|  </ItemGroup>
              |  <Import Project="${targetConfig.buildToolsPath}" />
              |</Project>
              |""".|
        (s"${p.output} (${targetConfig.id}).csproj", csproj_content)
      } ::: Nil
    ).toMap
  }
}


/* Csc Compiler Wrapper                                                                                               */
/**********************************************************************************************************************/

/*
 * $ csc /help
 * Microsoft (R) Visual C# Compiler version 2.3.0.61801 (3722bb71)
 * Copyright (C) Microsoft Corporation. All rights reserved.
 * 
 * 
 *                               Visual C# Compiler Options

 exe: a console executable, winexe: a Windows executable, library: a library, module: a module that can be added to another assembly, appcontainerexe: an Appcontainer executable, winmdobj: a Windows Runtime intermediate file that is consumed by WinMDExp
 * 
 *                         - OUTPUT FILES -
 *  /out:<file>                   Specify output file name (default: base name of file with main class or first file)
 *  /target:exe                   Build a console executable (default) (Short form: /t:exe)
 *  /target:winexe                Build a Windows executable (Short form: /t:winexe)
 *  /target:library               Build a library (Short form: /t:library)
 *  /target:module                Build a module that can be added to another assembly (Short form: /t:module)
 *  /target:appcontainerexe       Build an Appcontainer executable (Short form: /t:appcontainerexe)
 *  /target:winmdobj              Build a Windows Runtime intermediate file that is consumed by WinMDExp (Short form: /t:winmdobj)
 *  /doc:<file>                   XML Documentation file to generate
 *  /refout:<file>                Reference assembly output to generate
 *  /platform:<string>            Limit which platforms this code can run on: x86, Itanium, x64, arm, anycpu32bitpreferred, or anycpu. The default is anycpu.
 * 
 *                         - INPUT FILES -
 *  /recurse:<wildcard>           Include all files in the current directory and subdirectories according to the wildcard specifications
 *  /reference:<alias>=<file>     Reference metadata from the specified assembly
 *                                file using the given alias (Short form: /r)
 *  /reference:<file list>        Reference metadata from the specified assembly
 *                                files (Short form: /r)
 *  /addmodule:<file list>        Link the specified modules into this assembly
 *  /link:<file list>             Embed metadata from the specified interop
 *                                assembly files (Short form: /l)
 *  /analyzer:<file list>         Run the analyzers from this assembly
 *                                (Short form: /a)
 *  /additionalfile:<file list>   Additional files that don't directly affect code
 *                                generation but may be used by analyzers for producing
 *                                errors or warnings.
 *  /embed                        Embed all source files in the PDB.
 *  /embed:<file list>            Embed specific files in the PDB
 * 
 *                         - RESOURCES -
 *  /win32res:<file>              Specify a Win32 resource file (.res)
 *  /win32icon:<file>             Use this icon for the output
 *  /win32manifest:<file>         Specify a Win32 manifest file (.xml)
 *  /nowin32manifest              Do not include the default Win32 manifest
 *  /resource:<resinfo>           Embed the specified resource (Short form: /res)
 *  /linkresource:<resinfo>       Link the specified resource to this assembly
 *                                (Short form: /linkres) Where the resinfo format
 *                                is <file>[,<string name>[,public|private]]
 * 
 *                         - CODE GENERATION -
 *  /debug[+|-]                   Emit debugging information
 *  /debug:{full|pdbonly|portable|embedded}
 *                                Specify debugging type ('full' is default,
 *                                'portable' is a cross-platform format,
 *                                'embedded' is a cross-platform format embedded into
 *                                the target .dll or .exe)
 *  /optimize[+|-]                Enable optimizations (Short form: /o)
 *  /deterministic                Produce a deterministic assembly
 *                                (including module version GUID and timestamp)
 *  /refonly                      Produce a reference assembly in place of the main output
 *  /instrument:TestCoverage      Produce an assembly instrumented to collect
 *                                coverage information
 *  /sourcelink:<file>            Source link info to embed into PDB.
 * 
 *                         - ERRORS AND WARNINGS -
 *  /warnaserror[+|-]             Report all warnings as errors
 *  /warnaserror[+|-]:<warn list> Report specific warnings as errors
 *  /warn:<n>                     Set warning level (0-4) (Short form: /w)
 *  /nowarn:<warn list>           Disable specific warning messages
 *  /ruleset:<file>               Specify a ruleset file that disables specific
 *                                diagnostics.
 *  /errorlog:<file>              Specify a file to log all compiler and analyzer
 *                                diagnostics.
 *  /reportanalyzer               Report additional analyzer information, such as
 *                                execution time.
 * 
 *                         - LANGUAGE -
 *  /checked[+|-]                 Generate overflow checks
 *  /unsafe[+|-]                  Allow 'unsafe' code
 *  /define:<symbol list>         Define conditional compilation symbol(s) (Short
 *                                form: /d)
 *  /langversion:<string>         Specify language version mode: ISO-1, ISO-2, 3,
 *                                4, 5, 6, 7, 7.1, Default, or Latest
 * 
 *                         - SECURITY -
 *  /delaysign[+|-]               Delay-sign the assembly using only the public
 *                                portion of the strong name key
 *  /publicsign[+|-]              Public-sign the assembly using only the public
 *                                portion of the strong name key
 *  /keyfile:<file>               Specify a strong name key file
 *  /keycontainer:<string>        Specify a strong name key container
 *  /highentropyva[+|-]           Enable high-entropy ASLR
 * 
 *                         - MISCELLANEOUS -
 *  @<file>                       Read response file for more options
 *  /help                         Display this usage message (Short form: /?)
 *  /nologo                       Suppress compiler copyright message
 *  /noconfig                     Do not auto include CSC.RSP file
 *  /parallel[+|-]                Concurrent build.
 *  /version                      Display the compiler version number and exit.
 * 
 *                         - ADVANCED -
 *  /baseaddress:<address>        Base address for the library to be built
 *  /checksumalgorithm:<alg>      Specify algorithm for calculating source file
 *                                checksum stored in PDB. Supported values are:
 *                                SHA1 (default) or SHA256.
 *  /codepage:<n>                 Specify the codepage to use when opening source
 *                                files
 *  /utf8output                   Output compiler messages in UTF-8 encoding
 *  /main:<type>                  Specify the type that contains the entry point
 *                                (ignore all other possible entry points) (Short
 *                                form: /m)
 *  /fullpaths                    Compiler generates fully qualified paths
 *  /filealign:<n>                Specify the alignment used for output file
 *                                sections
 *  /pathmap:<K1>=<V1>,<K2>=<V2>,...
 *                                Specify a mapping for source path names output by
 *                                the compiler.
 *  /pdb:<file>                   Specify debug information file name (default:
 *                                output file name with .pdb extension)
 *  /errorendlocation             Output line and column of the end location of
 *                                each error
 *  /preferreduilang              Specify the preferred output language name.
 *  /nostdlib[+|-]                Do not reference standard library (mscorlib.dll)
 *  /subsystemversion:<string>    Specify subsystem version of this assembly
 *  /lib:<file list>              Specify additional directories to search in for
 *                                references
 *  /errorreport:<string>         Specify how to handle internal compiler errors:
 *                                prompt, send, queue, or none. The default is
 *                                queue.
 *  /appconfig:<file>             Specify an application configuration file
 *                                containing assembly binding settings
 *  /moduleassemblyname:<string>  Name of the assembly which this module will be
 *                                a part of
 *  /modulename:<string>          Specify the name of the source module
 * 
 * 
 */

object Csc extends Program.CompilerInterface {

  private case class ResInfo (
    file: String,
    name: String,
    access: String // public|private
  )

  private case class Args (
    // output files
    out                   : Option[String] = None,                        // /out:<file>                   Specify output file name (default: base name of file with main class or first file)
    target                : Option[String] = None,                        // /target:<kind>                Build [exe]: a console executable, [winexe]: a Windows executable, [library]: a library, [module]: a module that can be added to another assembly, [appcontainerexe]: an Appcontainer executable, [winmdobj]: a Windows Runtime intermediate file that is consumed by WinMDExp
    doc                   : Option[String] = None,                        // /doc:<file>                   XML Documentation file to generate
    refout                : Option[String] = None,                        // /refout:<file>                Reference assembly output to generate
    platform              : Option[String] = None,                        // /platform:<string>            Limit which platforms this code can run on: [x86], [Itanium], [x64], [arm], [anycpu32bitpreferred], or [anycpu]. The default is [anycpu].
    // input files
    recurse               : Option[List[String]] = None,                  // /recurse:<wildcard>           Include all files in the current directory and subdirectories according to the wildcard specifications
    reference             : Option[List[String]] = None,
    reference_alias       : Option[Map[String, String]] = None,
    add_module            : Option[List[String]] = None,
    link                  : Option[List[String]] = None,
    analyzer              : Option[List[String]] = None,
    additionalfile        : Option[List[String]] = None,
    embed                 : Option[Unit] = None,
    embed_specific        : Option[List[String]] = None,
    // resources
    win32res              : Option[String] = None,
    win32icon             : Option[String] = None,
    win32manifest         : Option[String] = None,
    nowin32manifest       : Option[Boolean] = None,
    resource              : Option[List[ResInfo]] = None,
    linkresource          : Option[List[ResInfo]] = None,
    // code generation
    debug                 : Option[Boolean] = None,
    debug_type            : Option[String] = None,                        // full|pdbonly|portable|embedded
    optimize              : Option[Boolean] = None,
    deterministic         : Option[Unit] = None,
    refonly               : Option[Unit] = None,
    instrument            : Option[Unit] = None,
    sourcelink            : Option[List[String]] = None,
    // errors and warnings
    warnaserror           : Option[Boolean] = None,                       // Report all warnings as errors
    warnaserror_specific  : Option[List[String]] = None,                  // Report specific warnings as errors
    warn                  : Option[Int] = None,                           // Set warning level (0-4) (Short form: /w)
    nowarn                : Option[List[String]] = None,                  // Disable specific warning messages 
    ruleset               : Option[String] = None,                        // Specify a ruleset file that disables specific diagnostics.
    errorlog              : Option[String] = None,                        // Specify a file to log all compiler and analyzer diagnostics.
    reportanalyzer        : Option[Unit] = None                           // Report additional analyzer information, such as execution time.
  )

  def command (project: Model.Project): Program.Command = command { Args ()
    .copy (target = Some (project.target))
    .copy (out = Some ("bin/csc/" + project.output))
    .copy (recurse = Some (project.sources))
  }

  private def command (args: Args): Program.Command = ??? /* Program.Command ("csc", (
    // output files
    args.out.map                  { x => Some (s"/out:$x") } ::
    args.target.map               { x => Some (s"/target:$x") } ::
    args.doc.map                  { x => Some (s"/doc:$x") } ::
    args.refout.map               { x => Some (s"/refout:$x") } ::
    args.platform.map             { x => Some (s"/platform:$x") } ::
    // input files
    args.recurse.map              { x => x.map (s => Some (s"/recurse:$s")) }.getOrElse (Nil) :::
  
    Nil).collect { case Some (x) => x })*/

}

