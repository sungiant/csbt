name := "csbt"

scalaVersion := "2.12.5"

logLevel := Level.Info

libraryDependencies ++=
  "org.typelevel"                   %% "cats-core"                  % "1.0.1" ::
  "io.circe"                        %% "circe-core"                 % "0.9.3" ::
  "io.circe"                        %% "circe-generic"              % "0.9.3" ::
  "io.circe"                        %% "circe-jawn"                 % "0.9.3" ::
  "org.apache.commons"              %  "commons-io"                 % "1.3.2" ::
  "com.github.scopt"                %% "scopt"                      % "3.7.0" ::
  Nil

outputStrategy := Some (StdoutOutput)

connectInput in run := true

fork in run := true

lazy val install = taskKey[Unit]("Execute the install script")

install := { "sh install.sh" ! }
