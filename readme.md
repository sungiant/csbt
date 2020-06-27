# csbt

[![Build Status](https://img.shields.io/travis/sungiant/csbt)][travis]
[![License](https://img.shields.io/github/license/sungiant/csbt)][license]

This repository contains the source for `csbt`, a super simple C# build tool.  Right now this project is essentially a proof of concept.

`csbt` was inspired by the simplicty of the original .NET Core build tool that enabled developers to define projects and solutions exculsively with simple JSON files.

`csbt` currently supports the following operations:

  * defining projects and solutions with simple json files.
  * compilation of solutions using mcs.
  * `t4` text transform pre processing.
  * external dependencies via `nuget` and/or local packages.
  * Visual Studio project and solution generation.

## License

`csbt` is licensed under the **[MIT License][license]**; you may not use this software except in compliance with the License.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

[travis]: https://travis-ci.org/sungiant/csbt
[license]: https://raw.githubusercontent.com/sungiant/csbt/master/LICENSE
