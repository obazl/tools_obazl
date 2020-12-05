# Copyright 2019 Gregg Reynolds
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

###############################
def _tools_repo_impl(repo_ctx):
    repo_ctx.report_progress("Bootstrapping obazl_tools repo")

    repo_ctx.file("WORKSPACE.bazel", "workspace = ( \"obazl_tools\" )", False)

    repo_ctx.file(
        "BUILD.bazel",
        content = "exports_files(glob([\"lib/**/*\"]))",
        executable = False,
    )

##############################
_tools_repo = repository_rule(
    implementation = _tools_repo_impl,
    local = True,
)

#######################
def _obazl_tools_configure():
    _tools_repo(name="obazl_tools")

################################################################
# export:
obazl_tools_configure = _obazl_tools_configure
