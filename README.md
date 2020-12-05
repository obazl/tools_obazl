# OBazl Rules for Tools

## boolean options

In WORKSPACE.bazel:

In BUILD.bazel:

# ############### Bootstrap Bazel Tools ###############
# git_repository(
#     name = "obazl_tools_bazel",
#     remote = "https://github.com/obazl/tools_bazel",
#     branch = "main",
# )
# load("@obazl_tools_bazel//obazl:bootstrap.bzl", "obazl_tools_configure")
# obazl_tools_configure()

load("@obazl_tools_bazel//obazl:macros.bzl", "bool_opt")
VERBOSE = bool_opt(name = "verbose")
...
    opts = [VERBOSE],
