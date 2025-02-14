load("@obazl_cc//config:BASE.bzl",
     _BASE_COPTS    = "BASE_COPTS",
     _define_module_version = "define_module_version")
     # _BASE_LOCAL_DEFINES = "BASE_LOCAL_DEFINES")

define_module_version = _define_module_version
PROFILE            = ["PROFILE_$(COMPILATION_MODE)"]

BASE_SRCS          = []
BASE_DEPS          = []
BASE_INCLUDE_PATHS = []
BASE_COPTS         = _BASE_COPTS
BASE_LINKOPTS      = []
BASE_LOCAL_DEFINES = [
]
BASE_DEFINES       = []
