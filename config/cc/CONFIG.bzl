load("@cc_config//:CONFIG.bzl",
     # _BASE_DEPS     = "BASE_DEPS",
     # _BASE_INCLUDE_PATHS = "BASE_INCLUDE_PATHS",
     _BASE_COPTS    = "BASE_COPTS",
     _BASE_LINKOPTS = "BASE_LINKOPTS")
     # _BASE_DEFINES  = "BASE_DEFINES",

BASE_SRCS          = []
BASE_DEPS          = [] #_BASE_DEPS
BASE_INCLUDE_PATHS = [] #_BASE_INCLUDE_PATHS
BASE_COPTS         = _BASE_COPTS
BASE_LINKOPTS      = _BASE_LINKOPTS
BASE_DEFINES       = select({
    "//config/profile:dev?": ["DEVBUILD", "TRACING"],
    "//config/trace:trace?": ["TRACING"],
    "//conditions:default": []
})
