# generated file - DO NOT EDIT

load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "sdk", # runtime?
    srcs = select({
        "@rules_ocaml//cfg/runtime/sys:std?": ["lib/libasmrun.a"],
        "@rules_ocaml//cfg/runtime/sys:pic?": ["lib/libasmrun_pic.a"],
        "@rules_ocaml//cfg/runtime/sys:dbg?": ["lib/libasmrund.a"],
        "@rules_ocaml//cfg/runtime/sys:instrumented?": ["lib/libasmruni.a"],
        "@rules_ocaml//cfg/runtime/sys:shared?": ["lib/libasmrun_shared.so"],

        "@rules_ocaml//cfg/runtime/vm:std?": ["lib/libcamlrun.a"],
        "@rules_ocaml//cfg/runtime/vm:pic?": ["lib/libcamlrun_pic.a"],
        "@rules_ocaml//cfg/runtime/vm:dbg?": ["lib/libcamlrund.a"],
        "@rules_ocaml//cfg/runtime/vm:instrumented?": ["lib/libcamlruni.a"],
        "@rules_ocaml//cfg/runtime/vm:shared?": ["lib/libcamlrun_shared.so"],

        "//conditions:default": ["lib/libasmrun.a"]
    }),
    hdrs = glob(["caml/**"]),
    visibility = ["//visibility:public"],
)
