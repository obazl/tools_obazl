genrule(
    name = "exit",
    outs = ["ppx_driver.ml"],
    cmd = "\n".join([
        "echo \"(* GENERATED FILE - DO NOT EDIT *)\" > \"$@\"",
        "echo \"let () = Ppx_inline_test_lib.Runtime.exit ();;\""
    ])
)
