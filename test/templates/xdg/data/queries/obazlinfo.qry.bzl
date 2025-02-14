# bazel cquery $1 --output=starlark --starlark:file="./queries/ocamlinfo.bzl"

ocamlInfo = "@rules_ocaml//ocaml/_providers:ocaml.bzl%OcamlProvider"

moduleInfo = "@rules_ocaml//providers:moduleinfo.bzl%OCamlModuleInfo"

resolverInfo = "@rules_ocaml//ocaml/_providers:ocaml.bzl%OcamlNsResolverProvider"

ppxCodepsInfo = "@rules_ocaml//ppx:providers.bzl%PpxCodepsInfo"

def dump_cc_library_to_link(idx, lib):
    print("dump_cc_library_to_link")
    print("  alwayslink[{i}]: {al}".format(i=idx, al = lib.alwayslink))
    flds = ["static_library",
            "pic_static_library",
            "interface_library",
            "dynamic_library",]
    for fld in flds:
        if hasattr(lib, fld):
            if getattr(lib, fld):
                print("  lib[{i}].{f}: {p}".format(
                    i=idx, f=fld, p=getattr(lib,fld).path))
            else:
                print("  lib[{i}].{f} == None".format(i=idx, f=fld))

############################
def print_ocamlinfo(ps):
    if hasattr(ps[ocamlInfo], "sigs"):
        for dep in ps[ocamlInfo].sigs.to_list():
            print("sig: %s" % dep.path)

    if hasattr(ps[ocamlInfo], "ofiles"):
        for dep in ps[ocamlInfo].ofiles.to_list():
            print("ofiles: %s" % dep)

    if hasattr(ps[ocamlInfo], "archives"):
        for dep in ps[ocamlInfo].archives.to_list():
            print("archive: %s" % dep.path)

    if hasattr(ps[ocamlInfo], "afiles"):
        for dep in ps[ocamlInfo].afiles.to_list():
            print("afiles: %s" % dep)

    if hasattr(ps[ocamlInfo], "structs"):
        for dep in ps[ocamlInfo].structs.to_list():
            print("struct: %s" % dep)

    if hasattr(ps[ocamlInfo], "astructs"):
        for dep in ps[ocamlInfo].astructs.to_list():
            print("astruct: %s" % dep.path)

    # if hasattr(ps[ocamlInfo], "cmts"):
    #     print("cmts: %s" % ps[ocamlInfo].cmts)
        # for dep in ps[ocamlInfo].cmts.to_list():
        #     print("cmt: %s" % dep.path)

    # if hasattr(ps[ocamlInfo], "cmtis"):
    #     print("cmtis: %s" % ps[ocamlInfo].cmtis)
        # for dep in ps[ocamlInfo].cmtis.to_list():
        #     print("cmti: %s" % dep.path)

    if hasattr(ps[ocamlInfo], "cli_link_deps"):
        for dep in ps[ocamlInfo].cli_link_deps.to_list():
            print("link dep: %s" % dep.path)

###################
def format(target):
    ps = providers(target)

    if True: # target.label.name == "Colors":
        print("T: %s" % target)

    if ocamlInfo in ps:
        print_ocamlinfo(ps)

    if resolverInfo in ps:
        print("RESOLVER: %s" % ps[resolverInfo])

    if ppxCodepsInfo in ps:
        print("CODEPS astructs:")
        for dep in ps[ppxCodepsInfo].astructs.to_list():
            print(" codep astruct: %s" % dep)
        print("CODEPS linkdeps:")
        for dep in ps[ppxCodepsInfo].cli_link_deps.to_list():
            print(" codep link codep: %s" % dep.path)

        # if target.label.name == "expander":
        #     fail("expander")

    if moduleInfo in ps:
        print("module info:")
        print("m name: %s" % ps[moduleInfo].name)
        print("m struct: %s" % ps[moduleInfo].struct)
        print("m ofile: %s" % ps[moduleInfo].ofile)
        print("m sig: %s" % ps[moduleInfo].sig)
        print("m namespaced?: %s" % ps[moduleInfo].namespaced)
        print("m ns resolver: %s" % ps[moduleInfo].ns_resolver)

    if "CcInfo" in ps:
        print("CcInfo: %s" % ps["CcInfo"])
        print("dumping linking_context")
        cc_info = ps["CcInfo"]
        linking_ctx     = cc_info.linking_context
        linker_inputs = linking_ctx.linker_inputs.to_list()
        print("linker_inputs count: %s" % len(linker_inputs))
        lidx = 0
        for linput in linker_inputs:
            print(" linker_input[{i}]".format(i=lidx))
            print(" linkflags[{i}]: {f}".format(i=lidx, f= linput.user_link_flags))
            libs = linput.libraries
            print(" libs count: %s" % len(libs))
            if len(libs) > 0:
                i = 0
                for lib in linput.libraries:
                    dump_cc_library_to_link(i, lib)
                    i = i+1
            lidx = lidx + 1

    print("runfiles:")
    for rf in target.default_runfiles.files.to_list():
        print("rf: %s" % rf.path)
    for rf in target.default_runfiles.symlinks.to_list():
        print("symlink: %s" % rf.path)
    for rf in target.default_runfiles.root_symlinks.to_list():
        print("rsymlink: %s" % rf.path)

    return "Done"
