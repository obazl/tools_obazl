#include <unistd.h>

/* #include "bazel_config.h" */
/* #include "mibl_config.h" */
/* #include "s7_config.h" */
/* #include "ansi_colors.h" */
/* #include "load_dune.h" */

#include "mibl.h"
#include "convert.h"

extern bool debug;
extern bool trace;
extern bool verbose;

extern char *ews_root;
extern int dir_ct;
extern int file_ct;
extern int dunefile_ct;

int main(int argc, char *argv[])
{
    char *opts = "hdtv";
    int opt;
    while ((opt = getopt(argc, argv, opts)) != -1) {
        switch (opt) {
        case '?':
            fprintf(stderr, "uknown opt: %c", optopt);
            exit(EXIT_FAILURE);
            break;
        case ':':
            fprintf(stderr, "uknown option: %c", optopt);
            exit(EXIT_FAILURE);
            break;
        case 'd':
            debug = true;
            break;
        case 'h':
            /* _print_usage(); */
            printf("help msg ...\n");
            exit(EXIT_SUCCESS);
            break;
        case 't':
            trace = true;
            break;
        case 'v':
            verbose = true;
        default:
            ;
        }
    }
    /* config in this order: bazel then s7 */
    bazel_configure(); // getcwd(NULL, 0));
    s7_configure();

    /* test */
    /* char *canonical_path(char *path); */
    /* printf("np: %s\n", canonical_path("dune/stanzas/rule/deps/glob/../foo.ml")); */
    /* printf("np: a/b/../c       %s\n", canonical_path("a/b/../c")); */
    /* printf("np: a/b/../../c:   %s\n", canonical_path("a/b/../../c")); */
    /* printf("np: a/b/c/../../d: %s\n", canonical_path("a/b/c/../../d")); */

    /* printf("np: a/b/c:         %s\n", canonical_path("a/b/c")); */
    /* printf("np: .a/b/c:        %s\n", canonical_path("./a/b/c")); */
    /* printf("np: ./././a/b/c:   %s\n", canonical_path("./././a/b/c")); */
    /* printf("np: .///a/b/c:     %s\n", canonical_path(".///a/b/c")); */
    /* printf("np: ././/a/./b/././//c: %s\n", canonical_path("././/a/./b/././//c")); */
    /* printf("np: %s\n", canonical_path(".//a/b/c")); /\* busted *\/ */
    /* printf("np: a/b.c/e.f:     %s\n", canonical_path("a/b.c/e.f")); */
    /* printf("np: a/.b/e.f:      %s\n", canonical_path("a/.b/e.f")); */
    /* printf("np: ./a/b/c:       %s\n", canonical_path("./a/b/c")); */
    /* printf("np: ../a/b/c:      %s\n", canonical_path("../a/b/c")); */
    /* printf("np: a////b/./c//d: %s\n", */
    /*        canonical_path("a////b/./c//d")); */
    /* printf("np: a/./c:         %s\n", canonical_path("a/./c")); */
    /* printf("np: a//c:          %s\n", canonical_path("a//c")); */
    /* printf("np: a/////b:       %s\n", canonical_path("a/////b")); */
    /* printf("np: a/././././b:   %s\n", canonical_path("a/././././b")); */

    //TODO: cli args for root and path
    char *rootdir;
    char *pathdir;

    /* rootdir = "obazl/mibl/test"; */
    /* /\* pathdir = "test/files"; *\/ */
    /* pathdir = "test/dune/stanzas/rule/deps/glob"; */

    /* pathdir = "ws/a/b"; */

    /* launching from test/ws */
    /* pathdir = "a/b"; // ws/a/WORKSPACE.bazel */

    /* pathdir = "test/baddot"; */
    /* pathdir = "test/includes/mwe"; */
    /* pathdir = "test/filetypes"; */
    /* pathdir = "test/mwe"; */

    /* pathdir = "test/tezos/lib_clic"; */
    /* pathdir = "test/tezos/lib_requester"; */
    /* pathdir = "test/tezos/lib_stdlib_unix"; */
    /* pathdir = "test/tezos/lib_test"; */
    /* pathdir = "test/tezos/lib_workers"; */
    /* pathdir = "test/tezos/openapi"; */
    /* pathdir = "test/tezos/proto_000_Ps9mPmXa"; */

    /* pathdir = "test/mina"; */
    /* pathdir = "test/mina/ocaml-sodium"; */

    /* **************** */
    /* rootdir = "tweag/tezos"; */
    /* pathdir = "src/bin_node"; */
    /* pathdir = "src/lib_clic"; */
    /* pathdir = "src/lib_stdlib_unix"; */
    /* pathdir = "src/proto_alpha"; */
    /* pathdir = "src/lib_protocol_environment"; */

    /* **************** */
    /* rootdir = "minadev/gitfork"; */
    /* pathdir = "src"; */
    /* pathdir = "src/lib/snarky/src"; */

    rootdir = "obazl/mibl/test";
    /* pathdir = "test/dune/stanzas/rule/deps/glob"; */
    /* pathdir = "test/dune/stanzas/rule/action/run/cp"; */
    pathdir = "test/dune/stanzas/library/deps/select";

    s7_pointer pkg_tbl = load_dune(rootdir, pathdir);
    printf("cwd: %s\n", getcwd(NULL, 0));

    printf(BGRN "pkg_tbl:" CRESET "\n%s\n", s7_object_to_c_string(s7, pkg_tbl));

    /* (pkg (hash-table-ref pkgs arg)) */
    s7_pointer ht_ref = s7_name_to_value(s7, "hash-table-ref");
    if (ht_ref == s7_undefined(s7)) {
        printf("unbound symbol: hash-table-ref");
        exit(EXIT_FAILURE);
    }
    s7_pointer pkg_key = s7_make_string(s7, "dune/stanzas/library/deps/select");
    s7_pointer pkg = s7_call(s7, ht_ref, s7_list(s7, 2, pkg_tbl, pkg_key));
    printf(BGRN "pkg:" CRESET " %s\n", TO_STR(pkg));

    /* (stanzas (assoc :dune-stanzas pkg)) */
    s7_pointer assoc = s7_name_to_value(s7, "assoc");
    if (assoc == s7_undefined(s7)) {
        printf("unbound symbol: assoc");
        exit(EXIT_FAILURE);
    }
    s7_pointer dune_stanzas_kw = s7_make_keyword(s7, "dune-stanzas");
    s7_pointer stanzas = s7_call(s7, assoc,
                                 s7_list(s7, 2,
                                         dune_stanzas_kw, pkg));
    printf(BGRN "stanzas:" CRESET " %s\n", TO_STR(stanzas));

    /* (nzs (dune-pkg->mibl pkg)) */
    s7_pointer nds = s7_name_to_value(s7, "dune-pkg->mibl");
    if (nds == s7_undefined(s7)) {
        printf("unbound symbol: dune-pkg->mibl");
        exit(EXIT_FAILURE);
    }
    s7_pointer normalized = s7_call(s7, nds,
                                    s7_list(s7, 1, pkg));
    printf(BGRN "normalized stanza:" CRESET " %s\n", TO_STR(normalized));

    /* printf("*load-path*: %s\n", */
    /*        s7_object_to_c_string(s7, */
    /*                              s7_load_path(s7) */
    /*                              )); */

    if (verbose) {
        printf("ews: %s\n", ews_root);
        printf("dir count: %d\n", dir_ct);
        printf("file count: %d\n", file_ct);
        printf("dunefile count: %d\n", dunefile_ct);
    }
}
