#include <unistd.h>

/* #include "bazel_config.h" */
/* #include "mibl_config.h" */
/* #include "s7_config.h" */
/* #include "ansi_colors.h" */
/* #include "load_dune.h" */

#include "ini.h"
#include "log.h"
#include "mibl.h"
#include "convert.h"

extern bool debug;
extern bool trace;
extern bool verbose;

extern char *ews_root;
extern int dir_ct;
extern int file_ct;
extern int dunefile_ct;

s7_pointer _load_load_dune(s7_scheme *s7)
{
    s7_pointer _load_dune;
    _load_dune = s7_name_to_value(s7, "load-dune");
    if (_load_dune == s7_undefined(s7)) {
        log_error("unbound symbol: load-dune");
        log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));
        s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                 s7_list(s7, 1, s7_make_string(s7, "load-dune")));
    }
    return _load_dune;
}

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
    s7_scheme *s7 = s7_configure();

    char *rootdir;
    char *pathdir;

    rootdir = getcwd(NULL, 0);
    pathdir = "./";

    s7_pointer _s7_load_dune = _load_load_dune(s7);
    printf("load-dune: %s\n", TO_STR(_s7_load_dune));

    s7_pointer wss = s7_eval_c_string(s7, "(load-dune)");
    printf("wss: %s\n", TO_STR(wss));

    /*
      1. get :@ ws
      2. get :pkgs from :@
      2. for-each pkg in :pkgs ...
     */

    /* printf(BGRN "pkg_tbl:" CRESET "\n%s\n", s7_object_to_c_string(s7, pkg_tbl)); */

    /* (pkg (hash-table-ref pkgs arg)) */
    /* s7_pointer ht_ref = s7_name_to_value(s7, "hash-table-ref"); */
    /* if (ht_ref == s7_undefined(s7)) { */
    /*     printf("unbound symbol: hash-table-ref"); */
    /*     exit(EXIT_FAILURE); */
    /* } */
    /* s7_pointer pkg_key = s7_make_string(s7, "dune/stanzas/library/deps/select"); */
    /* s7_pointer pkg = s7_call(s7, ht_ref, s7_list(s7, 2, pkg_tbl, pkg_key)); */
    /* printf(BGRN "pkg:" CRESET " %s\n", TO_STR(pkg)); */

    /* /\* (stanzas (assoc :dune-stanzas pkg)) *\/ */
    /* s7_pointer assoc = s7_name_to_value(s7, "assoc"); */
    /* if (assoc == s7_undefined(s7)) { */
    /*     printf("unbound symbol: assoc"); */
    /*     exit(EXIT_FAILURE); */
    /* } */
    /* s7_pointer dune_stanzas_kw = s7_make_keyword(s7, "dune-stanzas"); */
    /* s7_pointer stanzas = s7_call(s7, assoc, */
    /*                              s7_list(s7, 2, */
    /*                                      dune_stanzas_kw, pkg)); */
    /* printf(BGRN "stanzas:" CRESET " %s\n", TO_STR(stanzas)); */

    /* /\* (nzs (dune-pkg->mibl pkg)) *\/ */
    /* s7_pointer nds = s7_name_to_value(s7, "dune-pkg->mibl"); */
    /* if (nds == s7_undefined(s7)) { */
    /*     printf("unbound symbol: dune-pkg->mibl"); */
    /*     exit(EXIT_FAILURE); */
    /* } */
    /* s7_pointer normalized = s7_call(s7, nds, */
    /*                                 s7_list(s7, 1, pkg)); */
    /* printf(BGRN "normalized stanza:" CRESET " %s\n", TO_STR(normalized)); */

    /* /\* printf("*load-path*: %s\n", *\/ */
    /* /\*        s7_object_to_c_string(s7, *\/ */
    /* /\*                              s7_load_path(s7) *\/ */
    /* /\*                              )); *\/ */

    if (verbose) {
        printf("ews: %s\n", ews_root);
        printf("dir count: %d\n", dir_ct);
        printf("file count: %d\n", file_ct);
        printf("dunefile count: %d\n", dunefile_ct);
    }
}
