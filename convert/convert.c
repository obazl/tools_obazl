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

void _print_usage()
{
    printf("Usage: msg...\n");
}

int main(int argc, char *argv[])
{
    char *opts = "p:hdtvx";
    int opt;
    char *pkgarg = NULL;

    bool exit_on_error = false;

    while ((opt = getopt(argc, argv, opts)) != -1) {
        /* printf("optind: %d\n", optind); */
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
            _print_usage();
            exit(EXIT_SUCCESS);
            break;
        case 'p':
            printf("package: %s\n", optarg);
            pkgarg = strdup(optarg);
            break;
        case 't':
            trace = true;
            break;
        case 'v':
            verbose = true;
            break;
        case 'x':
            exit_on_error = true;
        default:
            ;
        }
    }

    if (argc > optind) {
        printf(RED "ERROR:" CRESET "extra argument passed\n");
        _print_usage();
        exit(EXIT_SUCCESS);
    }

    /* config in this order: first bazel, then mibl, then s7 */
    bazel_configure(); // getcwd(NULL, 0));
    mibl_configure();
    s7_scheme *s7 = s7_configure();

    if (exit_on_error) {
        printf("exit on error\n");
        s7_define_variable(s7, "*exit-on-error*", s7_t(s7));
    } else {
        printf("no exit on error\n");
        s7_define_variable(s7, "*exit-on-error*", s7_f(s7));
    }
    printf("*exit-on-error*? %d\n",
           (s7_t(s7) == s7_name_to_value(s7, "*exit-on-error*")));

    s7_load(s7, "starlark.scm");

    char *rootdir;
    char *pathdir;

    rootdir = getcwd(NULL, 0);
    pathdir = "./";

    s7_pointer _s7_load_dune = _load_load_dune(s7);
    printf("load-dune: %s\n", TO_STR(_s7_load_dune));
    s7_pointer _wss;
    if (pkgarg) {
        _wss = s7_call(s7, _s7_load_dune,
                                  s7_list(s7, 1, s7_make_string(s7, pkgarg)));
    } else {
        _wss = s7_call(s7, _s7_load_dune, s7_nil(s7));
                                  /* s7_list(s7, 1, s7_make_string(s7, pkgarg))); */
        /* s7_pointer _wss = s7_eval_c_string(s7, "(load-dune)"); */
    }
    /* printf("_wss: %s\n", TO_STR(_wss)); */

    /*
      1. get :@ ws
      2. get :pkgs from :@
      2. for-each pkg in :pkgs ...
     */

        /* close_error_config(); */
        /* init_error_handling(); */
        /* error_config(); */

    s7_pointer root_ws =
        s7_eval_c_string(s7, "(assoc-val :@ -mibl-ws-table)");
    /* printf("root_ws: %s\n", TO_STR(root_ws)); */

    s7_pointer pkgs =
        s7_eval_c_string_with_environment(s7,  "(car (assoc-val :pkgs @ws))",
                                          s7_inlet(s7, s7_list(s7, 1,
                                                               s7_cons(s7, s7_make_symbol(s7, "@ws"), root_ws))));
    /* printf("pkgs: %s\n", TO_STR(pkgs)); */

    char *sexp =
        "(map (lambda (kv) "
        "       (let ((mibl-pkg (dune-pkg->mibl :@ (cdr kv)))) "
        "         (hash-table-set! pkgs (car kv) mibl-pkg))) "
        "  pkgs)";

    s7_pointer npkgs =
        s7_eval_c_string_with_environment(s7, sexp,
                                          s7_inlet(s7, s7_list(s7, 1,
                                                               s7_cons(s7, s7_make_symbol(s7, "pkgs"), pkgs))));
    /* printf("npkgs: %s\n", TO_STR(npkgs)); */

    sexp = "(resolve-labels (assoc-val :@ -mibl-ws-table))";
    s7_eval_c_string(s7, sexp);

    sexp = "(car (assoc-val :exports (assoc-val :@ -mibl-ws-table)))";
    s7_pointer exports = s7_eval_c_string(s7, sexp);
    printf("exports: %s\n", TO_STR(exports));

    /* printf("*load-path*: %s\n", TO_STR(s7_load_path(s7))); */

    /* printf("npkgs: %s\n", TO_STR(npkgs)); */

    /* to starlark */
    sexp =
        "(for-each (lambda (kv)"
        "              (mibl-pkg->starlark (cdr kv)))"
        "       npkgs)";
    s7_eval_c_string_with_environment(s7, sexp,
                                      s7_inlet(s7, s7_list(s7, 1,
                                                           s7_cons(s7, s7_make_symbol(s7, "npkgs"), npkgs))));


    if (verbose) {
        printf("ews: %s\n", ews_root);
        printf("dir count: %d\n", dir_ct);
        printf("file count: %d\n", file_ct);
        printf("dunefile count: %d\n", dunefile_ct);
    }
}
