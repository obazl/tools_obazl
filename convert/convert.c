#include <unistd.h>

#include "ini.h"
#include "log.h"
#include "utarray.h"

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
    char *opts = "p:r:hdtvx";
    int opt, len;
    char *rootpath = NULL;
    char *pkgarg = NULL;

    bool exit_on_error = false;

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
            printf("SETTING DEBUG\n");
            debug = true;
            break;
        case 'h':
            /* _print_usage(); */
            printf("help msg ...\n");
            exit(EXIT_SUCCESS);
            break;
        case 'p':
            pkgarg = strdup(optarg);
            /* remove trailing '/' */
            len = strlen(pkgarg);
            if (pkgarg[len-1] == '/') {
                pkgarg[len-1] = '\0';
            }
            /* validate - no abs paths, may start with '//" */
            break;
        case 'r':
            rootpath = strdup(optarg);
            /* remove trailing '/' */
            len = strlen(rootpath);
            if (rootpath[len-1] == '/') {
                rootpath[len-1] = '\0';
            }
            /* validate - relative path, not internal .. */
            break;
        case 't':
            trace = true;
            break;
        case 'v':
            verbose = true;
        case 'x':
            exit_on_error = true;
        default:
            ;
        }
    }
    log_debug("argc: %d", argc);
    log_debug("optind: %d", optind);

    if (argc != optind) {
        log_error("non-opt argument");
        /* log_error("next: %s", argv[optind]); */
        exit(EXIT_FAILURE);
    }

    /* if ((argc-optind) == 1) { */
    /*     rootpath = argv[optind]; */
    /*     log_info("ROOTPATH: %s", rootpath); */
    /* } */

    if (debug) {
        log_debug("rootpath: '%s'", rootpath);
        log_debug("pkgarg: '%s'", pkgarg);
    }

    /* config in this order: first bazel, then mibl, then s7 */
    bazel_configure(); // getcwd(NULL, 0));
    mibl_configure();
    s7_scheme *s7 = s7_configure();

    if (exit_on_error) {
        s7_define_variable(s7, "*exit-on-error*", s7_t(s7));
    } else {
        s7_define_variable(s7, "*exit-on-error*", s7_f(s7));
    }
    /* printf("*exit-on-error*? %d\n", */
    /*        (s7_t(s7) == s7_name_to_value(s7, "*exit-on-error*"))); */

    if (pkgarg) {
        UT_string *setter; utstring_new(setter);
        utstring_printf(setter, "(set! *emit-bazel-pkg* \"%s\")", pkgarg);
        /* log_debug("SETTING *emit-bazel-pkg* to: %s", pkgarg); */
        s7_eval_c_string(s7, utstring_body(setter));
    }

    s7_load(s7, "starlark.scm");

    s7_load(s7, "convert.scm");

    s7_pointer _main = s7_name_to_value(s7, "dune->obazl");

    if (_main == s7_undefined(s7)) {
        log_error(RED "Could not find procedure 'dune->obazl'; exiting\n");
        exit(EXIT_FAILURE);
    }

    s7_pointer _s7_pkgarg;
    if (pkgarg) {
        _s7_pkgarg = s7_make_string(s7, pkgarg);
    } else {
        _s7_pkgarg = s7_nil(s7);
    }

    s7_pointer _s7_args;

    if (rootpath) {
        _s7_args = s7_list(s7, 2,
                           s7_make_string(s7, rootpath),
                           _s7_pkgarg);
    } else {
        _s7_args = s7_list(s7, 2,
                      s7_nil(s7),
                      _s7_pkgarg);
    }

    if (debug) log_debug("s7 args: %s", TO_STR(_s7_args));

    s7_gc_on(s7, s7_f(s7));

    /* s7_int main_gc_loc = s7_gc_protect_via_stack(s7, _main); */
    s7_pointer result = s7_apply_function(s7, _main, _s7_args);
    /* log_info("RESULT: %s\n", TO_STR(result)); */
    /* s7_gc_unprotect_via_stack(s7, _main); */

    char *errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
    if ((errmsg) && (*errmsg)) {
        log_error("[%s\n]", errmsg);
        s7_quit(s7);
        exit(EXIT_FAILURE);
    }
    log_info("convert exit...");
    return 0;
}
