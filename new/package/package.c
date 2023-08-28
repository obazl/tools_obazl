#include <unistd.h>

/* #include "ini.h" */
#include "log.h"
#include "utarray.h"

#include "mibl.h"
#include "package.h"

extern bool debug;
extern bool trace;
extern bool verbose;

extern char *ews_root;
extern int dir_ct;
extern int file_ct;
extern int dunefile_ct;

int main(int argc, char *argv[])
{
    char *opts = "dtvx";
    int opt, len;
    char *rootpath = NULL;
    char *pkgarg = NULL;

    bool exit_on_error = false;

    /* non-opt args: pkg paths. none means all */

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

    char *s;
    UT_array *paths;
    utarray_new(paths, &ut_str_icd);
    while (optind < argc) {
        log_info("arg: %s", argv[optind]);
        utarray_push_back(paths, &argv[optind]);
        optind++;
    }
    char **p;
    p = NULL;
    while ( (p=(char**)utarray_next(paths,p))) {
        printf("path: %s\n",*p);
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

    s7_load(s7, "bazel.scm");

    s7_load(s7, "new.scm");

    s7_pointer _main = s7_name_to_value(s7, "new-pkgs");

    if (_main == s7_undefined(s7)) {
        log_error(RED "Could not find procedure 'new-pkgs'; exiting\n");
        exit(EXIT_FAILURE);
    }

    s7_pointer _s7_pkgs = s7_nil(s7);
    p = NULL;
    while ( (p=(char**)utarray_next(paths,p))) {
        log_info("adding path: %s",*p);
        s7_pointer s = s7_make_string(s7, *p);
        log_debug("S: %s", TO_STR(s));
        _s7_pkgs = s7_cons(s7, s, _s7_pkgs);
        log_debug("PKGS: %s", TO_STR(_s7_pkgs));
    }
    _s7_pkgs = s7_reverse(s7, _s7_pkgs);
    log_debug("PKGS: %s", TO_STR(_s7_pkgs));

    s7_define_variable(s7, "*new-pkg-paths*", _s7_pkgs);

    /* s7_gc_on(s7, s7_f(s7)); */

    /* s7_int main_gc_loc = s7_gc_protect_via_stack(s7, _main); */
    s7_pointer result = s7_apply_function(s7, _main, _s7_pkgs);
    /* log_info("RESULT: %s\n", TO_STR(result)); */
    /* s7_gc_unprotect_via_stack(s7, _main); */

    /* char *errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */
    /* if ((errmsg) && (*errmsg)) { */
    /*     log_error("[%s\n]", errmsg); */
    /*     s7_quit(s7); */
    /*     exit(EXIT_FAILURE); */
    /* } */

    s7_quit(s7);

    log_info("new package...");
    return 0;
}
