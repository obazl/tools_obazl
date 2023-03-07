#include <unistd.h>

#include "ini.h"
#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "s7.h"
#include "libmibl.h"

#include "batch.h"

#if defined(DEBUG_TRACE)
extern bool debug;
extern bool trace;
#endif
extern bool verbose;

extern char *ews_root;
extern int dir_ct;
extern int file_ct;
extern int dunefile_ct;

s7_pointer _load_load_dune(s7_scheme *s7)
{
    s7_pointer _load_dune;
    _load_dune = s7_name_to_value(s7, "mibl-load-project");
    if (_load_dune == s7_undefined(s7)) {
        log_error("unbound symbol: mibl-load-project");
        log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));
        s7_error(s7, s7_make_symbol(s7, "unbound-symbol"),
                 s7_list(s7, 1, s7_make_string(s7, "mibl-load-project")));
    }
    return _load_dune;
}

int main(int argc, char *argv[])
{
    char *opts = "p:hdtvx";
    int opt;
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
#if defined(DEBUG_TRACE)
            debug = true;
#endif
            break;
        case 'h':
            /* _print_usage(); */
            printf("help msg ...\n");
            exit(EXIT_SUCCESS);
            break;
        case 'p':
            pkgarg = strdup(optarg);
            /* remove trailing '/' */
            int len = strlen(pkgarg);
            if (pkgarg[len-1] == '/') {
                pkgarg[len-1] = '\0';
            }
            log_debug("package: %s\n", pkgarg);
            break;
        case 't':
#if defined(DEBUG_TRACE)
            trace = true;
#endif
            break;
        case 'v':
            verbose = true;
        case 'x':
            exit_on_error = true;
        default:
            ;
        }
    }
    if ((argc - optind) > 1) {
        log_error("Too many options");
        exit(EXIT_FAILURE);
    } else {
        if ((argc-optind) == 1) {
            pkgarg = argv[optind];
            log_info("PATH: %s", pkgarg);
        }
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

    s7_load(s7, "starlark.scm");

    /* FIXME: accept driver scm file and main routine name as params */
    /* (like clojure compiler?) */
    s7_load(s7, "mibl.scm");

    s7_pointer _main = s7_name_to_value(s7, "-main");

    if (_main == s7_undefined(s7)) {
        log_error(RED "Could not find procedure '-main'; exiting\n");
        exit(EXIT_FAILURE);
    }

    s7_pointer _s7_pkgarg;
    if (pkgarg) {
        _s7_pkgarg = s7_make_string(s7, pkgarg);
    } else {
        _s7_pkgarg = s7_nil(s7);
    }
    /* s7_pointer arg; */
    /* if (pkgarg) */
    /*     arg = s7_list(s7, 1, s7_make_string(s7, pkgarg)); */
    /* else */
    /*     arg = s7_nil(s7); */

    s7_pointer _s7_args;
    /* if (rootpath) { */
    /*     _s7_args = s7_list(s7, 2, */
    /*                        s7_make_string(s7, rootpath), */
    /*                        _s7_pkgarg); */
    /* } else { */
    _s7_args = s7_list(s7, 2,
                       s7_nil(s7),
                       _s7_pkgarg);
    /* } */

    /* s7_pointer result = s7_call(s7, _main, arg); */
    s7_pointer result = s7_apply_function(s7, _main, _s7_args);

    char *errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
    if ((errmsg) && (*errmsg)) {
        log_error("[%s\n]", errmsg);
        s7_quit(s7);
        exit(EXIT_FAILURE);
    }
    log_info("batch exit...");
    return 0;
}
