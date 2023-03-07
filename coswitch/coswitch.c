//FIXME: support -j (--jsoo-enable) flag

#include <errno.h>
#include <fcntl.h>
#include <libgen.h>

#if INTERFACE
#ifdef __linux__
#include <linux/limits.h>
#else
#include <limits.h>
#endif
#endif

#include <unistd.h>

#include "gopt.h"
#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "s7.h"

#include "libmibl.h"
#include "coswitch.h"


#if defined(DEBUG_TRACE)
extern bool debug;
extern bool debug_findlib;
extern bool trace;
#endif

extern bool verbose;
extern bool enable_jsoo;

s7_scheme *s7;                  /* GLOBAL s7 */

char *pkg_path = NULL;

UT_array *opam_include_pkgs;
UT_array *opam_exclude_pkgs;

enum OPTS {
    OPT_PKG = 0,
    FLAG_JSOO,
    FLAG_CLEAN,
    FLAG_DEBUG,
    FLAG_TRACE,
    FLAG_VERBOSE,
    FLAG_QUIET,
    FLAG_HELP,
    LAST
};

void _print_usage(void) {
    printf("Usage:\t$ bazel run @obazl//convert [flags, options]\n");

    printf("Flags\n");
    printf("\t-j, --jsoo\t\t\tImport Js_of_ocaml resources.\n");
    printf("\t-c, --clean\t\t\tClean coswitch and reset to uninitialized state.\n");

    printf("\t-d, --debug\t\t\tEnable all debugging flags.\n");
    /* printf("\t-t, --trace\t\t\tEnable all trace flags.\n"); */
    printf("\t-v, --verbose\t\t\tEnable verbosity. Repeatable.\n");
}

/* we need s7 to read dune-package files */
void _mibl_s7_init(void) {
    s7 = s7_init();

    /* trap error messages */
    /* close_error_config(); */
    error_config_opam();
    init_error_handlers_opam();

    /* tmp dir */
    char tplt[] = "/tmp/obazl.XXXXXXXXXX";
    char *tmpdir = mkdtemp(tplt);
    /* printf("tmpdir: %s\n", tmpdir); */
    s7_define_variable(s7, "*tmp-dir*", s7_make_string(s7, tmpdir));
}

int main(int argc, char *argv[])
{
    char *opam_switch = NULL;

    utarray_new(opam_include_pkgs,&ut_str_icd);
    utarray_new(opam_exclude_pkgs,&ut_str_icd);

    static struct option options[] = {
        /* 0 */
        [OPT_PKG] = {.long_name="pkg",.short_name='p',
                     .flags=GOPT_ARGUMENT_REQUIRED
        },
        [FLAG_JSOO] = {.long_name="jsoo", .short_name='j',
                      .flags=GOPT_ARGUMENT_REQUIRED},
        [FLAG_CLEAN] = {.long_name="clean",.short_name='c',
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
        [FLAG_DEBUG] = {.long_name="debug",.short_name='d',
                       .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
        [FLAG_TRACE] = {.long_name="trace",.short_name='t',
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
        [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v',
                         .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
        [FLAG_QUIET] = {.long_name="quiet",.short_name='q',
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
        [FLAG_HELP] = {.long_name="help",.short_name='h',
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
        [LAST] = {.flags = GOPT_LAST}
    };

    argc = gopt (argv, options);
    gopt_errors (argv[0], options);

    if (options[FLAG_HELP].count) {
        _print_usage();
        exit(EXIT_SUCCESS);
    }

    if (options[FLAG_VERBOSE].count) {
        /* printf("verbose ct: %d\n", options[FLAG_VERBOSE].count); */
        verbose = true;
        verbosity = options[FLAG_VERBOSE].count;
    }

    if (options[FLAG_DEBUG].count) {
#if defined(DEBUG_TRACE)
        debug = true;
        debug_level = options[FLAG_DEBUG].count;
#endif
    }

    if (options[FLAG_TRACE].count) {
#if defined(DEBUG_TRACE)
        trace = true;
#endif
    }

    if (options[FLAG_JSOO].count) {
        enable_jsoo = true;
    }

    if (options[OPT_PKG].count) {
        utarray_push_back(opam_include_pkgs, &optarg);
    }

        /* case 'x': */
        /*     printf("EXCL %s\n", optarg); */
        /*     utarray_push_back(opam_exclude_pkgs, &optarg); */
        /*     break; */

    bazel_configure();

    chdir(rootws);            /* always run from base ws root */

    // opam_configure must be run from root ws to account for local switches
    // sets global opam_switch_* vars
    if (opam_switch)
        opam_configure(opam_switch);
    else
        opam_configure("");

    mibl_configure();

    _mibl_s7_init();

    /* char *wd = getenv("BUILD_WORKING_DIRECTORY"); */
    /* if (wd) { */
    /*     /\* we launched from bazel workspace, cd to launch dir *\/ */
    /*     chdir(wd); */
    /* } */

    /* walk_tree(opam_lib, pkg_path); */

    if (options[FLAG_CLEAN].count) {
        clean_coswitch();
        if (options[FLAG_QUIET].count < 1)
            printf(GRN "INFO: " CRESET
                   "Cleaned and reset coswitch."
                   " To reinitialize run 'bazel run @obazl//coswitch'\n");
    } else {
        convert_findlib_pkgs(opam_include_pkgs, opam_exclude_pkgs);
    }

    /* UT_array *result = opam_lex_file(utstring_body(opam_file)); */

    /* UT_array *result = sealark_lex_string("'hello'\n#cmt1\n"); */

    /* FIXME: free opam_include_pkgs, opam_exclude_pkgs */

#if defined(DEBUG_TRACE)
    log_debug("exiting coswitch");
#endif
    /* dump_nodes(result); */
}
