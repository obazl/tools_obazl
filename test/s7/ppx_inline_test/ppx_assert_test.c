#include <stdlib.h>             /* putenv */
#include <unistd.h>             /* getcwd */

#include "gopt.h"
#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "s7.h"
#include "libmibl.h"

#include "unity.h"


#if defined(DEBUG_TRACE)
extern bool debug;
extern int  mibl_debug_level;
extern bool debug_miblrc;
extern bool trace;
#endif
extern bool verbose;

s7_scheme *s7;

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    log_info("setup");
}

void tearDown(void) {
    log_info("teardown");
}

UT_string *setter;
char *parsetree_expected =
    "(hash-table \"test/cases/dune/inline_tests/ppx_assert/test\" ((:ws-path \"/Users/gar/obazl/mibl\") (:pkg-path \"test/cases/dune/inline_tests/ppx_assert/test\") (:realpath \"/Users/gar/obazl/mibl/test/cases/dune/inline_tests/ppx_assert/test\") (dune (library (name ppx_assert_test_lib) (libraries sexplib str) (preprocess (pps ppx_compare ppx_sexp_conv ppx_here ppx_assert ppx_inline_test)))) (:structures (:static (Ppx_assert_test . ppx_assert_test.ml)))) \"test/cases/dune/inline_tests/ppx_assert\" ((:ws-path \"/Users/gar/obazl/mibl\") (:pkg-path \"test/cases/dune/inline_tests/ppx_assert\") (:realpath \"/Users/gar/obazl/mibl/test/cases/dune/inline_tests/ppx_assert\") (:cc-srcs (:static \"ppx_assert_test.c\"))))";

void test_ppx_assert_test(void) {
    log_info("test_ppx_assert_test");
    char *rootdir;
    char *pathdir;

    /* rootdir = "test/cases/dune"; */
    /* pathdir = "inline_tests/ppx_assert"; */

    rootdir = "test/cases/dune",
    pathdir = "test/cases/dune/inline_tests/ppx_assert";

    s7_pointer pkg_tbl = load_project(rootdir, pathdir);

    log_debug(BGRN "pkg_tbl:" CRESET "\n%s",
           s7_object_to_c_string(s7, pkg_tbl));

    s7_pointer pp = s7_name_to_value(s7, "mibl-pretty-print");
    s7_pointer tbl_str = s7_call(s7, pp, s7_list(s7, 1, pkg_tbl));
    printf("%s\n", TO_STR(tbl_str));
    fflush(stdout);

    utstring_new(setter);

    /* (pkg (hash-table-ref pkgs arg)) */
    /* s7_pointer ht_ref = s7_name_to_value(s7, "hash-table-ref"); */
    /* if (ht_ref == s7_undefined(s7)) { */
    /*     printf("unbound symbol: hash-table-ref"); */
    /*     exit(EXIT_FAILURE); */
    /* } */
    /* s7_pointer pkg_key = s7_make_string(s7, "dune/stanzas/library/deps/select"); */
    /* s7_pointer pkg = s7_call(s7, ht_ref, s7_list(s7, 2, pkg_tbl, pkg_key)); */
    /* printf(BGRN "pkg:" CRESET " %s\n", TO_STR(pkg)); */
    TEST_ASSERT_TRUE(true);
}

void test_ppx_assert_bench(void) {
    log_info("test_ppx_assert_bench");
    TEST_ASSERT_TRUE(true);
}

void _print_usage(void) {
    printf("Usage:\t$ bazel test <tgt> [flags, options]\n");
    printf("Flags\n");
    printf("\t-d, --debug\t\tEnable all debugging flags.\n");
    printf("\t-t, --trace\t\tEnable trace flags.\n");
    printf("\t-v, --verbose\t\tEnable verbosity. Repeatable.\n");

    printf("Options:\n");
    /* printf("\t-D | -log <arg>\t\tLog <arg> (parsetree, mibl, or starlark}) to stdout.\n"); */

    printf("\t-r, --root <arg>"
           "\tStart traversal at <arg> (path relative to cwd).\n");
    printf("\t-p, --pkg <arg>"
           "\t\tProcess only <arg> (relative root path).\n");
}

enum OPTS {
    OPT_ROOT = 0,
    OPT_PKG,
    OPT_PACKAGE,

    FLAG_HELP,
    FLAG_DEBUG,
    FLAG_DEBUG_MIBLRC,
    FLAG_TRACE,
    FLAG_VERBOSE,

    LAST
};

/* extern bool debug_bazel; */
/* extern bool trace_bazel; */

int main(int argc, char **argv)
{
    /* debug_bazel = true; */
    /* trace_bazel = true; */

    static struct option options[] = {
        /* 0 */
        [OPT_ROOT] = {.long_name="root",.short_name='r',
                      .flags=GOPT_ARGUMENT_REQUIRED},
        [OPT_PKG] = {.long_name="pkg",.short_name='p',
                     .flags=GOPT_ARGUMENT_REQUIRED
        },
        [FLAG_DEBUG] = {.long_name="debug",.short_name='d',
                       .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
        [FLAG_DEBUG_MIBLRC] = {.long_name="debug_miblrc",
                               .flags=GOPT_ARGUMENT_FORBIDDEN},
        [FLAG_TRACE] = {.long_name="trace",.short_name='t',
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
        [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v',
                         .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
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
        printf("verbose ct: %d\n", options[FLAG_VERBOSE].count);
        verbose = true;
        verbosity = options[FLAG_VERBOSE].count;
    }

    if (options[FLAG_DEBUG].count) {
#if defined(DEBUG_TRACE)
        debug = true;
        mibl_debug_level = options[FLAG_DEBUG].count;
#endif
    }

    if (options[FLAG_DEBUG_MIBLRC].count) {
#if defined(DEBUG_TRACE)
        debug_miblrc = true;
#endif
    }

    if (options[FLAG_TRACE].count) {
        /* printf("trace ct: %d\n", options[FLAG_TRACE].count); */
#if defined(DEBUG_TRACE)
        trace = true;
#endif
    }

    bazel_configure();
    log_info("bazel_configure done");

    /* mibl_configure(); */

    /* s7 = s7_configure(); */

    /* s7_load(s7, "dune.scm"); */

    /* initialize_mibl_data_model(s7); */

    UNITY_BEGIN();
    /* RUN_TEST(test_ppx_assert_test); */
    RUN_TEST(test_ppx_assert_bench);
    UNITY_END();

    /* s7_shutdown(s7); */
    return 0;
}
