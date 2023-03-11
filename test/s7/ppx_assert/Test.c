#include <libgen.h>
#include <stdlib.h>             /* putenv */
#include <unistd.h>             /* getcwd */
#include <sys/errno.h>

#include "gopt.h"
#include "log.h"
#include "utarray.h"
#include "utstring.h"

#include "s7.h"
#include "libmibl.h"

#include "unity.h"


#if defined(DEBUG_TRACE)
extern bool mibl_debug;
extern int  mibl_debug_level;
extern bool mibl_debug_bazel;
extern bool mibl_debug_mibl;
extern bool mibl_debug_miblrc;
extern bool mibl_debug_mibl_crawl;
extern bool mibl_debug_scm;
extern bool mibl_debug_s7_config;

extern bool mibl_trace_bazel;
extern bool mibl_trace;
extern bool mibl_trace_mibl;
#endif

extern bool verbose;

s7_scheme *s7;

s7_pointer mibl_project;

static const char *_mpp(s7_scheme *sc, s7_pointer obj) /* (pp obj) */
{
  return(s7_string(
          s7_eval_c_string_with_environment(sc,
            "(catch #t                         \
               (lambda ()                      \
                 (unless (defined? 'mibl-pp)        \
                   (load \"mibl_pp.scm\"))       \
                 (mibl-pp obj))                     \
               (lambda (type info)             \
                 (apply format #f info)))",
	   s7_inlet(sc, s7_list(sc, 1, s7_cons(sc, s7_make_symbol(sc, "obj"), obj))))));
}

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

UT_string *setter;

void x_pp_mibl_project(void)
{// the hard way to do it:
    s7_pointer mpp = s7_name_to_value(s7, "mibl-pretty-print");
    if (mpp == s7_undefined(s7)) {
        log_error("unbound symbol: mibl-pretty-print");
        /* log_info("*load-path*: %s", TO_STR(s7_load_path(s7))); */
    }
    s7_pointer mproj = s7_name_to_value(s7, "*mibl-project*");
    if (mproj == s7_undefined(s7)) {
        log_error("unbound symbol: *mibl-project*");
        /* log_info("*load-path*: %s", TO_STR(s7_load_path(s7))); */
    }

    log_debug("printing");
    printf("before\n");
    s7_pointer tbl_str = s7_apply_function(s7, mpp,
                                           s7_list(s7, 1, mproj)
                                           );
    s7_newline(s7, s7_current_output_port(s7));
    printf("after\n");
    s7_flush_output_port(s7, s7_current_output_port(s7));

    /* printf("%s\n", TO_STR(tbl_str)); */
    /* printf("%s\n", _mpp(s7, pkg_tbl)); */
    fflush(stdout);
}

void _pp_mibl_project(void)
{// the easy way:

    s7_pointer res = s7_eval_c_string(s7,
                                      "(mibl-pretty-print *mibl-project*)");
    s7_newline(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_output_port(s7));

    /* printf("%s\n", TO_STR(tbl_str)); */
    /* printf("%s\n", _mpp(s7, pkg_tbl)); */
    fflush(stdout);
}

void _pp_mibl_expected(void)
{
    s7_pointer res = s7_eval_c_string(s7,
                                      "(mibl-pretty-print *mibl-expected*)");
    s7_newline(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_output_port(s7));
    fflush(stdout);
}

void test_a(void) {
    log_info("test_a");

    /* _pp_mibl_project(); */
    /* log_debug("after initial"); */

    char *sexp =
        "(begin "
        "(miblize :@) "
        "(add-filegroups-to-pkgs :@) "
        "(normalize-manifests! :@) "
        "(normalize-rule-deps! :@) "
        /* "(flush-output-port) " */
        "(miblarkize :@) "
        "(resolve-pkg-file-deps :@) "
        "(resolve-labels! :@) "
        "(handle-shared-ppx :@) "
        "(if *mibl-shared-deps*"
        "  (begin "
        "    (handle-shared-deps :@) "
        "    (handle-shared-opts :@))) "
        /* "(ws->starlark :@) " */
        "(flush-output-port) "
        ")"
        ;

    s7_flush_output_port(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_error_port(s7));

    s7_pointer res = s7_eval_c_string(s7, sexp);

    s7_flush_output_port(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_error_port(s7));

    /* log_debug("actual:"); */
    /* _pp_mibl_project(); */
    /* s7_flush_output_port(s7, s7_current_output_port(s7)); */

    /* log_debug("expected:"); */
    /* _pp_mibl_expected(); */

    /* /\* log_debug("expected:"); *\/ */
    /* /\* res = s7_eval_c_string(s7, "(format #t \"~A\" *mibl-expected*)"); *\/ */
    /* /\* s7_newline(s7, s7_current_output_port(s7)); *\/ */
    /* s7_flush_output_port(s7, s7_current_output_port(s7)); */

    /* log_debug("actual:"); */
    /* res = s7_eval_c_string(s7, "(format #t \"~A\" *mibl-project*)"); */
    /* s7_newline(s7, s7_current_output_port(s7)); */
    /* s7_flush_output_port(s7, s7_current_output_port(s7)); */


    res = s7_eval_c_string(s7, "(equal? *mibl-expected* *mibl-project*)");
    char *s = TO_STR(res);
    log_info("Equal? %d", s7_boolean(s7,res));
    free(s);

    if (res == s7_f(s7)) {

        log_debug(GRN "EXPECTED:" CRESET);
        res = s7_eval_c_string(s7, "(mibl-pretty-print *mibl-expected*)");
        /* s7_format(s7, s7_list(s7, 3, */
        /*                       s7_current_output_port(s7), //port, */
        /*                       s7_make_string(s7, "~A)"), */
        /*                       s7_name_to_value(s7, "*mibl-expected*"))); */
        s7_newline(s7, s7_current_output_port(s7));
        s7_flush_output_port(s7, s7_current_output_port(s7));

        log_debug(GRN "ACTUAL:" CRESET);
        res = s7_eval_c_string(s7, "(mibl-pretty-print *mibl-project*)");
        /* s7_format(s7, s7_list(s7, 3, */
        /*                       s7_current_output_port(s7), //port, */
        /*                       s7_make_string(s7, "~A)"), */
        /*                       s7_name_to_value(s7, "*mibl-project*"))); */
        s7_newline(s7, s7_current_output_port(s7));
        s7_flush_output_port(s7, s7_current_output_port(s7));
    }

    TEST_ASSERT(res == s7_t(s7));
}

void test_b(void) {
    log_info("test_b");

    /* s7_pointer env = s7_inlet(s7, s7_list(s7, 1, */
    /*                                       s7_cons(s7, */
    /*                                               s7_make_symbol(s7, "ws-mibl"), mibl_project))); */

    char *sexp = "(mibl-pretty-print *mibl-project*)";
        /* "(let* ((ws-tbl (eval (read (open-input-string ws-mibl))))) " */
        /* "  (mibl-pretty-print *mibl-project*))" */

        /* /\* "  (format #t \"~A~%\" (hash-table-keys pkgs-ht)))" *\/ */
        /* /\* "  (format #t \"~A~%\" (type-of (car (hash-table-keys pkgs-ht)))))" *\/ */
        /* /\* "  (format #t \"~A~%\" (hash-table? pkgs-ht)))" *\/ */
        /* /\* "  (format #t \"~A~%\" (hash-table-ref pkgs-ht 'case010)))" *\/ */
        /* ; */
    s7_eval_c_string(s7, sexp);

    /* s7_eval_c_string_with_environment(s7, sexp, env); */
    s7_newline(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_output_port(s7));

    TEST_ASSERT_TRUE(true);


}

void _print_usage(void) {
    printf("Usage:\t$ bazel test <tgt> [flags, options]\n");
    printf("Flags\n");
    printf("\t-d, --debug\t\tEnable all debugging flags.\n");
    printf("\t--debug-config\t\tEnable all config debugging flags.\n");
    printf("\t--debug-scm\t\tEnable all scheme debugging flags.\n");
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
    FLAG_DEBUG_CONFIG,
    FLAG_DEBUG_MIBLRC,
    FLAG_DEBUG_MIBL_CRAWL,
    FLAG_DEBUG_SCM,
    FLAG_DEBUG_SCM_LOADS,

    FLAG_EMIT_PARSETREE,        /* config load_project to emit PARSETREE.mibl */

    FLAG_TRACE,
    FLAG_VERBOSE,

    LAST
};

static struct option options[] = {
    /* 0 */
    [OPT_ROOT] = {.long_name="root",.short_name='r',
                  .flags=GOPT_ARGUMENT_REQUIRED},
    [OPT_PKG] = {.long_name="pkg",.short_name='p',
                 .flags=GOPT_ARGUMENT_REQUIRED
    },
    [FLAG_DEBUG] = {.long_name="debug",.short_name='d',
                    .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_DEBUG_CONFIG] = {.long_name="debug-config",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_MIBLRC] = {.long_name="debug-miblrc",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_MIBL_CRAWL] = {.long_name="debug-mibl-crawl",
                               .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_SCM] = {.long_name="debug-scm", .short_name = 'D',
                        .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_SCM_LOADS] = {.long_name="debug-scm-loads",
                              .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_PARSETREE] = {.long_name="emit-parsetree",
                              .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_TRACE] = {.long_name="trace",.short_name='t',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v',
                      .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_HELP] = {.long_name="help",.short_name='h',
                   .flags=GOPT_ARGUMENT_FORBIDDEN},
    [LAST] = {.flags = GOPT_LAST}
};

extern bool emit_parsetree;

void x_init_mibl_proj(char *test_root)
{
    // the hard way to do it:

    s7_int i, gc_loc;

    UT_string *pkg_mibl_file;
    utstring_new(pkg_mibl_file);
    utstring_printf(pkg_mibl_file, "%s/WS.s7", test_root);

    // pkg_mibl_file = $TEST_SRCDIR/mibl/scm/test/library/multple/WS.mibl
    //  = $TEST_SRCDIR + pkgpart(TEST_TARGET) + /WS.mibl

    /* char *pkg_mibl_file = "scm/test/library/multiple/WS.mibl"; */
    char *s1;
    s7_pointer port = s7_open_input_file(s7,
                                         utstring_body(pkg_mibl_file), "r");
    s7_pointer ws_s7;
    if (!s7_is_input_port(s7, port)) {
        {log_error("%s is not an input port?\n", s1 = TO_STR(port)); free(s1);}
        exit(EXIT_FAILURE);
    } else {
        gc_loc = s7_gc_protect(s7, port);
        /* should be exactly one sexp in PKG.mibl */
        ws_s7 = s7_read(s7, port);
        /* otherwise: */
        /* while(true) { */
        /*     s7_pointer code; */
        /*     code = s7_read(sc, port); */
        /*     if (code == s7_eof_object(sc)) break; */
        /*     /\* do something with this sexp *\/ */
        /* } */
        s7_close_input_port(s7, port);
        s7_gc_unprotect_at(s7, gc_loc);
    }

    s7_pointer env = s7_inlet(s7, s7_list(s7, 1,
                                          s7_cons(s7,
                                                  s7_make_symbol(s7, "ws-s7"), ws_s7)));

    char *sexp =
        "(define *mibl-project* (eval (read (open-input-string ws-s7))))"
        ;
    /* char *sexp = */
    /*     "(let* ((ws-tbl (eval (read (open-input-string ws-mibl))))) " */
    /*     "  (mibl-pretty-print ws-tbl))" */
    /* s7_eval_c_string(s7, sexp); */

    // this will not define *mibl-project* globally because of with_environment
    s7_pointer res = s7_eval_c_string_with_environment(s7, sexp, env);
    char *s = TO_STR(res);
    log_debug("res: %s", s);
    free(s);
    s7_define_variable(s7, "*mibl-project*", res);

    char *sexp2 = "(mibl-pretty-print *mibl-project*)";
    s7_eval_c_string(s7, sexp2);

    s7_newline(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_output_port(s7));

    s7_newline(s7, s7_current_output_port(s7));
    s7_flush_output_port(s7, s7_current_output_port(s7));

    /* s7_define_variable(s7, "*mibl-project*", mibl_project); */
}

/* init_mibl_proj - read initial and expected data files */
void _init_mibl_proj(char *test_root)
{
    log_debug("_init_mibl_proj");
    UT_string *pgm;
    utstring_new(pgm);

    utstring_printf(pgm,
                    "(define *mibl-project* "
                    "(call-with-input-file \"%s/PARSETREE.s7\" "
                    "  (lambda (p) "
                    "    (let* ((x (read p)) "
                    "           (y (eval (read (open-input-string x))))) "
                    "        y))))",
                    test_root);

    s7_pointer res = s7_eval_c_string(s7, utstring_body(pgm));
    /* char *s = TO_STR(res); */
    /* log_debug("parsetree: %s", s); */
    /* free(s); */

    utstring_renew(pgm);
    utstring_printf(pgm,
                    "(define *mibl-expected* "
                    "(call-with-input-file \"%s/EXPECTED.s7\" "
                    "  (lambda (p) "
                    "    (let* ((x (read p)) "
                    "           (y (eval (read (open-input-string x))))) "
                    "        y))))",
                    test_root);

    res = s7_eval_c_string(s7, utstring_body(pgm));
    /* s = TO_STR(res); */
    /* log_debug("expected: %s", s); */
    /* free(s); */
}

int main(int argc, char **argv)
{
    argc = gopt (argv, options);
    (void)argc;
    gopt_errors (argv[0], options);

    if (options[FLAG_HELP].count) {
        _print_usage();
        exit(EXIT_SUCCESS);
    }

    if (options[FLAG_VERBOSE].count) {
        log_info("verbose ct: %d", options[FLAG_VERBOSE].count);
        verbose = true;
        verbosity = options[FLAG_VERBOSE].count;
    }

    if (options[FLAG_DEBUG].count) {
#if defined(DEBUG_TRACE)
        if (verbose)
            log_info("debug ct: %d", options[FLAG_DEBUG].count);
        mibl_debug = true;
        mibl_debug_level = options[FLAG_DEBUG].count;
#endif
    }

    if (options[FLAG_DEBUG_CONFIG].count) {
#if defined(DEBUG_TRACE)
        if (verbose)
           log_info("debug_config ct: %d", options[FLAG_DEBUG_CONFIG].count);
        mibl_debug_bazel = true;
        mibl_debug_mibl = true;
        mibl_debug_s7_config = true;
#else
        log_error("--debug-config only valid with -c dbg");
        exit(EXIT_FAILURE);
#endif
    }

    if (options[FLAG_DEBUG_MIBLRC].count) {
#if defined(DEBUG_TRACE)
        if (verbose) log_info("debug_miblrc ct: %d", options[FLAG_DEBUG_MIBLRC].count);
        mibl_debug_miblrc = true;
#endif
    }

    if (options[FLAG_DEBUG_MIBL_CRAWL].count) {
#if defined(DEBUG_TRACE)
        if (verbose) log_info("debug_mibl_crawl ct: %d", options[FLAG_DEBUG_MIBL_CRAWL].count);
        mibl_debug_mibl_crawl = true;
#else
        log_error(RED "ERROR: " CRESET
                  "--debug-mibl-crawl requires -c dbg");
        exit(EXIT_FAILURE);
#endif
    }

    if (options[FLAG_DEBUG_SCM].count) {
#if defined(DEBUG_TRACE)
        mibl_debug_scm = true;
#else
        log_warn("--debug-scm only takes effect for debug builds (-c dbg)");
#endif
    }
    if (options[FLAG_DEBUG_SCM_LOADS].count) {
        mibl_s7_set_flag("*mibl-debug-loads*", true);
    }

    if (options[FLAG_TRACE].count) {
#if defined(DEBUG_TRACE)
        if (verbose)
            log_info("trace ct: %d", options[FLAG_TRACE].count);
        mibl_trace = true;
        mibl_trace_bazel = true;
#endif
    }

    if (options[FLAG_EMIT_PARSETREE].count) {
        if (getenv("BAZEL_TEST")) {
            /* log_warn("BAZEL_TEST: %s", getenv("BAZEL_TEST")); */
            if ( !getenv("BUILD_WORKSPACE_DIRECTORY") ) {
                fprintf(stderr,
                        RED "ERROR: " CRESET
                        "--emit-parsetree not supported under bazel test. Try 'bazel run'.\n");
                exit(EXIT_FAILURE);
            }
        }
        emit_parsetree = true;
    }

    /* **************************************************************** */
    /* In test env:
       BAZEL_TEST 1
       TEST_TARGET - label of this target, relative to BUILD_WS_DIRECTORY
       BUILD_WORKSPACE_DIRECTORY - set
       BUILD_WORKING_DIRECTORY - set
       RUNFILES_DIR - set

       task: config s7 with runfiles, then chdir to
               BUILD_WORKSPACE_DIRECTORY/TEST_TARGET pkg

       alternative: put the project tree in runfiles. the drawback is
       that the paths would be relative to the launch dir, not the dir
       containing the test target. but we want that dir to be like a ws.

       alternative: argv[0] is the test executable in the test case
       dir. take its dirname. or use TEST_SRCDIR which is dirname(argv[0])

       But according the the Test Encyclopedia, we should not change
       directory, but use the runfiles (TEST_SRCDIR, TEST_WORKSPACE,
       etc.)

       https://bazel.build/reference/test-encyclopedia#initial-conditions:

       "The test runner must invoke each test with the path to the
       test executable in argv[0]. This path must be relative and
       beneath the test's current directory (which is in the runfiles
       tree, see below). The test runner should not pass any other
       arguments to a test unless the user explicitly requests it."

       "The initial working directory shall be
       $TEST_SRCDIR/$TEST_WORKSPACE."

       "File descriptors 1 (stdout) and 2 (stderr) shall be open for
       writing, but what they are attached to is unspecified. It could
       be a terminal, a pipe, a regular file, or anything else to
       which characters can be written."

       "Tests must not assume that any constant path is available for
       their exclusive use."

       "Tests should create files only within the directories
       specified by $TEST_TMPDIR and $TEST_UNDECLARED_OUTPUTS_DIR (if
       set). These directories will be initially empty. Tests must not
       attempt to remove, chmod, or otherwise alter these directories."

       "Tests must access inputs through the runfiles mechanism, or
       other parts of the execution environment which are specifically
       intended to make input files available."

       "Tests must not access other outputs of the build system at
       paths inferred from the location of their own executable."

       "Tests should avoid using paths containing .. components within
       the runfiles tree."

       "No directory, file, or symlink within the runfiles tree
       (including paths which traverse symlinks) should be writable.
       (It follows that the initial working directory should not be
       writable.) Tests must not assume that any part of the runfiles
       is writable, or owned by the current user (for example, chmod
       and chgrp may fail)."

       "The runfiles tree (including paths which traverse symlinks)
       must not change during test execution. Parent directories and
       filesystem mounts must not change in any way which affects the
       result of resolving a path within the runfiles tree."

     */

    /* we do not crawl the project, but we do use s7, so we need to
       configure. */
    struct mibl_config_s *mibl_config = mibl_s7_init(NULL, /* script dir */
                                                     NULL); /* ws */

    log_info("cwd after mibl_config: %s", getcwd(NULL, 0));
    /* show_bazel_config(); */
    /* show_mibl_config(); */
    /* show_s7_config(); */
    /* log_info("arg0: %s", argv[0]); */

    char *test_pgm = strdup(argv[0]); // free
    errno = 0;
    char *test_root = dirname(test_pgm);
    log_debug("test_root: %s", test_root);
    if (test_root == NULL) {
        perror(test_pgm);
    }

    _init_mibl_proj(test_root);

    UNITY_BEGIN();
    RUN_TEST(test_a);
    /* RUN_TEST(test_b); */
    return UNITY_END();
}
