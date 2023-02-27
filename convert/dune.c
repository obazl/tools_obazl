#include <errno.h>
#include <unistd.h>

#include "gopt.h"
#include "ini.h"
/* #include "log.h" */
/* #include "utarray.h" */

#include "s7.h"
#include "libmibl.h"
#include "dune.h"

bool debug;
#if defined(DEBUG_TRACE)
extern bool trace;
#endif

extern bool verbose;
extern struct mibl_config_s mibl_config;

extern s7_scheme *s7;

extern char *ews_root;
extern int dir_ct;
extern int file_ct;
extern int dunefile_ct;

bool dump_parsetree = false;
bool dump_mibl      = false;
bool dump_starlark  = false;

bool emit_parsetree = false;
bool emit_mibl      = false;
bool emit_starlark  = true;

int main(int argc, char **argv)
{
    /* int opt; */
    int len;
    char *rootpath = NULL;
    char *pkgarg = NULL;

    bool exit_on_error = false;

    enum OPTS {
        OPT_ROOT = 0,
        OPT_PKG,
        OPT_PACKAGE,
        OPT_EMIT,
        OPT_DUMP,
        OPT_HELP,
        OPT_DEBUG,
        OPT_TRACE,
        OPT_VERBOSE,
        OPT_LAST
    };

    /* static struct option options[9]; */
    /* struct option options[OPT_ROOT] {.long_name="root",.short_name='r',.flags=GOPT_ARGUMENT_REQUIRED}; */
    /* struct option options[OPT_PKG] {.long_name="pkg",.short_name='p', */
    /*                     .flags=GOPT_ARGUMENT_REQUIRED | GOPT_REPEATABLE}; */

    static struct option options[] = {
        /* 0 */
        [OPT_ROOT] = {.long_name="root",.short_name='r',
                      .flags=GOPT_ARGUMENT_REQUIRED},
        [OPT_PKG] = {.long_name="pkg",.short_name='p',
                     .flags=GOPT_ARGUMENT_REQUIRED
                     /* | GOPT_REPEATABLE */
        },
        [OPT_PACKAGE] = {.long_name="package",
                         .flags=GOPT_ARGUMENT_REQUIRED
                     /* | GOPT_REPEATABLE */
        },

        /* emit options: parsetree, mibl, starlark */
        [OPT_EMIT] = {.long_name="emit",.short_name='e',
                      .flags=GOPT_ARGUMENT_REQUIRED},

        /* stdout options (stages): parsetree, mibl. starlark */
        /* i.e. we can emit starlark but print mibl to stdout */
        /* -e starlark -D mibl */
        /* only one -D allowed at a time */
        [OPT_DUMP] = {.long_name="dump",.short_name='D',
                      .flags=GOPT_ARGUMENT_REQUIRED},

        [OPT_HELP] = {.long_name="help",.short_name='h',
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_DEBUG] = {.long_name="debug",.short_name='d',
                       .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
        [OPT_TRACE] = {.long_name="trace",.short_name='t',
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_VERBOSE] = {.long_name="verbose",.short_name='v',
                         .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
        [OPT_LAST] = {.flags = GOPT_LAST}
    };

    argc = gopt (argv, options);
    gopt_errors (argv[0], options);

    if (options[OPT_VERBOSE].count) {
        /* printf("verbose ct: %d\n", options[OPT_VERBOSE].count); */
        verbose = true;
        verbosity = options[OPT_VERBOSE].count;
    }

    if (options[OPT_ROOT].count) {
        printf("root ct: %d\n", options[OPT_ROOT].count);
        rootpath = strdup(options[OPT_ROOT].argument);
        /* remove trailing '/' */
        len = strlen(rootpath);
        if (rootpath[len-1] == '/') {
            rootpath[len-1] = '\0';
        }
        printf("ROOT: %s\n", rootpath);
    }

    if (options[OPT_HELP].count) {
        /* printf("help ct: %d\n", options[OPT_HELP].count); */
        /* _print_usage(); */
        printf("help msg ...\n");
        exit(EXIT_SUCCESS);
    }

    if (options[OPT_DEBUG].count) {
        /* printf("debug ct: %d\n", options[OPT_DEBUG].count); */
#if defined(DEBUG_TRACE)
        printf("SETTING DEBUG\n");
        debug = true;
#endif
    }

    if (options[OPT_TRACE].count) {
        /* printf("trace ct: %d\n", options[OPT_TRACE].count); */
#if defined(DEBUG_TRACE)
        trace = true;
#endif
    }

    if (debug) {
        log_debug("argc: %d", argc);
        log_debug("optind: %d", optind);
        log_debug("argv[0]: %s", argv[0]);
    }

/*     if (debug) { */
    /* char *launch_cwd = getcwd(NULL, 0); */
/*         log_debug("launch cwd: %s", launch_cwd); */
/* #ifdef BAZEL_CURRENT_REPOSITORY */
/*     char *current_repo = getenv("BAZEL_CURRENT_REPOSITORY"); */
/*     if (current_repo) { */
/*         log_debug("BAZEL_CURRENT_REPOSITORY: %s", current_repo); */
/*         log_debug("BAZEL_CURRENT_REPOSITORYx: %s", BAZEL_CURRENT_REPOSITORY); */
/*     } */
/* #endif */
/*     } */

    if (argc != optind) {
        log_error("non-opt argument");
        /* log_error("next: %s", argv[optind]); */
        exit(EXIT_FAILURE);
    }

    /* if ((argc-optind) == 1) { */
    /*     rootpath = argv[optind]; */
    /*     log_info("ROOTPATH: %s", rootpath); */
    /* } */

#if defined(DEBUG_TRACE)
    if (debug) {
        log_debug("rootpath: '%s'", rootpath);
        log_debug("pkgarg: '%s'", pkgarg);
    }
#endif

    /* config in this order: first bazel, then mibl, then s7 */
    bazel_configure(); // getcwd(NULL, 0));

    mibl_configure();

    char **p = NULL;
    /* while ( (p=(char**)utarray_next(mibl_config.pkgs, p))) { */
    /*     log_info("miblrc pkg: %s", *p); */
    /* } */

    /* cmd line --pkg args augment miblrc */
    if (options[OPT_PKG].count || options[OPT_PACKAGE].count) {
        char *token, *sep = " ,\t";
        printf("pkg ct: %d\n",
               options[OPT_PKG].count + options[OPT_PACKAGE].count);
        if (options[OPT_PKG].count) {
            token = strtok((char*)options[OPT_PKG].argument, sep);
            while( token != NULL ) {
                if (token[0] == '/') {
                    log_error("-pkg values must be relative paths: %s", token);
                    return 0;
                } else {
                    log_debug("miblrc pushing pkg: %s", token);
                    utarray_push_back(mibl_config.pkgs, &token);
                    token = strtok(NULL, sep);
                }
            }
            /* pkgarg = strdup(options[OPT_PKG].argument); */
            /* /\* remove trailing '/' *\/ */
            /* len = strlen(pkgarg); */
            /* if (pkgarg[len-1] == '/') { */
            /*     pkgarg[len-1] = '\0'; */
            /* } */
            /* printf("PKG: %s\n", pkgarg); */
            /* /\* validate - no abs paths, may start with '//" *\/ */
        }

        if (options[OPT_PACKAGE].count) {
            token = strtok((char*)options[OPT_PACKAGE].argument, sep);
            while( token != NULL ) {
                if (token[0] == '/') {
                    log_error("-pkg values must be relative paths: %s", token);
                    return 0;
                } else {
                    log_debug("miblrc pushing pkg: %s", token);
                    utarray_push_back(mibl_config.pkgs, &token);
                    token = strtok(NULL, sep);
                }
            }

            /* pkgarg = strdup(options[OPT_PACKAGE].argument); */
            /* /\* remove trailing '/' *\/ */
            /* len = strlen(pkgarg); */
            /* if (pkgarg[len-1] == '/') { */
            /*     pkgarg[len-1] = '\0'; */
            /* } */
            /* printf("PACKAGE: %s\n", pkgarg); */
            /* /\* validate - no abs paths, may start with '//" *\/ */
        }
    }

    /* cmd line options for --dump and --emit override miblrc settings */
    if (options[OPT_DUMP].count) {
        /* printf("dump ct: %d\n", options[OPT_DUMP].count); */
        if (strncmp(options[OPT_DUMP].argument, "starlark", 8) == 0) {
            /* printf("dumping starlark\n"); */
            mibl_config.dump_starlark  = true;
            mibl_config.dump_mibl      = false;
            mibl_config.dump_parsetree = false;
        }
        else if (strncmp(options[OPT_DUMP].argument, "mibl", 4) == 0) {
            /* printf("dumping mibl\n"); */
            mibl_config.dump_starlark  = false;
            mibl_config.dump_mibl      = true;
            mibl_config.dump_parsetree = false;
        }
        else if (strncmp(options[OPT_DUMP].argument, "parsetree", 9) == 0) {
            /* printf("dumping parsetree\n"); */
            mibl_config.dump_starlark  = false;
            mibl_config.dump_mibl      = false;
            mibl_config.dump_parsetree = true;
        }
        else {
            printf("Invalid dump arg: %s. Allowed: parsetree, mibl. Default: none\n", options[OPT_DUMP].argument);
            exit(EXIT_FAILURE);
        }
    }

    if (options[OPT_EMIT].count) {
        /* printf("emit ct: %d\n", options[OPT_EMIT].count); */
        if (strncmp(options[OPT_EMIT].argument, "starlark", 8) == 0) {
            printf("emitting starlark\n");
            mibl_config.emit_starlark = true;
        }
        else if (strncmp(options[OPT_EMIT].argument, "mibl", 4) == 0) {
            printf("emitting mibl\n");
            mibl_config.emit_mibl = true;
        }
        else if (strncmp(options[OPT_EMIT].argument, "parsetree", 9) == 0) {
            printf("emitting parsetree\n");
            mibl_config.emit_parsetree = true;
        }
        else if (strncmp(options[OPT_EMIT].argument, "none", 4) == 0) {
            if (verbose && verbosity > 1)
                log_info("emit function disabled");
            mibl_config.emit_starlark = false;
            mibl_config.emit_mibl = false;
            mibl_config.emit_parsetree = false;
        }
        else {
            printf("Invalid emit arg: %s. Allowed: parsetree, mibl, starlark, none. Default: starlark\n", options[OPT_EMIT].argument);
            exit(EXIT_FAILURE);
        }
    }

    if (verbose && verbosity > 1) {
        log_debug("DUMP_PARSETREE: %d", mibl_config.dump_parsetree);
        log_debug("DUMP_MIBL: %d", mibl_config.dump_mibl);
        log_debug("DUMP_STARLARK: %d", mibl_config.dump_starlark);
        log_debug("EMIT_PARSETREE: %d", mibl_config.emit_parsetree);
        log_debug("EMIT_MIBL: %d", mibl_config.emit_mibl);
        log_debug("EMIT_STARLARK: %d", mibl_config.emit_starlark);
    }

    /* cc tc sets this if we are being built as an external repo: */
/* #ifdef BAZEL_CURRENT_REPOSITORY */
/*     current_repo = getenv("BAZEL_CURRENT_REPOSITORY"); */
/*     log_debug("BAZEL_CURRENT_REPOSITORY 2: %s", BAZEL_CURRENT_REPOSITORY); */
/* #endif */

    s7_scheme *s7 = s7_configure();

    /* now starlark stuff */

    UT_string *setter;
    utstring_new(setter);

    /* printf("SETTING *dump-starlark* to %d\n", dump_starlark); */
    utstring_printf(setter, "(set! *dump-starlark* %s)",
                    mibl_config.dump_starlark? "#t": "#f");
    s7_eval_c_string(s7, utstring_body(setter));

    utstring_renew(setter);
    /* printf("SETTING *dump-mibl* to %s\n", */
    /*                 dump_mibl? "#t" : "#f"); */
    utstring_printf(setter, "(set! *dump-mibl* %s)",
                    mibl_config.dump_mibl? "#t" : "#f");
    s7_eval_c_string(s7, utstring_body(setter));

    utstring_renew(setter);
    /* printf("SETTING *dump-parsetree* to %d\n", dump_parsetree); */
    utstring_printf(setter, "(set! *dump-parsetree* %s)",
                    mibl_config.dump_parsetree? "#t": "#f");
    s7_eval_c_string(s7, utstring_body(setter));

    /* **************************************************************** */
    utstring_new(setter);
    utstring_printf(setter, "(set! *emit-starlark* %s)",
                    mibl_config.emit_starlark? "#t" : "#f");
    s7_eval_c_string(s7, utstring_body(setter));

    utstring_renew(setter);
    utstring_printf(setter, "(set! *emit-mibl* %s)",
                    mibl_config.emit_mibl? "#t" : "#f");
    s7_eval_c_string(s7, utstring_body(setter));

    utstring_renew(setter);
    utstring_printf(setter, "(set! *emit-parsetree* %s)",
                    mibl_config.emit_parsetree? "#t": "#f");
    s7_eval_c_string(s7, utstring_body(setter));

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
    if (verbose)
        log_info("*load-path*: %s", TO_STR(s7_load_path(s7)));

    s7_load(s7, "starlark.scm");

    s7_load(s7, "convert_dune.scm");

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

#if defined(DEBUG_TRACE)
    if (debug) log_debug("s7 args: %s", TO_STR(_s7_args));
#endif

    /* s7_gc_on(s7, s7_f(s7)); */


    /* s7_int main_gc_loc = s7_gc_protect(s7, _main); */

    if (verbose && verbosity > 2)
        log_info("calling s7: %s", TO_STR(_main));
    /* this does the actual conversion: */
    s7_pointer result = s7_apply_function(s7, _main, _s7_args);
    /* FIXME: check result */
    (void)result;

    /* log_info("RESULT: %s\n", TO_STR(result)); */
    s7_gc_unprotect_at(s7, (s7_int)_main);

    char *errmsg = (char*)s7_get_output_string(s7, s7_current_error_port(s7));
    if ((errmsg) && (*errmsg)) {
        log_error("[%s\n]", errmsg);
        s7_quit(s7);
        exit(EXIT_FAILURE);
    }
    if (verbose)
        log_info("convert exit...");
    return 0;
}