#include <errno.h>
#include <unistd.h>

#include "gopt.h"
#include "log.h"
#include "utarray.h"
#include "utstring.h"

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

bool emit_parsetree = false;
bool emit_mibl      = false;
bool emit_bazel  = true;

enum OPTS {
    OPT_ROOT = 0,
    OPT_PKG,
    OPT_PACKAGE,
    OPT_EMIT,
    OPT_EMIT_EM,            /* = OPT_EMIT_MIBL */
    OPT_EMIT_MIBL,
    OPT_NO_EMIT_MIBL,
    OPT_EMIT_EB,            /* = OPT_EMIT_BAZEL */
    OPT_EMIT_BAZEL,
    OPT_NO_EMIT_BAZEL,
    OPT_NOEMIT,
    OPT_MENHIR,
    OPT_DUMP_EXPORTS,
    OPT_DUMP_MIBL,
    OPT_DUMP_PARSETREE,
    OPT_DUMP_STARLARK,
    OPT_HELP,
    OPT_DEBUG,
    OPT_DEBUG_DE,
    OPT_DEBUG_EMIT,
    OPT_DEBUG_DX,
    OPT_DEBUG_EXECUTABLES,
    OPT_DEBUG_DM,
    OPT_DEBUG_MIBL,
    OPT_DEBUG_DPPX,
    OPT_DEBUG_PPX,
    OPT_TRACE,
    OPT_VERBOSE,
    OPT_LAST
};

void _check_tools(void) {
    /* is shell available? */
    int rc = system(NULL);
    if (rc == 0) {
        fprintf(stderr, "No system shell available\n");
        exit(EXIT_FAILURE);
    }

    /* FIXME: 'system' not portable.  instead, scan $PATH...? */
    if (system("which opam > /dev/null 2>&1")) {
        fprintf(stderr, "Cmd 'opam' not found, but it is required by the conversion tool.\n");
        exit(EXIT_FAILURE);
    }

    if (system("which ocamldep > /dev/null 2>&1")) {
        fprintf(stderr, "Cmd 'ocamldep' not found, but it is required by the conversion tool. If it is installed, try running 'eval $(opam env)'.\n");
        exit(EXIT_FAILURE);
    }

    /* if (system("which foobar > /dev/null 2>&1")) { */
    /*     fprintf(stderr, RED "ERROR: " CRESET "Command 'foobar' not found. Please run 'opam install ocamldep'.\n"); */
    /*     exit(EXIT_FAILURE); */
    /* } */
}

void _print_usage(void) {
    printf("Usage:\t$ bazel run @obazl//convert [flags, options]\n");
    printf("Flags (note that some flags require double-hyphenation):\n");
    printf("\t-d  | --debug\t\t\tEnable all debugging flags.\n");
    printf("\t--dx | --debug-executables\tDebug handling of Dune executable and executables stanzas.\n");
    printf("\t--de | --debug-emit\t\tDebug emit logic.\n");
    printf("\t--dm | --debug-mibl\t\tDebug mibl elaboration.\n");
    printf("\t--dppx | --debug-ppx\t\tDebug ppx stuff.\n");
    printf("\t--dump-exports\t\tDebug exported syms table.\n");
    printf("\t--em | --emit-mibl\t\tEmit BUILD.mibl files.\n");
    printf("\t--no-emit\t\t\tDisable emitting.\n");
    printf("\t-t  | --trace\t\t\tEnable trace flags.\n");
    printf("\t-v  | --verbose\t\t\tEnable verbosity. Repeatable.\n");
    printf("\t--menhir\t\t\tEmit 'menhir' targets for .mly files, instead of ocamlyacc.\n");

    printf("Options:\n");
    printf("\t-D | -dump <arg>\t\tDump <arg> (parsetree, mibl, or starlark}) to stdout.\n");
    printf("\t-e | --emit <arg>\t\tEmit BUILD.<arg> files, where <arg> = mibl | bazel. BUILD.bazel always emitted unless --no-emit passed.\n");

    printf("\t-p | --pkg | --package <arg>"
           "\tRestrict dump ouput to <arg> (relative pkg path).\n");

}

int main(int argc, char **argv)
{
    /* int opt; */
    int len;
    char *rootpath = NULL;
    char *pkgarg = NULL;

    bool menhir = false;
    bool exit_on_error = false;

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
        [OPT_EMIT_EM] = {.long_name="em",
                         .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_EMIT_MIBL] = {.long_name="emit-mibl",
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_NO_EMIT_MIBL] = {.long_name="no-emit-mibl",
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_EMIT_EB] = {.long_name="no-eb",
                         .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_EMIT_BAZEL] = {.long_name="emit-bazel",
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_NO_EMIT_BAZEL] = {.long_name="no-emit-bazel",
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_NOEMIT] = {.long_name="no-emit",.short_name='E',
                      .flags=GOPT_ARGUMENT_FORBIDDEN},

        [OPT_MENHIR] = {.long_name="menhir", .flags=GOPT_ARGUMENT_FORBIDDEN},

        /* stdout options (stages): parsetree, mibl. starlark */
        /* i.e. we can emit starlark but print mibl to stdout */
        /* -e starlark -D mibl */
        /* only one -D allowed at a time */
        /* [OPT_DUMP] = {.long_name="dump",.short_name='D', */
        /*               .flags=GOPT_ARGUMENT_REQUIRED}, */
        [OPT_DUMP_EXPORTS] = {.long_name="dump-exports",
                              .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_DUMP_MIBL] = {.long_name="dump-mibl",
                              .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_DUMP_PARSETREE] = {.long_name="dump-parsetree",
                              .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_DUMP_STARLARK] = {.long_name="dump-starlark",
                              .flags=GOPT_ARGUMENT_FORBIDDEN},

        [OPT_HELP] = {.long_name="help",.short_name='h',
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_DEBUG] = {.long_name="debug",.short_name='d',
                       .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
        [OPT_DEBUG_DX] = {.long_name="dx",
                            .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_DEBUG_EXECUTABLES] = {.long_name="debug-executables",
                            .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_DEBUG_DE] = {.long_name="de",
                            .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_DEBUG_EMIT] = {.long_name="debug-emit",
                            .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_DEBUG_DM] = {.long_name="dm",
                            .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_DEBUG_MIBL] = {.long_name="debug-mibl",
                            .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_DEBUG_DPPX] = {.long_name="dppx",
                            .flags=GOPT_ARGUMENT_FORBIDDEN},
        [OPT_DEBUG_PPX] = {.long_name="debug-ppx",
                            .flags=GOPT_ARGUMENT_FORBIDDEN},
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

    if (options[OPT_MENHIR].count) {
        /* printf("menhir ct: %d\n", options[OPT_MENHIR].count); */
        menhir = true;
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
        _print_usage();
        exit(EXIT_SUCCESS);
    }

    if (options[OPT_DEBUG].count) { debug = true; }

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

    _check_tools();

#if defined(DEBUG_TRACE)
    if (debug) {
        log_debug("rootpath: '%s'", rootpath);
        log_debug("pkgarg: '%s'", pkgarg);
    }
#endif

    /* config in this order: first bazel, then mibl, then s7 */
    bazel_configure(); // getcwd(NULL, 0));

    mibl_configure();

    /* char **p = NULL; */
    /* while ( (p=(char**)utarray_next(mibl_config.pkgs, p))) { */
    /*     log_info("miblrc pkg: %s", *p); */
    /* } */

    /* cmd line --pkg args augment miblrc */
    if (options[OPT_PKG].count || options[OPT_PACKAGE].count) {
        char *token, *sep = " ,\t";
        /* printf("pkg ct: %d\n", */
        /*        options[OPT_PKG].count + options[OPT_PACKAGE].count); */
        if (options[OPT_PKG].count) {
            token = strtok((char*)options[OPT_PKG].argument, sep);
            while( token != NULL ) {
                if (token[0] == '/') {
                    log_error("-pkg values must be relative paths: %s", token);
                    return 0;
                } else {
                    /* log_debug("miblrc pushing pkg: %s", token); */
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

    if (options[OPT_NOEMIT].count) {
        if (verbose && verbosity > 1)
            log_info("defaulting emit functions to #f");
        mibl_config.emit_bazel = false;
        mibl_config.emit_mibl = false;
        mibl_config.emit_parsetree = false;
    }

    if ((options[OPT_EMIT_BAZEL].count)
        || (options[OPT_EMIT_EB].count)) {
        mibl_config.emit_bazel = true;
    }
    if (options[OPT_NO_EMIT_BAZEL].count) {
        mibl_config.emit_bazel = false;
    }

    if ((options[OPT_EMIT_MIBL].count)
        || (options[OPT_EMIT_EM].count)) {
        mibl_config.emit_mibl = true;
    }
    if (options[OPT_NO_EMIT_MIBL].count) {
        mibl_config.emit_mibl = false;
    }

    if (options[OPT_EMIT].count) {
        /* printf("emit opt: %d - %s\n", */
        /*        options[OPT_EMIT].count, */
        /*        options[OPT_EMIT].argument); */
        if (strncmp(options[OPT_EMIT].argument, "starlark", 8) == 0) {
            /* printf("emitting starlark\n"); */
            mibl_config.emit_bazel = true;
        }
        if (strncmp(options[OPT_EMIT].argument, "bazel", 5) == 0) {
            /* printf("emitting starlark\n"); */
            mibl_config.emit_bazel = true;
        }
        else if (strncmp(options[OPT_EMIT].argument, "mibl", 4) == 0) {
            /* printf("emitting mibl\n"); */
            /* mibl_config.emit_parsetree = false; */
            mibl_config.emit_mibl = true;
            /* mibl_config.emit_bazel = false; */
        }
        else if (strncmp(options[OPT_EMIT].argument, "parsetree", 9) == 0) {
            /* printf("emitting parsetree\n"); */
            mibl_config.emit_parsetree = true;
        }
        else {
            printf("Invalid emit arg: %s. Allowed: parsetree, mibl, starlark. Default: starlark\n", options[OPT_EMIT].argument);
            exit(EXIT_FAILURE);
        }
    }

    if (verbose && verbosity > 1) {
        log_debug("MENHIR: %d", menhir);
        log_debug("DUMP_EXPORTS: %d", mibl_config.dump_exports);
        log_debug("DUMP_MIBL: %d", mibl_config.dump_mibl);
        log_debug("DUMP_PARSETREE: %d", mibl_config.dump_parsetree);
        log_debug("DUMP_STARLARK: %d", mibl_config.dump_starlark);
        log_debug("EMIT_PARSETREE: %d", mibl_config.emit_parsetree);
        log_debug("EMIT_MIBL: %d", mibl_config.emit_mibl);
        log_debug("EMIT_BAZEL: %d", mibl_config.emit_bazel);
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

    /* printf("SETTING *debugging* to %s\n", debug? "#t": "#f"); */
    utstring_printf(setter, "(set! *debugging* %s)",
                    (options[OPT_DEBUG].count)? "#t": "#f");
    s7_eval_c_string(s7, utstring_body(setter));

    utstring_renew(setter);
    utstring_printf(setter, "(set! *debug-emit* %s)",
                    ((options[OPT_DEBUG_DE].count)
                     || (options[OPT_DEBUG_EMIT].count))?
                    "#t" : "#f");
    s7_eval_c_string(s7, utstring_body(setter));

    utstring_renew(setter);
    utstring_printf(setter, "(set! *debug-executables* %s)",
                    ((options[OPT_DEBUG_DX].count)
                     || (options[OPT_DEBUG_EXECUTABLES].count))?
                    "#t" : "#f");
    s7_eval_c_string(s7, utstring_body(setter));

    utstring_renew(setter);
    utstring_printf(setter, "(set! *debug-mibl* %s)",
                    ((options[OPT_DEBUG_DM].count)
                     || (options[OPT_DEBUG_MIBL].count))?
                    "#t" : "#f");
    s7_eval_c_string(s7, utstring_body(setter));

    utstring_renew(setter);
    utstring_printf(setter, "(set! *debug-ppx* %s)",
                    ((options[OPT_DEBUG_DPPX].count)
                     || (options[OPT_DEBUG_PPX].count))?
                    "#t" : "#f");
    s7_eval_c_string(s7, utstring_body(setter));

    /* **************************************************************** */
    /* dumps */
    log_debug("dump exports? %d", mibl_config.dump_exports);
    utstring_renew(setter);
    if (options[OPT_DUMP_EXPORTS].count) {
        utstring_printf(setter, "(set! *dump-exports* %s)", "#t");
        s7_eval_c_string(s7, utstring_body(setter));
    }
    else if (mibl_config.dump_exports) {
        utstring_printf(setter, "(set! *dump-exports* %s)", "#t");
        s7_eval_c_string(s7, utstring_body(setter));
    }

    utstring_renew(setter);
    if (options[OPT_DUMP_MIBL].count) {
        utstring_printf(setter, "(set! *dump-mibl* %s)", "#t");
        s7_eval_c_string(s7, utstring_body(setter));
    }
    else if (mibl_config.dump_mibl) {
        utstring_printf(setter, "(set! *dump-mibl* %s)", "#t");
        s7_eval_c_string(s7, utstring_body(setter));
    }

    utstring_renew(setter);
    if (options[OPT_DUMP_PARSETREE].count) {
        utstring_printf(setter, "(set! *dump-parsetree* %s)", "#t");
        s7_eval_c_string(s7, utstring_body(setter));
    }
    else if (mibl_config.dump_parsetree) {
        utstring_printf(setter, "(set! *dump-parsetree* %s)", "#t");
        s7_eval_c_string(s7, utstring_body(setter));
    }

    utstring_renew(setter);
    if (options[OPT_DUMP_STARLARK].count) {
        utstring_printf(setter, "(set! *dump-starlark* %s)", "#t");
        s7_eval_c_string(s7, utstring_body(setter));
    }
    else if (mibl_config.dump_starlark) {
        utstring_printf(setter, "(set! *dump-starlark* %s)", "#t");
        s7_eval_c_string(s7, utstring_body(setter));
    }

    /* **************************************************************** */
    utstring_new(setter);
    utstring_printf(setter, "(set! *emit-starlark* %s)",
                    mibl_config.emit_bazel? "#t" : "#f");
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
        utstring_renew(setter);
        utstring_printf(setter, "(set! *emit-bazel-pkg* \"%s\")", pkgarg);
        /* log_debug("SETTING *emit-bazel-pkg* to: %s", pkgarg); */
        s7_eval_c_string(s7, utstring_body(setter));
    }

    /* printf("SETTING *menhir* to %s\n", menhir? "#t": "#f"); */
    utstring_renew(setter);
    utstring_printf(setter, "(set! *menhir* %s)",
                    menhir? "#t": "#f");
    s7_eval_c_string(s7, utstring_body(setter));

    utstring_free(setter);

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

    /* **************************************************************** */
    /* this does the actual conversion: */
    s7_pointer result = s7_apply_function(s7, _main, _s7_args);
    (void)result; /* FIXME: check result */
    /* **************************************************************** */

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
