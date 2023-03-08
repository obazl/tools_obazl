#include <errno.h>
#include <unistd.h>

#include "gopt.h"
#include "log.h"
#include "utarray.h"
#include "utstring.h"
#include "s7.h"

#include "libmibl.h"

#include "dune.h"

extern bool debug;
#if defined(DEBUG_TRACE)
extern bool trace;
#endif

extern bool verbose;

extern bool bzl_mode;

extern struct mibl_config_s mibl_config;

/* extern s7_scheme *s7; */

extern char *ews_root;
extern int dir_ct;
extern int file_ct;
extern int dunefile_ct;

extern bool debug_bazel;
extern bool debug_mibl;
extern bool debug_s7_config;

/* extern bool debug_bazel; */
/* extern bool debug_mibl; */
/* extern bool trace_mibl; */


extern bool emit_parsetree;
bool emit_mibl      = false;
bool emit_starlark  = true;

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

void _print_version(void) {
    printf("FIXME: version id\n");
}

void _print_usage(void) {
    printf("Usage:\t$ bazel run @obazl//convert [flags, options]\n");
    printf("Flags (note that some flags require double-hyphenation):\n");
    printf("\t-d   | --debug\t\t\tEnable all debugging flags.\n");
    printf("\t--dx | --debug-executables\tDebug handling of Dune executable and executables stanzas.\n");
    printf("\t--de | --debug-emit\t\tDebug emit logic.\n");
    printf("\t--dm | --debug-mibl\t\tDebug mibl elaboration.\n");
    printf("\t--dppx | --debug-ppx\t\tDebug ppx stuff.\n");

    printf("\t--show-config\t\t\tPrint config and exit.\n");
    printf("\t--lparsetree, --show-parsetree\n");
    printf("\t--lexports,   --show-exports\n");
    printf("\t--lmibl,      --show-mibl\n");
    printf("\t--lstarlark,  --show-starlark\n");

    printf("\t--em | --emit-mibl\t\tEmit BUILD.mibl files.\n");
    printf("\t--em | --emit-parsetree\t\tEmit PARSETREE.mibl files.\n");
    printf("\t--em | --emit-starlark\t\tEmit BUILD.bazel files.\n");
    printf("\t--no-emit\t\t\tDisable emitting.\n");

    printf("\t-t  | --trace\t\t\tEnable trace flags.\n");
    printf("\t-v  | --verbose\t\t\tEnable verbosity. Repeatable.\n");
    printf("\t--menhir\t\t\tEmit 'menhir' targets for .mly files, instead of ocamlyacc.\n");

    printf("Options:\n");
    /* printf("\t-D | -log <arg>\t\tLog <arg> (parsetree, mibl, or starlark}) to stdout.\n"); */
    printf("\t-e | --emit <arg>\t\tEmit BUILD.<arg> files, where <arg> = mibl | bazel. BUILD.bazel always emitted unless --no-emit passed.\n");

    printf("\t-p | --pkg | --package <arg>"
           "\tRestrict log ouput to <arg> (relative pkg path).\n");

}

enum OPTS {
    OPT_ROOT = 0,
    OPT_PKG,
    OPT_PACKAGE,

    /* NB: no-emit flags are for overriding miblrc config */
    OPT_EMIT,
    FLAG_EMIT_STARLARK,
    FLAG_NO_EMIT_STARLARK,
    FLAG_EMIT_EM,            /* = FLAG_EMIT_MIBL */
    FLAG_EMIT_MIBL,
    FLAG_NO_EMIT_MIBL,
    FLAG_EMIT_PT,            /* = FLAG_EMIT_PARSETREE */
    FLAG_EMIT_PARSETREE,
    FLAG_NO_EMIT_PARSETREE,
    FLAG_NOEMIT,
    FLAG_MENHIR,
    /* logging */
    FLAG_LEXPORTS,
    FLAG_SHOW_EXPORTS,
    FLAG_LMIBL,
    FLAG_SHOW_MIBL,
    FLAG_LPARSETREE,
    FLAG_SHOW_PARSETREE,
    FLAG_LSTARLARK,
    FLAG_SHOW_STARLARK,

    FLAG_ONLY_CONFIG,
    FLAG_ONLY_PARSETREE,
    FLAG_ONLY_MIBL,

    FLAG_HELP,
    FLAG_DEBUG,
    FLAG_DEBUG_CONFIG,
    FLAG_DEBUG_DE,
    FLAG_DEBUG_EMIT,
    FLAG_DEBUG_DX,
    FLAG_DEBUG_EXECUTABLES,
    FLAG_DEBUG_LOADS,
    FLAG_DEBUG_DM,
    FLAG_DEBUG_MIBL,
    FLAG_DEBUG_DPPX,
    FLAG_DEBUG_PPX,

    FLAG_SHOW_CONFIG,

    FLAG_TRACE,
    FLAG_VERBOSE,
    FLAG_QUIET,
    FLAG_VERSION,
    LAST
};

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
    [FLAG_EMIT_STARLARK] = {.long_name="emit-starlark",
                            .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_NO_EMIT_STARLARK] = {.long_name="no-emit-starlark",
                               .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_EMIT_EM] = {.long_name="em",
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_MIBL] = {.long_name="emit-mibl",
                        .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_NO_EMIT_MIBL] = {.long_name="no-emit-mibl",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_EMIT_PT] = {.long_name="ept",
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_PARSETREE] = {.long_name="emit-parsetree",
                             .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_NO_EMIT_PARSETREE] = {.long_name="no-emit-parsetree",
                                .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_NOEMIT] = {.long_name="no-emit",.short_name='E',
                     .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_MENHIR] = {.long_name="menhir", .flags=GOPT_ARGUMENT_FORBIDDEN},

    /* stdout options (stages): parsetree, mibl. starlark */
    /* i.e. we can emit starlark but print mibl to stdout */
    /* -e starlark -D mibl */
    /* only one -D allowed at a time */
    /* [OPT_LOG] = {.long_name="log",.short_name='D', */
    /*               .flags=GOPT_ARGUMENT_REQUIRED}, */
    [FLAG_LEXPORTS] = {.long_name="lexports",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_EXPORTS] = {.long_name="show-exports",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_LMIBL] = {.long_name="lmibl",
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_MIBL] = {.long_name="show-mibl",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_LPARSETREE] = {.long_name="lparsetree",
                         .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_PARSETREE] = {.long_name="show-parsetree",
                            .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_LSTARLARK] = {.long_name="lstarlark",
                        .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_STARLARK] = {.long_name="show-starlark",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_ONLY_CONFIG] = {.long_name="only-config",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_ONLY_PARSETREE] = {.long_name="only-parsetree",
                             .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_ONLY_MIBL] = {.long_name="only-MIBL",
                        .flags=GOPT_ARGUMENT_FORBIDDEN},


    [FLAG_HELP] = {.long_name="help",.short_name='h',
                   .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG] = {.long_name="debug",.short_name='d',
                    .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_DEBUG_CONFIG] = {.long_name="debug-config",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_DX] = {.long_name="dx",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_EXECUTABLES] = {.long_name="debug-executables",
                                .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_DE] = {.long_name="de",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_EMIT] = {.long_name="debug-emit",
                         .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_DM] = {.long_name="dm",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_LOADS] = {.long_name="debug-loads",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_MIBL] = {.long_name="debug-mibl",
                         .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_DPPX] = {.long_name="dppx",
                         .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_PPX] = {.long_name="debug-ppx",
                        .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_SHOW_CONFIG] = {.long_name="show-config",
                           .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_TRACE] = {.long_name="trace",.short_name='t',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v',
                      .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_QUIET] = {.long_name="quiet",.short_name='q',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERSION] = {.long_name="version",
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [LAST] = {.flags = GOPT_LAST}
};

int _update_mibl_config(struct option options[],
                   struct mibl_config_s *mibl_config)
{
#if defined(DEBUG_TRACE)
    if (mibl_trace) {
        log_debug("_update_mibl_config");
        log_debug("options[OPT_PKG].count: %d", options[OPT_PKG].count);
    }
#endif
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
                    return -1;
                } else {
                    /* log_debug("miblrc pushing pkg: %s", token); */
                    utarray_push_back(mibl_config->pkgs, &token);
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
                    return -1;
                } else {
                    log_debug("miblrc pushing pkg: %s", token);
                    utarray_push_back(mibl_config->pkgs, &token);
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

    if (options[FLAG_NOEMIT].count) {
        if (verbose && verbosity > 1)
            log_info("defaulting emit functions to #f");
        mibl_config->emit_starlark = false;
        mibl_config->emit_mibl = false;
        mibl_config->emit_parsetree = false;
    }

    if (options[FLAG_EMIT_STARLARK].count) {
        mibl_config->emit_starlark = true;
    }
    if (options[FLAG_NO_EMIT_STARLARK].count) {
        mibl_config->emit_starlark = false;
    }

    if ((options[FLAG_EMIT_MIBL].count)
        || (options[FLAG_EMIT_EM].count)) {
        mibl_config->emit_mibl = true;
    }
    if (options[FLAG_NO_EMIT_MIBL].count) {
        mibl_config->emit_mibl = false;
    }

    if ((options[FLAG_EMIT_PARSETREE].count)
        || (options[FLAG_EMIT_PT].count)) {
        mibl_config->emit_parsetree = true;
    }
    if (options[FLAG_NO_EMIT_PARSETREE].count) {
        mibl_config->emit_mibl = false;
    }

    if (options[OPT_EMIT].count) {
        /* printf("emit opt: %d - %s\n", */
        /*        options[OPT_EMIT].count, */
        /*        options[OPT_EMIT].argument); */
        if (strncmp(options[OPT_EMIT].argument, "starlark", 8) == 0) {
            /* printf("emitting starlark\n"); */
            mibl_config->emit_starlark = true;
        }
        if (strncmp(options[OPT_EMIT].argument, "bazel", 5) == 0) {
            /* printf("emitting starlark\n"); */
            mibl_config->emit_starlark = true;
        }
        else if (strncmp(options[OPT_EMIT].argument, "mibl", 4) == 0) {
            /* printf("emitting mibl\n"); */
            /* mibl_config->emit_parsetree = false; */
            mibl_config->emit_mibl = true;
            /* mibl_config->emit_starlark = false; */
        }
        else if (strncmp(options[OPT_EMIT].argument, "parsetree", 9) == 0) {
            /* printf("emitting parsetree\n"); */
            mibl_config->emit_parsetree = true;
        }
        else {
            printf("Invalid emit arg: %s. Allowed: parsetree, mibl, starlark. Default: starlark\n", options[OPT_EMIT].argument);
            exit(EXIT_FAILURE);
        }
    }

    if (verbose && verbosity > 1) {
        log_debug("SHOW_EXPORTS: %d", mibl_config->show_exports);
        log_debug("SHOW_MIBL: %d", mibl_config->show_mibl);
        log_debug("SHOW_PARSETREE: %d", mibl_config->show_parsetree);
        log_debug("SHOW_STARLARK: %d", mibl_config->show_starlark);
        log_debug("EMIT_PARSETREE: %d", mibl_config->emit_parsetree);
        log_debug("EMIT_MIBL: %d", mibl_config->emit_mibl);
        log_debug("EMIT_STARLARK: %d", mibl_config->emit_starlark);
    }
    return 0;                   /* success */
}

void _update_s7_globals(struct option options[])
{
    /* mibl_s7_set_flag("*debugging*", true); */
    if (options[FLAG_DEBUG_LOADS].count)
        mibl_s7_set_flag("*mibl-debug-loads*", true);

    mibl_s7_set_flag("*mibl-quiet*", ((options[FLAG_QUIET].count) > 0));

    mibl_s7_set_flag("*debugging*", options[FLAG_DEBUG].count);
    mibl_s7_set_flag("*debug-emit*",
                    ((options[FLAG_DEBUG_DE].count)
                     || (options[FLAG_DEBUG_EMIT].count)));
    mibl_s7_set_flag("*debug-executables*",
                    ((options[FLAG_DEBUG_DX].count)
                     || (options[FLAG_DEBUG_EXECUTABLES].count)));
    mibl_s7_set_flag("*debug-mibl*",
                    ((options[FLAG_DEBUG_DM].count)
                     || (options[FLAG_DEBUG_MIBL].count)));
    mibl_s7_set_flag("*debug-ppx*",
                    ((options[FLAG_DEBUG_DPPX].count)
                     || (options[FLAG_DEBUG_PPX].count)));

    if ((options[FLAG_LEXPORTS].count)
        || (options[FLAG_SHOW_EXPORTS].count))
        mibl_s7_set_flag("*show-exports*", true);
    else if (mibl_config.show_exports)
        mibl_s7_set_flag("*show-exports*", true);

    if ((options[FLAG_LMIBL].count)
        || (options[FLAG_SHOW_MIBL].count))
        mibl_s7_set_flag("*show-mibl*", true);
    else if (mibl_config.show_mibl)
        mibl_s7_set_flag("*show-mibl*", true);

    mibl_s7_set_flag("*emit-starlark*", ((options[FLAG_EMIT_STARLARK].count) > 0));
    mibl_s7_set_flag("*menhir*", ((options[FLAG_MENHIR].count) >0));

}

int main(int argc, char **argv)
{
    /* int opt; */
    int len;
    char *rootpath = NULL;
    /* char *pkgarg = NULL; */

    /* bool exit_on_error = false; */

    argc = gopt(argv, options);
    gopt_errors(argv[0], options);

    /* **************************************************************** */

    if (options[FLAG_HELP].count) {
        _print_usage();
        exit(EXIT_SUCCESS);
    }

    if (options[FLAG_VERSION].count) {
        _print_version();
        exit(EXIT_SUCCESS);
    }

    if (options[FLAG_VERBOSE].count) {
        /* printf("verbose ct: %d\n", options[FLAG_VERBOSE].count); */
        verbose = true;
        verbosity = options[FLAG_VERBOSE].count;
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

    if (options[FLAG_DEBUG].count) {
#if defined(DEBUG_TRACE)
        debug = true;
#endif
    }

    if (options[FLAG_TRACE].count) {
        /* printf("trace ct: %d\n", options[FLAG_TRACE].count); */
#if defined(DEBUG_TRACE)
        trace = true;
#endif
    }

#if defined(DEBUG_TRACE)
    if (debug) {
        log_debug("argc: %d", argc);
        log_debug("optind: %d", optind);
        log_debug("argv[0]: %s", argv[0]);
    }
#endif
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
        log_debug("ROOTPATH: '%s'", rootpath);
        log_debug("pkgarg: '%s'", pkgarg);
    }
#endif

    if (options[FLAG_DEBUG_CONFIG].count) {
#if defined(DEBUG_TRACE)
        debug_bazel = true;
        debug_mibl = true;
        debug_s7_config = true;
#else
        log_error("--debug-config only valid with -c dbg");
        exit(EXIT_FAILURE);
#endif
    }

    /* **************************************************************** */

    struct mibl_config_s *mibl_config = mibl_s7_init("//obazl/scm",
                                                     NULL);
    log_debug("ADUNE CWD: %s", getcwd(NULL, 0));

    if (_update_mibl_config(options, mibl_config)) exit(EXIT_FAILURE);

    _update_s7_globals(options);

    if (options[FLAG_SHOW_CONFIG].count) {
        show_bazel_config();
        show_mibl_config();
        show_s7_config();

        /* for (char **env = envp; *env != 0; env++) { */
        /*     char *thisEnv = *env; */
        /*     printf("%s\n", thisEnv); */
        /* } */
        exit(EXIT_SUCCESS);
    }
    log_debug("DUNE CWD: %s", getcwd(NULL, 0));
    mibl_s7_run("obazl_main.scm", NULL);

    if (verbose)
        log_info("convert exit...");
    return 0;
}
