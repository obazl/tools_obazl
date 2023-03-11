#include <unistd.h>

#include "gopt.h"
#include "log.h"

#include "s7.h"                 /* needed by libmibl.h */
#include "libmibl.h"

#include "script.h"

#if defined(DEBUG_TRACE)
extern bool mibl_debug;
extern bool mibl_debug_mibl_crawl;
extern bool mibl_trace;
#endif

extern bool mibl_show_traversal;
extern bool verbose;

extern char *ews_root;
extern int dir_ct;
extern int file_ct;
extern int dunefile_ct;

extern struct mibl_config_s mibl_config;

#define DEV_MODE

enum OPTS {
    OPT_MAIN,
    OPT_WS,
    OPT_FLAGS, /* ad-hoc flags; if not passed, (if *mibl-foo*...) fails */

    FLAG_HELP,
    FLAG_SHOW_CONFIG,
    FLAG_SHOW_PARSETREE,
    FLAG_SHOW_MIBL,
    FLAG_SHOW_TRAVERSAL,
    FLAG_DEBUG,
#if defined(DEV_MODE)
    FLAG_DEBUG_PPX,
    FLAG_DEBUG_S7,
    FLAG_DEBUG_LOAD_PROJECT,
#endif

    FLAG_CLEAN,
    FLAG_CLEAN_MIBL,
    FLAG_CLEAN_S7,

    FLAG_EMIT_PKGS,
    FLAG_EMIT_WSS,
    FLAG_EMIT_PARSETREE,
    FLAG_EMIT_RESULT,
    // formats:
    FLAG_EMIT_MIBL,
    FLAG_EMIT_S7,

    FLAG_TRACE,
    FLAG_VERBOSE,
    FLAG_QUIET,
    FLAG_VERSION,
    LAST
};

int _update_mibl_config(struct option options[],
                        struct mibl_config_s *mibl_config)
{
    /* log_debug("_update_mibl_config"); */

    if (verbose && verbosity > 1) {
        log_debug("SHOW_EXPORTS: %d", mibl_config->show_exports);
        log_debug("SHOW_PARSETREE: %d", mibl_config->show_parsetree);
        /* log_debug("SHOW_MIBL: %d", mibl_config->show_mibl); */
        log_debug("SHOW_STARLARK: %d", mibl_config->show_starlark);
        log_debug("EMIT_PARSETREE: %d", mibl_config->emit_parsetree);
        log_debug("EMIT_MIBL: %d", mibl_config->emit_mibl);
        log_debug("EMIT_STARLARK: %d", mibl_config->emit_starlark);
    }
    return 0;                   /* success */
}

void _update_s7_globals(struct option options[])
{
    /* mibl_s7_set_flag("*mibl-debugging*", true); */
    mibl_s7_set_flag("*mibl-quiet*", ((options[FLAG_QUIET].count) > 0));

    mibl_s7_set_flag("*mibl-debugging*", (options[FLAG_DEBUG].count > 0));
    mibl_s7_set_flag("*mibl-debug-s7*", (options[FLAG_DEBUG_S7].count > 0));

    if (options[FLAG_DEBUG_PPX].count)
        mibl_s7_set_flag("*mibl-debug-ppx*", true);
    else {
        s7_pointer fld = s7_name_to_value(s7, "*mibl-debug-ppx*");
        if (fld == s7_undefined(s7)) {
            mibl_s7_set_flag("*mibl-debug-ppx*", false);
        }
    }

    mibl_s7_set_flag("*mibl-debug-show-pkgs*", false);
    mibl_s7_set_flag("*mibl-debug-modules*", false);
    mibl_s7_set_flag("*mibl-debug-tests*", false);
    mibl_s7_set_flag("*mibl-debug-updaters*", false);

    if (options[FLAG_SHOW_MIBL].count)
        mibl_s7_set_flag("*mibl-show-mibl*", true);
    else // if (mibl_config.show_mibl)
        mibl_s7_set_flag("*mibl-show-mibl*", false);

    if (options[FLAG_SHOW_PARSETREE].count)
        mibl_s7_set_flag("*mibl-show-parsetree*", true);
    else if (mibl_config.show_parsetree)
        mibl_s7_set_flag("*mibl-show-parsetree*", true);

    if (options[FLAG_EMIT_MIBL].count)
        mibl_s7_set_flag("*mibl-emit-mibl*", true);
    else
        mibl_s7_set_flag("*mibl-emit-mibl*", false);

    if (options[FLAG_EMIT_PARSETREE].count)
        mibl_s7_set_flag("*mibl-emit-parsetree*", true);
    else
        mibl_s7_set_flag("*mibl-emit-parsetree*", false);

    if (options[FLAG_EMIT_S7].count)
        mibl_s7_set_flag("*mibl-emit-s7*", true);
    else
        mibl_s7_set_flag("*mibl-emit-s7*", false);

    if (options[FLAG_EMIT_WSS].count)
        mibl_s7_set_flag("*mibl-emit-wss*", true);
    else
        mibl_s7_set_flag("*mibl-emit-wss*", false);

    if (options[FLAG_EMIT_RESULT].count)
        mibl_s7_set_flag("*mibl-emit-result*", true);
    else
        mibl_s7_set_flag("*mibl-emit-result*", false);

    if (options[FLAG_EMIT_PKGS].count)
        mibl_s7_set_flag("*mibl-emit-pkgs*", true);
    else
        mibl_s7_set_flag("*mibl-emit-pkgs*", false);

    if (options[FLAG_CLEAN].count)
        mibl_s7_set_flag("*mibl-clean-all*", true);
    else
        mibl_s7_set_flag("*mibl-clean-all*", false);
    if (options[FLAG_CLEAN_MIBL].count)
        mibl_s7_set_flag("*mibl-clean-mibl*", true);
    else
        mibl_s7_set_flag("*mibl-clean-mibl*", false);
    if (options[FLAG_CLEAN_S7].count)
        mibl_s7_set_flag("*mibl-clean-s7*", true);
    else
        mibl_s7_set_flag("*mibl-clean-s7*", false);

    /* --flags sets vars defined above, may be used to define ad-hoc
       flags (where the scm src will not permanently refer to the
       flag, e.g. (if *mibl-foo*)
     */
    if (options[OPT_FLAGS].count > 0) {
        UT_string *flag;
        utstring_new(flag);

        char *token, *sep = ",";
        log_debug("--flags arg: %s", options[OPT_FLAGS].argument);
        token = strtok((char*)options[OPT_FLAGS].argument, sep);
        log_debug("--flags tok 1: %s", token);
        while( token != NULL ) {
            log_debug("--flags token: %s", token);
            utstring_renew(flag);
            utstring_printf(flag, "*mibl-%s*", token);
            mibl_s7_set_flag(utstring_body(flag), true);
            token = strtok(NULL, sep);
        }
        utstring_free(flag);
    }
}

void _print_version(void) {
    printf("FIXME: version id\n");
}

void _print_usage(void) {
    printf("Usage:\t$ bazel run @obazl//convert [flags, options]\n");
    printf("Options:\n");
    printf("\t-m, --main <arg>"
           "\tPath to script containing -main routine. (REQUIRED)\n");
    printf("\n");
    printf("Flags:\n");
    printf("  Emit flags control file writing.\n");
    printf("\t--emit-wss\t\tSet var *mibl-emit-wss*; default script writes WS files.\n");
    printf("\t--emit-pkgs\t\tSet var *mibl-emit-pkgs; default script writes PKG files.\n");

    printf("    Formats: mibl = human-readable; s7=scheme-readable\n");
    printf("\t--emit-mibl\t\tSet var *mibl-emit-mibl*; default script writes WORKSPACE.mibl or PKG.mibl files.\n");
    printf("\t--emit-s7\t\tSet var *mibl-emit-s7*; default script writes WORKSPACE.s7 or PKG.s7 files.\n");

    /* printf("\t--clean\t\t\tSet var *mibl-clean*; default script removes *.mibl and *.s7 files.\n"); */
    /* printf("\t--clean-mibl\t\tSet var *mibl-clean-mibl*; default script removes *.mibl files.\n"); */
    /* printf("\t--clean-s7\t\tSet var *mibl-clean-s7*; default script removes *.s7 files.\n"); */

    printf("\n");
    printf("  Show flags control output to stdout.\n");
    printf("\t--show-config\t\tPrint configuration to stdout and exit.\n");
    printf("\t--show-mibl\t\tPrint mibl to stdout.\n");
    printf("\t--show-parsetree\tPrint parsetree to stdout and exit.\n");
    printf("\t--show-traversal\tPrint statistics on traversal.\n");

    printf("\n");
    printf("\t-d, --debug\t\tEnable all debugging flags.\n");
    printf("\t-t, --trace\t\tEnable trace flags.\n");
    printf("\t-v, --verbose\t\tEnable verbosity. Repeatable.\n");
    printf("\t-q, --quiet\t\tSuppress stdout/stderr.\n");
    printf("\t--version\t\tShow version Id.\n");
    printf("\n");
    printf("INI file: $XDG_CONFIG_HOME/miblrc\n");


    printf("\n");
}

static struct option options[] = {
    /* 0 */
    [OPT_MAIN] = {.long_name="main",.short_name='m',
                  .flags=GOPT_ARGUMENT_REQUIRED | GOPT_REPEATABLE},
    [OPT_WS] = {.long_name="workspace",.short_name='w',
                .flags=GOPT_ARGUMENT_REQUIRED},
    [OPT_FLAGS] = {.long_name="flags",
                   .flags=GOPT_ARGUMENT_REQUIRED},

    [FLAG_HELP] = {.long_name="help",.short_name='h',
                   .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_CONFIG] = {.long_name="show-config",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_MIBL] = {.long_name="show-mibl",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_PARSETREE] = {.long_name="show-parsetree",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_SHOW_TRAVERSAL] = {.long_name="show-traversal",
                             .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG] = {.long_name="debug",.short_name='d',
                    .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},

    [FLAG_DEBUG_LOAD_PROJECT] = {.long_name="debug-load-project",
                                 .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_PPX] = {.long_name="debug-ppx",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG_S7] = {.long_name="debug-s7",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_CLEAN] = {.long_name="clean",
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_CLEAN_MIBL] = {.long_name="clean-mibl",
                         .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_CLEAN_S7] = {.long_name="clean-s7",
                         .flags=GOPT_ARGUMENT_FORBIDDEN},

    [FLAG_EMIT_MIBL] = {.long_name="emit-mibl",
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_PARSETREE] = {.long_name="emit-parsetree",
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_S7] = {.long_name="emit-s7",
                      .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_WSS] = {.long_name="emit-wss",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_PKGS] = {.long_name="emit-pkgs",
                       .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_EMIT_RESULT] = {.long_name="emit-result",
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

int main(int argc, char **argv, char **envp)
{
    argc = gopt(argv, options);
    (void)argc;
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

    if (options[FLAG_DEBUG].count) {
#if defined(DEBUG_TRACE)
        mibl_debug = true;
#endif
    }

    if (options[FLAG_DEBUG_LOAD_PROJECT].count) {
#if defined(DEBUG_TRACE)
        mibl_debug_mibl_crawl = true;
#else
        log_error("--debug-load-project requires debug build, -c dbg");
        exit(EXIT_FAILURE);
#endif
    }

    if (options[FLAG_TRACE].count) {
        /* printf("trace ct: %d\n", options[FLAG_TRACE].count); */
#if defined(DEBUG_TRACE)
        mibl_trace = true;
#endif
    }

    if (options[FLAG_SHOW_TRAVERSAL].count) {
        mibl_show_traversal = true;
    }

    struct mibl_config_s *mibl_config
        = mibl_s7_init(NULL, // "//obazl/scm",
                       options[OPT_WS].argument);

    if (_update_mibl_config(options, mibl_config)) exit(EXIT_FAILURE);

    _update_s7_globals(options);

    if (options[FLAG_SHOW_CONFIG].count) {
        show_bazel_config();
        show_mibl_config();
        show_s7_config();

        /* dump env vars: */
        /* for (char **env = envp; *env != 0; env++) { */
        /*     char *thisEnv = *env; */
        /*     printf("%s\n", thisEnv); */
        /* } */
        exit(EXIT_SUCCESS);
    }

    mibl_s7_run(options[OPT_MAIN].argument, options[OPT_WS].argument);

    if (verbose)
        log_info("script exit...");
    return 0;

    /* if (exit_on_error) { */
    /*     s7_define_variable(s7, "*exit-on-error*", s7_t(s7)); */
    /* } else { */
    /*     s7_define_variable(s7, "*exit-on-error*", false); */
    /* } */
    /* printf("*exit-on-error*? %d\n", */
    /*        (s7_t(s7) == s7_name_to_value(s7, "*exit-on-error*"))); */

}
