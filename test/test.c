#include <errno.h>
#include <fcntl.h>
#include <libgen.h>

#if INTERFACE
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#endif

#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#if INTERFACE
#include "utstring.h"
#endif

#include "libmetadune.h"

#include "test.h"


#if EXPORT_INTERFACE
struct logging {
    int verbosity;
    int log_level;
    int parse_verbosity;
    int parse_log_level;
    int lex_verbosity;
    int lex_log_level;
    bool quiet;
    bool log_color;
};
#endif

EXPORT struct logging logger;

UT_string *dune_file;
/* UT_string *meta_file; */

bool watch = false;

char launchdir[PATH_MAX];
int launchdir_len = 0;

char cwd[PATH_MAX];
int cwd_len = 0;

int main(int argc, char *argv[])
{
    int opt;
    bool _quiet = false;

    char *_dune_file = NULL;
    /* utstring_new(meta_file); */
    /* UT_string *watch_arg; */
    /* utstring_new(watch_arg); */

    /* FIXME: add --version */
    while ((opt = getopt(argc, argv, "d:m:qpw:xhv")) != -1) {
        switch (opt) {
        /* case 'c': */
        /*     log_color = true; */
        /*     break; */
        case 'd':
            _dune_file = strdup(optarg);
            /* utstring_printf(dune_file, "%s", optarg); */
            break;
        /* case 'm': */
        /*     utstring_printf(meta_file, "%s", optarg); */
        /*     break; */
        case 'q':
            _quiet = true;
            break;
        case 'p':
            logger.parse_verbosity++;
            break;
        /* case 'w': */
        /*     watch = true; */
        /*     utstring_printf(watch_arg, "%s", optarg); */
        /*     break; */
        case 'x':
            logger.lex_verbosity++;
            break;
        case 'v':
            logger.verbosity++;
            break;
        case 'h':
            log_info("Help: ");
            exit(EXIT_SUCCESS);
        default:
            log_error("Usage: %s [-d] [dunefile]", argv[0]);
            exit(EXIT_FAILURE);
        }
    }

    if (getcwd(launchdir, sizeof(launchdir)) != NULL) {
        /* log_debug("Launch working dir: %s", launchdir); */
    }

    char *_proj_root = getenv("BUILD_WORKSPACE_DIRECTORY");
    if (_proj_root != NULL) {
        /* we were launched by 'bazel run' */
        int rc = chdir(_proj_root);
        if (rc) {
            perror("chdir");
            log_fatal("Unable to chdir to launch root %", _proj_root);
            exit(EXIT_FAILURE);
        }
    }

    log_set_level(LOG_TRACE);

    const char *logfile = ".log";
    FILE *logfp;
    if (_quiet) {
        logfp = fopen(logfile, "w");
        int errnum;
        if (logfp == NULL) {
            errnum = errno;
            fprintf(stderr, "fopen failure for %s", logfile);
            log_error("Value of errno: %d", errnum);
            log_error("fopen error %s", strerror( errnum ));
            exit(EXIT_FAILURE);
        }
        log_add_fp(logfp, LOG_TRACE);
        log_set_quiet(_quiet);
    }

    log_debug("launchdir: %s", launchdir);
    log_debug("BUILD_WORKSPACE_DIRECTORY: %s", _proj_root);

    if (_dune_file == NULL) {
        log_error("-d <dunefile> must be provided.");
        exit(EXIT_FAILURE);
    }
    if (_dune_file[0] == '/') {
        log_error("path to dune file must be relative: %s", _dune_file);
        exit(EXIT_FAILURE);
    }

    log_debug("_dune_file %s", _dune_file);

    if (getcwd(cwd, sizeof(cwd)) != NULL) {
        log_debug("Current working dir: %s", cwd);
        cwd_len = strlen(cwd);
    }

    /* if (watch) { */
    /*     obazl_watch_controller(utstring_body(watch_arg)); */
    /* } */
    /* exit(EXIT_FAILURE);         /\* debugging *\/ */


    /* apps must configure in this order: */
    /* md_configure(dirname(argv[0])); */

    /* md_config_dune(); */
    /* lua may be used by multiple modules (dune, meta, bazel), so we configure it separately */
    /* obazl_config_lua("dune.lua"); */

    /* configure app-specific lua file */
    /* UT_string *lua_file; */
    /* utstring_new(lua_file); */
    /* utstring_printf(lua_file, "%s/%s", utstring_body(proj_root), "test.lua"); */

    /* obazl_init_lua(); // lua_file);   /\* runs user-provided lua 'init' fn *\/ */

    /* l_init();           /\* initial write build files *\/ */

    /* if (utstring_len(dune_file) > 0) { */
    log_info("obazl_dune version: %s", obazl_dune_version());
    /* log_info("proj root: %s", utstring_body(proj_root)); */
    utstring_new(dune_file);
    /* utstring_printf(dune_file, "%s/%s", utstring_body(proj_root), _dune_file); */
    utstring_printf(dune_file, "%s", _dune_file);
    log_info("parsing dune file %s", utstring_body(dune_file));

    struct obazl_dune_package_s *dune_ast = obazl_dune_parse_file(utstring_body(dune_file));

    /* log_set_level(LOG_DEBUG); */
    /* dump_dune_pkg(0, dune_ast); */
    /* dune_emit_build_bazel(dune_ast, "@opam", "lib"); */
    log_info("dune DONE");

    fclose(logfp);

    /* if (utstring_len(meta_file) > 0) { */
    /*     log_info("obazl_meta version: %s", obazl_dune_version()); */
    /*     struct obazl_meta_package *meta_ast = obazl_meta_parse_file(utstring_body(meta_file)); */
    /*     log_set_level(LOG_DEBUG); */
    /*     dump_pkg(0, meta_ast); */
    /*     /\* meta_emit_build_bazel(meta_ast, "@opam", "lib"); *\/ */
    /*     log_info("meta DONE"); */
    /* } */

    return 0;
}
