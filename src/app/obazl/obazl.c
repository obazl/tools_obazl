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

#include "obazl.h"


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
UT_string *meta_file;

bool watch = false;

const char cwd[PATH_MAX];
int cwd_len = 0;

int main(int argc, char *argv[])
{
    int opt;

    utstring_new(dune_file);
    utstring_new(meta_file);
    UT_string *watch_arg;
    utstring_new(watch_arg);

    /* FIXME: add --version */
    while ((opt = getopt(argc, argv, "d:m:qpw:xhv")) != -1) {
        switch (opt) {
        /* case 'c': */
        /*     log_color = true; */
        /*     break; */
        case 'd':
            utstring_printf(dune_file, "%s", optarg);
            break;
        case 'm':
            utstring_printf(meta_file, "%s", optarg);
            break;
        case 'q':
            logger.quiet = true;
            break;
        case 'p':
            logger.parse_verbosity++;
            break;
        case 'w':
            watch = true;
            utstring_printf(watch_arg, "%s", optarg);
            break;
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
            log_error("Usage: %s [-m] [metafile]", argv[0]);
            exit(EXIT_FAILURE);
        }
    }

    /* if ( (utstring_len(dune_file) == 0) */
    /*      && (utstring_len(meta_file) == 0)){ */
    /*     log_error("One of -d or -m must be provided.", argv[0]); */
    /*     exit(EXIT_FAILURE); */
    /* } */

    switch(logger.lex_verbosity) {
    case 0:
        /* quiet */
        /* lex_log_level = LOG_WARN; */
        /* break; */
    case 1:
        logger.lex_log_level = LOG_INFO;
        break;
    case 2:
        logger.lex_log_level = LOG_DEBUG;
        break;
    case 3:
        logger.lex_log_level = LOG_TRACE;
        break;
    default:
        logger.lex_log_level = LOG_WARN;
    }

    switch(logger.parse_verbosity) {
    case 0:
        /* quiet */
        /* parse_log_level = LOG_WARN; */
        /* break; */
    case 1:
        logger.parse_log_level = LOG_INFO;
        break;
    case 2:
        logger.parse_log_level = LOG_DEBUG;
        break;
    case 3:
        logger.parse_log_level = LOG_TRACE;
        break;
    default:
        logger.parse_log_level = LOG_WARN;
    }

    switch(logger.verbosity) {
    case 0:
        /* quiet */
    case 1:
        logger.log_level = LOG_INFO;
        break;
    case 2:
        logger.log_level = LOG_DEBUG;
        break;
    case 3:
        logger.log_level = LOG_TRACE;
        break;
    default:
        logger.log_level = LOG_WARN;
    }

    log_set_quiet(logger.quiet);

    /* if (getcwd(cwd, sizeof(cwd)) != NULL) { */
    /*     log_debug("Current working dir: %s", cwd); */
    /*     cwd_len = strlen(cwd); */
    /* } */

    if (watch) {
        control_watcher(utstring_body(watch_arg));
    }
    exit(EXIT_FAILURE);         /* debugging */

    if (utstring_len(dune_file) > 0) {
        log_info("obazl_dune version: %s", obzl_dune_version());
        log_info("parsing dune file %s", utstring_body(dune_file));
        struct obzl_dune_package_s *dune_ast = obzl_dune_parse_file(utstring_body(dune_file));
        /* log_set_level(LOG_DEBUG); */
        dump_dune_pkg(0, dune_ast);
        /* dune_emit_build_bazel(dune_ast, "@opam", "lib"); */
        log_info("dune DONE");
    }

    if (utstring_len(meta_file) > 0) {
        log_info("obazl_meta version: %s", obzl_dune_version());
        struct obzl_meta_package *meta_ast = obzl_meta_parse_file(utstring_body(meta_file));
        log_set_level(LOG_DEBUG);
        dump_pkg(0, meta_ast);
        /* meta_emit_build_bazel(meta_ast, "@opam", "lib"); */
        log_info("meta DONE");
    }

    return 0;
}
