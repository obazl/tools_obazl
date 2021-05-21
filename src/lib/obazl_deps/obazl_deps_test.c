#include <errno.h>
#include <fcntl.h>
/* #include <getopt.h> */
#include <libgen.h>

#if INTERFACE
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#endif

#include <unistd.h>             /* getopt, close, getcwd */

#include "log.h"
#include "utarray.h"
#include "uthash.h"
#include "utstring.h"

#include "obazl_deps_test.h"

int main(int argc, char *argv[])
{
    char *proj_root = getenv("BUILD_WORKSPACE_DIRECTORY");
    /* log_debug("BUILD_WORKSPACE_DIRECTORY: %s", proj_root); */


    /* char cwd[FILENAME_MAX]; */
    /* if (getcwd(cwd, sizeof(cwd)) != NULL) { */
    /*     log_debug("Current working dir: %s", cwd); */
    /* } */
    /* log_debug("changing dir to %s", proj_root); */
    int rc = chdir(proj_root);
    if (rc) {
        perror("chdir");
        log_fatal("Unable to chdir to projroot %", proj_root);
        exit(EXIT_FAILURE);
    }

    int opt;

    while ((opt = getopt(argc, argv, "h")) != -1) {
        switch (opt) {
        case 1:
            break;
        case 'h':
            log_info("Help: ");
            exit(EXIT_SUCCESS);
        default:
            log_error("Usage: %s ... [TODO]", argv[0]);
            /* exit(EXIT_FAILURE); */
        }
    }

    obzl_deps_parse_file(".obazl.d/codept.deps");

    /* struct module_s *module, *tmp; */
    /* HASH_ITER(hh, codept_modules, module, tmp) { */
    /*     log_debug(""); */
    /*     log_debug("module->name: %s", module->name); */
    /*     log_debug("module->type: %d", module->type); */
    /*     if (module->type == M_LOCAL) { */
    /*         log_debug("module->structfile: %s", module->structfile); */
    /*         log_debug("module->sigfile: %s", module->sigfile); */
    /*     } else { */
    /*         log_debug("module->lib: %s", module->lib); */
    /*     } */
    /* } */

    struct filedeps_s *fdeps, *tmpfdeps;
    char **p = NULL;
    HASH_ITER(hh, codept_filedeps, fdeps, tmpfdeps) {
        log_debug("");
        log_debug("fdeps->name: %s", fdeps->name);
        /* log_debug("fdeps->type: %d", fdeps->type); */
        log_debug("fdeps->deps:");
        while ( (p=(char**)utarray_next(fdeps->deps, p))) {
            log_debug("\t%s",*p);
        }
    }

    return 0;
}
