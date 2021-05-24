#include <ctype.h>
#include <errno.h>
#include <libgen.h>
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>             /* PATH_MAX */
#endif
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "ini.h"
#include "log.h"
#include "utstring.h"


#include "obazl_config.h"

UT_string *exec_root;
UT_string *runfiles_root;
UT_string *proj_root;
UT_string *obazl_d;

UT_string *runtime_data_dir;

bool ini_error = false;
UT_string *obazl_ini_path;
const char *obazl_ini_file = ".obazlrc";


int obazl_configure(char *_exec_root)
{
    log_debug("obazl_configure");
    char *_proj_root = getenv("BUILD_WORKSPACE_DIRECTORY");
    if (_proj_root == NULL) {
        log_error("Env var 'BUILD_WORKSPACE_DIRECTORY' not found. This program must be run in a Bazel project.");
        exit(EXIT_FAILURE);
    }
    /* log_debug("BUILD_WORKSPACE_DIRECTORY: %s", bazel_proj_root); */

    utstring_new(exec_root);
    utstring_printf(exec_root, "%s", _exec_root);
    /* log_debug("exec_root: %s", utstring_body(exec_root)); */

    utstring_new(runfiles_root);
    utstring_printf(runfiles_root, "%s", getcwd(NULL, 0));
    /* log_debug("runfiles_root: %s", utstring_body(runfiles_root)); */

    utstring_new(proj_root);
    utstring_printf(proj_root, "%s", _proj_root);
    /* log_debug("proj_root: %s", utstring_body(proj_root)); */

    /* .obazl.d hidden directory */
    utstring_new(obazl_d);
    utstring_printf(obazl_d, "%s/%s", utstring_body(proj_root), ".obazl.d");
    /* log_debug("obazl_d: %s", utstring_body(obazl_d)); */
    /* log_debug("mkdir %s", utstring_body(obazl_d)); */
    int rc = mkdir(utstring_body(obazl_d), S_IRWXU | S_IRGRP | S_IWGRP);
    if (rc != 0) {
        if (errno != EEXIST) {
            perror(utstring_body(obazl_d));
            log_error("mkdir error");
        }
    }

    /* .obazlrc config file */
    utstring_new(obazl_ini_path);
    utstring_printf(obazl_ini_path, "%s/%s", utstring_body(proj_root), obazl_ini_file);

    /* rc = access(utstring_body(obazl_ini_path), R_OK); */
    /* if (rc) { */
    /*     log_warn("Config file %s not found.", utstring_body(obazl_ini_path)); */
    /* } else { */
    /*     ini_error = false; */
    /*     utarray_new(obazl_config.src_dirs, &ut_str_icd); */
    /*     utarray_new(obazl_config.watch_dirs, &ut_str_icd); */
    /*     rc = ini_parse(utstring_body(obazl_ini_path), config_handler, &obazl_config); */
    /*     /\* log_debug("ini_parse rc: %d", rc); *\/ */
    /*     if (rc < 0) { */
    /*         //FIXME: deal with missing .obazl */
    /*         log_fatal("Can't load ini file: %s", utstring_body(obazl_ini_path)); */
    /*         return -1; */
    /*     } */
    /*     if (ini_error) { */
    /*         log_error("Error parsing ini file"); */
    /*         exit(EXIT_FAILURE); */
    /*     /\* } else { *\/ */
    /*     /\*     log_debug("Config loaded from %s", utstring_body(obazl_ini_path)); *\/ */
    /*     } */
    /* } */
}
