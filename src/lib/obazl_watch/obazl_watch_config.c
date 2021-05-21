#include <errno.h>
#include <fcntl.h>
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>             /* PATH_MAX */
#endif
#include <stdlib.h>
#include <string.h>
#include <unistd.h>             /* access, getopt, close, getcwd */

#include <sys/types.h>
#include <sys/stat.h>

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "ini.h"
#include "log.h"
#include "utarray.h"
#include "uthash.h"
#include "utstring.h"

#include "obazl_watch_config.h"

#define OBAZL_VERSION "0.1.0"

/* NOTE: this file is used by both server and client, separately. */
/* FIXME: separate out what is genuinely common to both, like paths, fifos, etc. */

#if INTERFACE
struct lib_s {
    char *name;
    char *path;
};

struct configuration_s {
    char *obazl_version;
    int libct;
    UT_array *src_dirs;         /* string list; used by fileseq to get src_files */
    UT_array *watch_dirs;       /* string list */
    struct lib_s *ocamllibs[10]; /* is 10 enough? */
    struct lib_s *coqlibs[10]; /* is 10 enough? */
};
#endif

struct configuration_s obazl_config = {.obazl_version = OBAZL_VERSION, .libct = 0};

char *opam_prefix;
size_t opam_prefix_len;

/* opam_dirs is effectively read-only. we initialize it before we
   launch the daemon, and we do not monitor the OPAM repo - users who
   change the repo are responsible for restarting the obazl watch
   daemon. */
UT_array *opam_dirs;

/* proj_dirs is volatile. it will change when the user adds or removes
   directories.
 */
UT_array *proj_dirs;

/* src_files is volatile. it will change when the user adds or removes
   files.
 */
UT_array *src_files;            /* FIXME: put this in configuration_s? */

const char * const fifo_to_server_name = "fifo.to_server";
UT_string *fifo_to_server_path = NULL;
int fifo_to_server_fd;

const char * const fifo_to_client_name = "fifo.to_client";
UT_string *fifo_to_client_path = NULL;
int fifo_to_client_fd;

UT_string *proj_root;
UT_string *obazl_d;
UT_string *obazl_watch_log;
FILE *obazl_watch_log_fp;

/* static const char codept_params[] = ".obazl.d/codept.params"; */
UT_string *codept_args_file;
const char *codept_args_filename = "codept.args";

/* static const char codept_depends[] = ".obazl.d/codept.depends"; */
UT_string *codept_deps_file;
const char *codept_deps_filename = "codept.deps";

char *codept_cmd;

bool ini_error = false;
UT_string *obazl_ini_path;
const char *obazl_ini_file = ".obazlrc";

int config_handler(void* config, const char* section, const char* name, const char* value)
{
    /* log_debug("config_handler section %s: %s=%s", section, name, value); */
    struct configuration_s *pconfig = (struct configuration_s*)config;

    #define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0

    if (MATCH("obazl", "version")) {
        log_debug("obazl version: %s", value);
        return 1;
    }

    if (MATCH("srcs", "dirs")) {
        /* log_debug("section: srcs; entry: dirs"); */
        /* log_debug("\t%s", value); */
        char *token, *sep = " ,\t";
        token = strtok((char*)value, sep);
        while( token != NULL ) {
            if (token[0] == '/') {
                log_error("Ini file: 'dir' values in section 'srcs' must be relative paths: %s", token);
                ini_error = true;
                return 0;
            } else {
                /* log_debug("pushing src dir: %s", token); */
                utarray_push_back(pconfig->src_dirs, &token);
                token = strtok(NULL, sep);
            }
        }
        return 1;
    }

    if (MATCH("watch", "dirs")) {
        /* log_debug("section: watch; entry: dirs"); */
        /* log_debug("\t%s", value); */
        char *token, *sep = " ,\t";
        token = strtok((char*)value, sep);
        while( token != NULL ) {
            if (token[0] == '/') {
                log_error("Ini file: 'dir' values in section 'watch' must be relative paths: %s", token);
                ini_error = true;
                return 0;
            } else {
                /* log_debug("pushing watch dir: %s", token); */
                utarray_push_back(pconfig->watch_dirs, &token);
                token = strtok(NULL, sep);
            }
        }
        return 1;
    }

    /* if (MATCH("obazl", "repos")) { */
    /*     resolve_repos((char*)value); */
    /* } */

    /* if (MATCH("repos", "coq")) { */
    /*     resolve_coq_repos((char*)value); */
    /* } */

    /* if (MATCH("repos", "ocaml")) { */
    /*     resolve_ocaml_repos((char*)value); */
    /* } */

    /* if ( strncmp(section, "repo:", 5) == 0 ) { */
    /*     /\* printf("REPO section: %s (%s = %s)\n", section, name, value); *\/ */
    /*     char *the_repo = &section[5]; */

    /*     char *repo_dir = get_workspace_dir(the_repo); */
    /*     printf("repo: %s -> %s\n", the_repo, repo_dir); */

    /*     /\* tmp_repo = NULL; *\/ */
    /*     /\* HASH_FIND_STR(repo_map, the_repo, tmp_repo);  /\\* already in the hash? *\\/ *\/ */
    /*     /\* if (tmp_repo) { *\/ */
    /*     /\*     printf("%s -> %s\n", tmp_repo->name, tmp_repo->base_path); *\/ */
    /*     /\* } else { *\/ */
    /*     /\*     fprintf(stderr, "No WS repo found for '%s' listed in .obazlrc\n", the_repo); *\/ */
    /*     /\*     exit(EXIT_FAILURE); *\/ */
    /*     /\* } *\/ */
    /* } */

    /* if ( strcmp(section, "coqlibs") == 0 ) { */
    /*     struct lib_s *cl = calloc(1, sizeof *cl); */
    /*     cl->name = strdup(name); */
    /*     cl->path = strdup(value); */
    /*     pconfig->coqlibs[pconfig->libct] = cl; */
    /*     /\* printf("loaded lib %d (%p): %s -> %s\n", *\/ */
    /*     /\*        pconfig->libct, *\/ */
    /*     /\*        pconfig->coqlibs[pconfig->libct], *\/ */
    /*     /\*        pconfig->coqlibs[pconfig->libct]->name, *\/ */
    /*     /\*        pconfig->coqlibs[pconfig->libct]->path); *\/ */
    /*     pconfig->libct++; */
    /* } */
    return 1;
}

void obazl_config_fifos(void)
{
    log_debug("obazl_config_fifos");

    int rc;
    /* client_to_server fifo */
    int ct = 0;
 retry_mkfifo_server:
    rc = mkfifo(utstring_body(fifo_to_server_path), S_IRUSR | S_IWUSR ); // | S_IRGRP );
    if (rc < 0) {
        if (errno == EEXIST) {
            /* config only runs on startup, so we can remove */
            log_warn("EEXIST: %s; replacing", utstring_body(fifo_to_server_path));
            rc = unlink(utstring_body(fifo_to_server_path));
            if (rc != 0) {
                log_error("unlink %s failed", utstring_body(fifo_to_server_path));
                exit(EXIT_FAILURE);
            }
            if (ct < 3) {
                ct++;
                goto retry_mkfifo_server;
            } else {
                log_fatal("%s already exists, unable to delete", utstring_body(fifo_to_server_path));
                exit(EXIT_FAILURE);
            }
        } else {
            perror(utstring_body(fifo_to_server_path));
            log_error("mkfifo failed for %s", utstring_body(fifo_to_server_path));
            exit(EXIT_FAILURE);
        }
    } else {
        log_debug("success: mkfifo for %s", utstring_body(fifo_to_server_path));
    }

    /* log_debug("opening fifo_to_server_path for reading"); */
    /* fifo_to_server_fd = open(utstring_body(fifo_to_server_path), O_RDONLY); */
    /* if (fifo_to_server_fd < 0) { */
    /*     perror(utstring_body(fifo_to_server_path)); */
    /*     log_fatal("failed to open fifo_to_server_path for reading"); */
    /*     unlink(utstring_body(fifo_to_server_path)); */
    /*     exit(EXIT_FAILURE); */
    /* } */

    /* server_to_client fifo: write only */
    ct = 0;
 retry_mkfifo_client:
    rc = mkfifo(utstring_body(fifo_to_client_path), S_IRUSR | S_IWUSR); // | S_IWGRP );
    if (rc < 0) {
        if (errno == EEXIST) {
            /* config only runs on startup, so we can remove */
            log_warn("EEXIST: %s; replacing", utstring_body(fifo_to_client_path));
            rc = unlink(utstring_body(fifo_to_client_path));
            if (rc != 0) {
                log_error("unlink %s failed", utstring_body(fifo_to_client_path));
                exit(EXIT_FAILURE);
            }
            if (ct < 3) {
                ct++;
                goto retry_mkfifo_client;
            } else {
                log_fatal("%s already exists, unable to delete", utstring_body(fifo_to_client_path));
                exit(EXIT_FAILURE);
            }
        } else {
            perror(utstring_body(fifo_to_client_path));
            log_error("mkfifo failed for %s", utstring_body(fifo_to_client_path));
            exit(EXIT_FAILURE);
        }
        /* if (errno != EEXIST) { */
        /*     perror(utstring_body(fifo_to_client_path)); */
        /*     log_error("mkdir error"); */
        /* } else { */
        /*     /\* errno == ??? *\/ */
        /*     log_error("fifo exists"); */
        /* } */
    } else {
        log_debug("success: mkfifo for %s", utstring_body(fifo_to_client_path));
    }

    /* log_debug("opening fifo_to_client_path for writing"); */
    /* fifo_to_client_fd = open(utstring_body(fifo_to_client_path), O_WRONLY); */
    /* if (fifo_to_client_fd < 0) { */
    /*     perror(utstring_body(fifo_to_client_path)); */
    /*     log_fatal("failed to open fifo_to_client_path for reading"); */
    /*     unlink(utstring_body(fifo_to_client_path)); */
    /*     exit(EXIT_FAILURE); */
    /* } */
}

/**
   client code needs only what is necessary to communicate with server - paths etc.
 */
int obazl_watch_configure_client()
{
    log_debug("obazl_watch_configure_client");

    return _watch_configure_common();
}

int obazl_watch_configure_server()
{
    log_debug("obazl_watch_configure_server");

    _watch_configure_common();

    /* experimental: lua */
    utstring_new(lua_file);
    utstring_printf(lua_file, "%s/%s", utstring_body(obazl_d), lua_file_name);
    log_debug("lua_file: %s", utstring_body(lua_file));

    /* opam prefix */
    char *cmd = "opam var prefix";
    char *pfx = run_cmd(cmd);
    if (pfx == NULL) {
        log_error("FAIL: run_cmd(%s)\n", cmd);
        exit(EXIT_FAILURE);
    }
    opam_prefix = strndup(pfx, strlen(pfx));
    opam_prefix_len = strlen(opam_prefix);

    /* codept cmd absolute path, using current switch */
    cmd = "opam var bin";
    char result[BUFSIZ];
    result[0] = '\0';
    char *codept_path;
    codept_path = run_cmd(cmd);
    if (codept_path == NULL) {
        log_error("FAIL: run_cmd(%s)\n", cmd);
        exit(EXIT_FAILURE);
    /* } else { */
    /*     log_debug("opam var bin: %s", result); */
    }
    strncat(result, codept_path, strlen(codept_path));
    strncat(result, "/codept", 7);
    rc = access(result, R_OK);
    if (rc) {
        log_error("codept executable not acccesible at %s", result);
        exit(EXIT_FAILURE);
    /* } else { */
    /*     log_debug("found codept executable at %s", result); */
    }
    codept_cmd = strndup(result, strlen(result));
    log_debug("codept_cmd: %s", codept_cmd);

    /* codept args and deps files */
    utstring_new(codept_args_file);
    utstring_printf(codept_args_file, "%s/%s", utstring_body(obazl_d), codept_args_filename);
    /* log_debug("codept_args_file: %s", utstring_body(codept_args_file)); */

    utstring_new(codept_deps_file);
    utstring_printf(codept_deps_file, "%s/%s", utstring_body(obazl_d), codept_deps_filename);
    /* log_debug("codept_deps_file: %s", utstring_body(codept_deps_file)); */

    utarray_new(dirty_fdeps, &ut_str_icd);   /* obazl_codept_parser.c */

    // write codept.args file with -L for opam dirs?

    /* project directories (FIXME: not needed?) */
    /* utarray_new(proj_dirs, &ut_str_icd); */
    /* dirseq(utstring_body(proj_root), proj_dirs); */
    /* log_debug("proj_dirs ct: %d", utarray_len(proj_dirs)); */
    /* while ( (p=(char**)utarray_next(proj_dirs, p))) { */
    /*     printf("\t%s\n", *p); */
    /* } */
    /* utarray_free(proj_dirs); */

    // append to codept.args file with -I for proj dirs?

    /* Source files */
    utarray_new(src_files, &ut_str_icd);

    return 1;
}

LOCAL int _watch_configure_common()
{
    log_debug("obazl_watch_configure_common");

    char **p = NULL;            /* debuggin */

    char *_proj_root = getenv("BUILD_WORKSPACE_DIRECTORY");
    if (_proj_root == NULL) {
        log_error("Env var 'BUILD_WORKSPACE_DIRECTORY' not found. This program must be run in a Bazel project.");
        exit(EXIT_FAILURE);
    }
    /* log_debug("BUILD_WORKSPACE_DIRECTORY: %s", bazel_proj_root); */

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

    rc = access(utstring_body(obazl_ini_path), R_OK);
    if (rc) {
        log_warn("Config file %s not found.", utstring_body(obazl_ini_path));
    } else {
        ini_error = false;
        utarray_new(obazl_config.src_dirs, &ut_str_icd);
        utarray_new(obazl_config.watch_dirs, &ut_str_icd);
        rc = ini_parse(utstring_body(obazl_ini_path), config_handler, &obazl_config);
        /* log_debug("ini_parse rc: %d", rc); */
        if (rc < 0) {
            //FIXME: deal with missing .obazl
            log_fatal("Can't load ini file: %s", utstring_body(obazl_ini_path));
            return -1;
        }
        if (ini_error) {
            log_error("Error parsing ini file");
            exit(EXIT_FAILURE);
        /* } else { */
        /*     log_debug("Config loaded from %s", utstring_body(obazl_ini_path)); */
        }
    }

    /* log file */
    utstring_new(obazl_watch_log);
    utstring_printf(obazl_watch_log, "%s/%s", utstring_body(proj_root), ".obazl.d/obazlw.log");
    log_debug("obazl_watch_log: %s", utstring_body(obazl_watch_log));

    /* fifo paths */
    utstring_new(fifo_to_server_path);
    utstring_printf(fifo_to_server_path, "%s/%s", utstring_body(obazl_d), fifo_to_server_name);
    log_debug("fifo_to_server_path: %s", utstring_body(fifo_to_server_path));

    utstring_new(fifo_to_client_path);
    utstring_printf(fifo_to_client_path, "%s/%s", utstring_body(obazl_d), fifo_to_client_name);
    log_debug("fifo_to_client_path: %s", utstring_body(fifo_to_client_path));

    return 1;
}
