#include <assert.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>              /* open() */
#include <libgen.h>             /* for basename() */
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#include <spawn.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>

#include "log.h"
#include "utarray.h"
#include "uthash.h"
#include "utstring.h"

#include "obazl_codept.h"

int errnum;
int rc;

/* BUFSIZE = 1024; */

/* char basedir[PATH_MAX]; */
char coqlib[PATH_MAX];

#if INTERFACE
struct codept_src {
    char dir[PATH_MAX];        /* key (string is WITHIN the structure) */
    UT_hash_handle hh;
};
#endif

struct codept_src *codept_srcs = NULL;

/*
  parse codept-generated dependency line and update data structs
 */
void process_codept_line(char *line)
{
    /* printf("%s", line); */
    strtok(line, " ");         /* ignore first */
    /* char *target = strtok(line, " "); */
    /* printf("target: %s\n", target); */
    strtok(NULL, " ");
    char *dep = strtok(NULL, " ");
    while (dep) {
        /* printf("dep: %s\n", dep); */
        dep = strtok(NULL, " ");
    }
}

void run_codept(char *codept_args_file, char *codept_deps_file)
{
    log_debug("running codept");

    /* char cwd[PATH_MAX]; */
    /* if (getcwd(cwd, sizeof(cwd)) != NULL) { */
    /*     log_debug("Current working dir: %s", cwd); */
    /* } */
    /* log_debug("changing dir to %s", utstring_body(proj_root)); */
    rc = chdir(utstring_body(proj_root));
    if (rc) {
        perror("chdir");
        log_fatal("Unable to chdir to projroot %", utstring_body(proj_root));
        exit(EXIT_FAILURE);
    }

    char cmd[PATH_MAX];
    sprintf(cmd, "codept -k -args %s 2> /dev/null", codept_args_file);

    bool keep = true;        /* ??? */

    /* FILE *deps_fp; */
    /* if (keep) { */
    /*     printf("keeping %s\n", codept_deps_file); */
    /*     deps_fp = fopen(codept_deps_file, "w"); */
    /*     if (deps_fp == NULL) { */
    /*         perror(codept_deps_file); */
    /*         log_fatal("FAIL: run_codept fopen(%s, 'w')", codept_deps_file); */
    /*         exit(EXIT_FAILURE); */
    /*     } */
    /* } */
    /* log_debug("opened codept_deps_file %s for writing", codept_deps_file); */

    /* log_debug("codept_args_file: %s", codept_args_file); */
    /* log_debug("codept_deps_file: %s", codept_deps_file); */

    pid_t pid;
    int rc;
    char *argv[] = {
        "codept",
        "-args", codept_args_file,
        NULL};
    extern char **environ;

    posix_spawn_file_actions_t action;
    posix_spawn_file_actions_init(&action);
    posix_spawn_file_actions_addopen (&action, STDOUT_FILENO, codept_deps_file,
                                      O_WRONLY | O_CREAT,
                                      S_IRUSR | S_IWUSR | S_IRGRP );

    // FIXME: get absolute path of codept
    // FIXME: restrict environ

    char *codept_cmd = "/Users/gar/.opam/4.09.0/bin/codept";
    /* log_debug("spawning %s", codept_cmd); */
    rc = posix_spawn(&pid, codept_cmd, &action, NULL, argv, environ);
    if (rc == 0) {
        /* log_debug("posix_spawn child pid: %i\n", pid); */
        if (waitpid(pid, &rc, 0) != -1) {
            if (rc) {
                log_error("codept rc: %d", rc);
                exit(EXIT_FAILURE);
            }
        } else {
            perror("waitpid");
            log_error("run_codept posix_spawn");
        }
    } else {
        /* does not set errno */
        log_fatal("run_codept posix_spawn error rc: %d, %s", rc, strerror(rc));
        exit(EXIT_FAILURE);
    }

    /* FILE *fp; */
    /* if ((fp = popen(cmd, "r+")) == NULL) { */
    /*     printf("Error opening pipe for codept!\n"); */
    /*     exit(-1); */
    /* } */
    /* log_debug("action: %s", cmd); */
    /* char buf[1024]; */
    /* while (fgets(buf, sizeof(buf), fp) != NULL) { */
    /*     if (keep) */
    /*         fputs(buf, deps_fp); */
    /*     process_codept_line(buf); */
    /* } */
    /* rc = pclose(fp); */
    /* if (rc) { */
    /*     exit(-1); */
    /* } */

    /* if (keep) { */
    /*     fflush(deps_fp); */
    /*     fclose(deps_fp); */
    /* } */
}

LOCAL void emit_codept_src_files(FILE *fp, UT_array *src_files)
{
    log_debug("emit_codept_src_files");

    /* fputs("-one-line\n", fp); */
    fputs("-sexp\n", fp);

    char **p = NULL;
    while ( (p=(char**)utarray_next(src_files, p))) {
            /* log_debug("writing codept arg: %s", *p); */

            rc = fputs(*p, fp);
            if ( rc < 1 ) { // success == non-neg int
                errnum = errno;
                perror("emit_codept_src_files");
                /* log_error("fputs fail putting %s to %s", *p, codept_args_file); */
                /* log_error("rc: %d, errno: %d", rc, errno); */
                /* log_error("Error putting %s: %s", *p, strerror( errnum )); */
                fclose(fp);
                exit(1);
            }
            rc = fputs("\n", fp);
            if ( rc < 1) {
                errnum = errno;
                perror("emit_codept_src_files 2");
                /* log_error("fputs fail putting newline to %s", codept_args_file); */
                /* log_error("Value of errno: %d", errno); */
                /* log_error("Error putting newline: %s", strerror( errnum )); */
                fclose(fp);
                exit(1);
            }
    }

    fputs("\n", fp);
}

LOCAL void emit_codept_opam_dirs(FILE *fp, UT_array *opam_dirs)
{
    log_debug("emit_codept_opam_dirs");

    char **p = NULL;
    while ( (p=(char**)utarray_next(opam_dirs, p))) {
        /* log_debug("writing codept arg: %s", *p); */

        /* Warning! '-L' is just for .cmi files, it will miss pkgs
           like Zarith, which does not have zarith.cmi. We need both
           -I and -L. */
        rc = fputs("-L\n", fp);
        rc = fputs(*p, fp);
        /* rc = fputs("\n-I\n", fp); */
        /* rc = fputs(*p, fp); */
        if ( rc < 1 ) { // success == non-neg int
            errnum = errno;
            perror("emit_codept_opam_dirs");
            log_error("emit_codept_opam_dirs 1 fputs");
            fclose(fp);
            exit(1);
        }
        rc = fputs("\n", fp);
        if ( rc < 1) {
            errnum = errno;
            perror("emit_codept_opam_dirs 2");
            log_error("emit_codept_opam_dirs 2 fputs");
            fclose(fp);
            exit(1);
        }
    }

    fputs("\n", fp);
}

void emit_codept_args(UT_string *_codept_args_file, UT_array *opam_dirs, UT_array *src_files)
{
    log_debug("emit_codept_args");

    char *codept_args_file = utstring_body(_codept_args_file);

    FILE *fp;
    int rc;

    fp = fopen(codept_args_file, "w");
    if (fp == NULL) {
        errnum = errno;
        perror(codept_args_file);
        log_error("fopen(%s): %s", codept_args_file, strerror( errnum ));
        exit(1);
    }

    emit_codept_src_files(fp, src_files);

    emit_codept_opam_dirs(fp, opam_dirs);

    // PREPROCS: .mll, .mly, .mlg, etc. - files that require preprocessing
    /* struct preproc *pp, *pptmp; */
    /* HASH_ITER(hh, preprocs, pp, pptmp) { */
    /*     /\* printf("putting %s to %s", cd->dir, codept_args_file); *\/ */
    /*     //FIXME: macro to handle error checking */
    /*     if (pp->mlout) { */
    /*         /\* printf("codept preproc: %s", pp->mlout); *\/ */
    /*         rc = fputs(pp->mlout, fp); */
    /*     } */
    /*     if ( rc < 1 ) { // success == non-neg int */
    /*         errnum = errno; */
    /*         log_error("fputs fail putting %s to %s", pp->name, codept_args_file); */
    /*         log_error("rc: %d, errno: %d", rc, errno); */
    /*         log_error("Error putting %s: %s", pp->name, strerror( errnum )); */
    /*         fclose(fp); */
    /*         exit(1); */
    /*     } */
    /*     rc = fputs("\n", fp); */
    /*     if ( rc < 1) { */
    /*         errnum = errno; */
    /*         log_error("fputs fail putting newline to %s", codept_args_file); */
    /*         log_error("Value of errno: %d", errno); */
    /*         log_error("Error putting newline: %s", strerror( errnum )); */
    /*         fclose(fp); */
    /*         exit(1); */
    /*     } */
    /* } */

    /* now write out -I params for lib deps if enabled */
    /* emit_codept_lib_deps(); */

    /* if (coq) { */
    /*     emit_coq_codept_args_file(fp); */
    /* } */

    fflush(fp);

    rc = fclose(fp);
    if ( rc != EXIT_SUCCESS ) {
        errnum = errno;
        log_error("fclose fail on %s, rc: %d", codept_args_file, rc);
        log_error("Value of errno: %d", errno);
        log_error("Error fclosing %s: %s", codept_args_file, strerror( errnum ));
        exit(1);
    }
}

/* void codept(void) */
/* { */
/*     /\* printf("codept\n"); *\/ */
/*     struct codept_src *cd, *cdtmp; */
/*     FILE *fp; */
/*     int rc; */

/*     fp = fopen(codept_params, "w"); */
/*     if (fp == NULL) { */
/*         errnum = errno; */
/*         log_error("fopen fail for %s", codept_params); */
/*         log_error("Value of errno: %d", errno); */
/*         log_error("Error opening file %s: %s", codept_params, strerror( errnum )); */
/*         exit(1); */
/*     } */

/*     fputs("-one-line\n", fp); */

/*     HASH_ITER(hh, codept_srcs, cd, cdtmp) { */
/*         /\* printf("putting %s to %s", cd->dir, codept_params); *\/ */
/*         //FIXME: macro to handle error checking */
/*         rc = fputs(cd->dir, fp); */
/*         if ( rc < 1 ) { // success == non-neg int */
/*             errnum = errno; */
/*             log_error("fputs fail putting %s to %s", cd->dir, codept_params); */
/*             log_error("rc: %d, errno: %d", rc, errno); */
/*             log_error("Error putting %s: %s", cd->dir, strerror( errnum )); */
/*             fclose(fp); */
/*             exit(1); */
/*         } */
/*         rc = fputs("\n", fp); */
/*         if ( rc < 1) { */
/*             errnum = errno; */
/*             log_error("fputs fail putting newline to %s", codept_params); */
/*             log_error("Value of errno: %d", errno); */
/*             log_error("Error putting newline: %s", strerror( errnum )); */
/*             fclose(fp); */
/*             exit(1); */
/*         } */
/*     } */

/*     fputs("\n", fp); */

/*     struct preproc *pp, *pptmp; */
/*     HASH_ITER(hh, preprocs, pp, pptmp) { */
/*         /\* printf("putting %s to %s", cd->dir, codept_params); *\/ */
/*         //FIXME: macro to handle error checking */
/*         if (pp->mlout) { */
/*             /\* printf("codept preproc: %s", pp->mlout); *\/ */
/*             rc = fputs(pp->mlout, fp); */
/*         } */
/*         if ( rc < 1 ) { // success == non-neg int */
/*             errnum = errno; */
/*             log_error("fputs fail putting %s to %s", pp->name, codept_params); */
/*             log_error("rc: %d, errno: %d", rc, errno); */
/*             log_error("Error putting %s: %s", pp->name, strerror( errnum )); */
/*             fclose(fp); */
/*             exit(1); */
/*         } */
/*         rc = fputs("\n", fp); */
/*         if ( rc < 1) { */
/*             errnum = errno; */
/*             log_error("fputs fail putting newline to %s", codept_params); */
/*             log_error("Value of errno: %d", errno); */
/*             log_error("Error putting newline: %s", strerror( errnum )); */
/*             fclose(fp); */
/*             exit(1); */
/*         } */
/*     } */

/*     /\* now write out -I params for lib deps if enabled *\/ */
/*     /\* emit_codept_lib_deps(); *\/ */

/*     if (coq) { */
/*         emit_coq_codept_params(fp); */
/*     } */


/*     fflush(fp); */

/*     rc = fclose(fp); */
/*     if ( rc != EXIT_SUCCESS ) { */
/*         errnum = errno; */
/*         log_error("fclose fail on %s, rc: %d", codept_params, rc); */
/*         log_error("Value of errno: %d", errno); */
/*         log_error("Error fclosing %s: %s", codept_params, strerror( errnum )); */
/*         exit(1); */
/*     } */

/*     run_codept(); */
/* } */

/* void emit_coq_codept_params(FILE *fp) */
/* { */
/*     /\* printf("coqlib: %s", coqlib); *\/ */
/*     /\* enumerate_coq_sdk(fp); *\/ */
/*     struct sdk_dir *sd = NULL; */
/*     struct sdk_dir *sdtmp; */
/*     HASH_ITER(hh, sdk_dirs, sd, sdtmp) { */
/*         /\* printf("sdk dir: %s", sd->dir); *\/ */
/*         fputs("-I\n", fp); */
/*         fputs(sd->dir, fp); */
/*         fputs("\n", fp); */
/*     } */
/* } */

/* **************************************************************** */
void dump_codept_modules(void)
{
    log_debug("MODULES parsed:");
    struct module_s *module, *tmp;
    HASH_ITER(hh, codept_modules, module, tmp) {
        log_debug("");
        log_debug("module->name: %s", module->name);
        log_debug("module->type: %d (%s)",
                  module->type,
                  (module->type == 0)? "LOCAL"
                  :(module->type == 1)? "EXTERNAL"
                  :(module->type == 2)? "UKNOWN"
                  : "UNRECOGNIZED MODULE TYPE");
        if (module->type == 0) { // M_LOCAL) { friggin makeheaders doesn't get M_LOCAL
            log_debug("module->structfile: %s", module->structfile);
            log_debug("module->sigfile: %s", module->sigfile);
        } else {
            if (module->type == 1) { // M_EXTERNAL) {
                log_debug("module->lib: %s", module->lib);
            /* } else { */
            /*     if (module->type == 2) { // M_UNKNOWN) { */
            /*         log_debug("module UKNOWN"); */
            /*     } else { */
            /*         log_fatal("Unrecognized module type"); */
            /*     } */
            }
        }
    }
    log_debug("");
}

void dump_codept_filedeps(void)
{
    struct filedeps_s *fdeps, *tmpfdeps;
    char **p = NULL;
    HASH_ITER(hh, codept_filedeps, fdeps, tmpfdeps) {
        log_debug("");
        log_debug("fdeps->name: %s", fdeps->name);
        log_debug("fdeps->deps_sexp_str: %s", fdeps->deps_sexp_str);
        /* log_debug("fdeps->type: %d", fdeps->type); */
        log_debug("fdeps->deps:");
        while ( (p=(char**)utarray_next(fdeps->deps, p))) {
            log_debug("\t%s",*p);
        }
    }
}

void dump_codept_dirty_filedeps(void)
{
    printf("cccc\n");
  UT_array *strs;
  char *s, **p = NULL;
  while ( (p=(char**)utarray_next(dirty_fdeps, p))) {
    log_debug("dirty: %s",*p);
  }
}
