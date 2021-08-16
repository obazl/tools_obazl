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
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>

#include "utarray.h"
#include "uthash.h"

#include "codept.h"

int errnum;
int rc;

static const char codept_params[] = ".obazl.d/codept.params";
static const char codept_depends[] = ".obazl.d/codept.depends";

/* /\* #define BUFSIZE 1024 *\/ */
/* /\* #if INTERFACE *\/ */
/* int BUFSIZE; */
/* /\* #endif *\/ */

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

void run_codept(void)
{
    if (verbosity >= 1)
        printf("running codept\n");
    char cmd[PATH_MAX];
    sprintf(cmd, "codept -k -args .obazl.d/codept.params 2> /dev/null");

    FILE *fcodept_depends;
    if (keep) {
        printf("keeping %s\n", codept_depends);
        fcodept_depends = fopen(codept_depends, "w");
        if (fcodept_depends == NULL) {
            perror(codept_depends);
            fprintf(stderr, "run_codept fopen(%s, 'w')\n", codept_depends);
            exit(EXIT_FAILURE);
        }
    }

    FILE *fp;
    if ((fp = popen(cmd, "r+")) == NULL) {
        printf("Error opening pipe for codept!\n");
        exit(-1);
    }
    if (verbosity >= 2)
        printf("action: %s\n", cmd);
    char buf[1024];
    while (fgets(buf, sizeof(buf), fp) != NULL) {
        if (keep)
            fputs(buf, fcodept_depends);
        process_codept_line(buf);
    }
    rc = pclose(fp);
    if (rc) {
        exit(-1);
    }

    if (keep) {
        fflush(fcodept_depends);
        fclose(fcodept_depends);
    }
}

void codept(void)
{
    /* printf("codept\n"); */
    struct codept_src *cd, *cdtmp;
    FILE *fp;
    int rc;

    fp = fopen(codept_params, "w");
    if (fp == NULL) {
        errnum = errno;
        fprintf(stderr, "fopen fail for %s\n", codept_params);
        fprintf(stderr, "Value of errno: %d\n", errno);
        fprintf(stderr, "Error opening file %s: %s\n", codept_params, strerror( errnum ));
        exit(1);
    }

    fputs("-one-line\n", fp);

    HASH_ITER(hh, codept_srcs, cd, cdtmp) {
        /* printf("putting %s to %s\n", cd->dir, codept_params); */
        //FIXME: macro to handle error checking
        rc = fputs(cd->dir, fp);
        if ( rc < 1 ) { // success == non-neg int
            errnum = errno;
            fprintf(stderr, "fputs fail putting %s to %s\n", cd->dir, codept_params);
            fprintf(stderr, "rc: %d, errno: %d\n", rc, errno);
            fprintf(stderr, "Error putting %s: %s\n", cd->dir, strerror( errnum ));
            fclose(fp);
            exit(1);
        }
        rc = fputs("\n", fp);
        if ( rc < 1) {
            errnum = errno;
            fprintf(stderr, "fputs fail putting newline to %s\n", codept_params);
            fprintf(stderr, "Value of errno: %d\n", errno);
            fprintf(stderr, "Error putting newline: %s\n", strerror( errnum ));
            fclose(fp);
            exit(1);
        }
    }

    fputs("\n", fp);

    struct preproc *pp, *pptmp;
    HASH_ITER(hh, preprocs, pp, pptmp) {
        /* printf("putting %s to %s\n", cd->dir, codept_params); */
        //FIXME: macro to handle error checking
        if (pp->mlout) {
            /* printf("codept preproc: %s\n", pp->mlout); */
            rc = fputs(pp->mlout, fp);
        }
        if ( rc < 1 ) { // success == non-neg int
            errnum = errno;
            fprintf(stderr, "fputs fail putting %s to %s\n", pp->name, codept_params);
            fprintf(stderr, "rc: %d, errno: %d\n", rc, errno);
            fprintf(stderr, "Error putting %s: %s\n", pp->name, strerror( errnum ));
            fclose(fp);
            exit(1);
        }
        rc = fputs("\n", fp);
        if ( rc < 1) {
            errnum = errno;
            fprintf(stderr, "fputs fail putting newline to %s\n", codept_params);
            fprintf(stderr, "Value of errno: %d\n", errno);
            fprintf(stderr, "Error putting newline: %s\n", strerror( errnum ));
            fclose(fp);
            exit(1);
        }
    }

    /* now write out -I params for lib deps if enabled */
    /* emit_codept_lib_deps(); */

    if (coq) {
        emit_coq_codept_params(fp);
    }


    fflush(fp);

    rc = fclose(fp);
    if ( rc != EXIT_SUCCESS ) {
        errnum = errno;
        fprintf(stderr, "fclose fail on %s, rc: %d\n", codept_params, rc);
        fprintf(stderr, "Value of errno: %d\n", errno);
        fprintf(stderr, "Error fclosing %s: %s\n", codept_params, strerror( errnum ));
        exit(1);
    }

    run_codept();
}

void emit_coq_codept_params(FILE *fp)
{
    /* printf("coqlib: %s\n", coqlib); */
    /* enumerate_coq_sdk(fp); */
    struct sdk_dir *sd = NULL;
    struct sdk_dir *sdtmp;
    HASH_ITER(hh, sdk_dirs, sd, sdtmp) {
        /* printf("sdk dir: %s\n", sd->dir); */
        fputs("-I\n", fp);
        fputs(sd->dir, fp);
        fputs("\n", fp);
    }
}
