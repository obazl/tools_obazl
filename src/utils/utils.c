#include <errno.h>
#include <dirent.h>
#include <stdbool.h>

#if EXPORT_INTERFACE
#include <stdio.h>
#endif

#ifdef __linux__
#include <linux/limits.h>
#else
#include <limits.h>
#endif

#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "log.h"
/* #include "utarray.h" */
#include "utstring.h"

#include "utils.h"

UT_string *runfiles_root;

EXPORT void mkdir_r(const char *dir) {
    /* log_debug("mkdir_r %s", dir); */
    char tmp[512];
    char *p = NULL;
    size_t len;

    snprintf(tmp, sizeof(tmp),"%s",dir);
    len = strlen(tmp);
    if (tmp[len - 1] == '/')
        tmp[len - 1] = 0;
    for (p = tmp + 1; *p; p++)
        if (*p == '/') {
            *p = 0;
            mkdir(tmp, S_IRWXU);
            *p = '/';
        }
    mkdir(tmp, S_IRWXU);
}

int copyfile(char *fromfile, char *tofile) {
    char ch;// source_file[20], target_file[20];

    FILE *source = fopen(fromfile, "r");
    if (source == NULL) {
        fprintf(stderr, "copyfile fopen fail on fromfile: %s\n", fromfile);
        exit(EXIT_FAILURE);
    }
    FILE *target = fopen(tofile, "w");
    if (target == NULL) {
        fclose(source);
        fprintf(stderr, "copyfile fopen fail on tofile: %s\n", tofile);
        exit(EXIT_FAILURE);
    }
    while ((ch = fgetc(source)) != EOF)
        fputc(ch, target);
/* #if defined(DEBUG_TRACE) */
/*         printf("File copy successful: %s -> %s.\n", */
/*                fromfile, tofile); */
/* #endif */
    fclose(source);
    fclose(target);
    return 0;
}

void _copy_template(char *buildfile, UT_string *to_file) {
    UT_string *src;
    utstring_new(src);
    utstring_printf(src,
                    "%s/external/obazl/templates/%s",
                    /* "%s/obazl/templates/%s", */
                    utstring_body(runfiles_root),
                    buildfile);
    int rc = access(utstring_body(src), F_OK);
    if (rc != 0) {
        log_error("not found: %s", utstring_body(src));
        fprintf(stderr, "not found: %s\n", utstring_body(src));
        return;
    }

    /* if (debug) { */
    /*     log_debug("copying %s to %s\n", */
    /*               utstring_body(src), */
    /*               utstring_body(to_file)); */
    /* } */
    errno = 0;
    rc = copyfile(utstring_body(src),
                  utstring_body(to_file));
    if (rc != 0) {
        log_error("copyfile: %s", strerror(errno));
        fprintf(stderr, "ERROR copyfile: %s", strerror(errno));
        log_error("Exiting");
        fprintf(stderr, "Exiting\n");
        exit(EXIT_FAILURE);
    }
}

/* void emit_template(char *t) */
/* { */
/*     log_debug("emit_template: %s\n", t); */
/*     UT_string *ocaml_file; */
/*     utstring_new(ocaml_file); */
/*     /\* utstring_concat(ocaml_file, bzl_switch_pfx); *\/ */
/*     /\* utstring_printf(ocaml_file, "/ocaml/toolchains"); *\/ */
/*     utstring_printf(ocaml_file, "/bzl/host"); */
/*     mkdir_r(utstring_body(ocaml_file)); */
/*     utstring_printf(ocaml_file, "/BUILD.bazel"); */
/*     _copy_template("ocaml_toolchains.BUILD", ocaml_file); */
/*     utstring_free(ocaml_file); */
/* } */

/* for each file in templates: copy to same relative location in dest */

