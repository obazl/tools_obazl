#include <libgen.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <unistd.h>

#include "liblogc.h"
/* #include "librunfiles.h" */
#include "obazl_utils.h"
#include "utstring.h"
#include "xdgc.h"

#include "new_bazel.h"

bool debug;
bool trace;
bool verbose;

/* static struct runfiles_s *runfiles = NULL; */

char *ws_dir;                  /* BUILD_WORKSPACE_DIRECTORY */
char *build_wd;                /* BUILD_WORKING_DIRECTORY */

bool prefix_matches(const char *pfx, const char *str)
{
    return strncmp(pfx, str, strlen(pfx)) == 0;
}

/* we expect this to always be run by bazel, as an external tool */
char *_templates[] = {
    /* dest: BUILD_WORKSPACE_DIRECTORY */
    "external/obazl/new/templates/.bazelignore",
    "external/obazl/new/templates/.bazeliskrc",
    "external/obazl/new/templates/.bazelrc",
    "external/obazl/new/templates/.bazelversion",
    "external/obazl/new/templates/.config/.gitignore",
    "external/obazl/new/templates/.config/miblrc",
    "external/obazl/new/templates/.config/user.bazelrc",
    "external/obazl/new/templates/WORKSPACE.bazel",
    "external/obazl/new/templates/WORKSPACE.bzl",
    "external/obazl/new/templates/WORKSPACE.opam.bzl",

    /* dest: XDG_DATA_HOME/obazl */
    "external/obazl/new/templates/xdg/data/queries/obazlinfo.qry.bzl",

    /* dest: XDG_BIN_HOME ($HOME/.local/bin) */
    "external/obazl/new/templates/xdg/bin/obazlinfo",

    "" /* <<== do not remove terminating null string */
};
char **template;
int pfx_len = 29; // strlen("external/obazl/new/templates") + 1

UT_string *abs_dest;
UT_string *abs_dir;
char *dest;
char *safe_dest;
char *path;

EXPORT void new_bazel_package(char *pgm)
{
    log_debug("NEW_bazel_package: %s", pgm);
}

EXPORT void new_bazel_module(char *pgm)
{
    log_debug("NEW_bazel_module: %s", pgm);
    return;
    /* if (debug) { */
        log_debug("new_workspace: %s", pgm);
        log_debug("CWD: %s", getcwd(NULL, 0));
        log_debug("BUILD_WORKSPACE_DIRECTORY: %s", ws_dir);
        log_debug("BUILD_WORKING_DIRECTORY: %s", build_wd);
    /* } */

    utstring_new(abs_dest);
    utstring_printf(abs_dest, "touch %s/BUILD.bazel", ws_dir);
    system(utstring_body(abs_dest));

    fprintf(stderr, "Bazel current repo: %s", BAZEL_CURRENT_REPOSITORY);

    template = _templates;

    int rc;
    while (strlen(*template) != 0) {
        if (verbose)
            log_info("copy src: '%s'", *template);
        errno = 0;
        rc = access(*template, F_OK);
        if (rc == 0) {
            ;
            /* log_info("file accessible: %s", *template); */
        } else {
            perror(NULL);
            log_info("file " RED "inaccessible:" CRESET " %s", *template);
        }

        /* strip pfx "external/new/templates" */
        dest = strndup(*template + pfx_len,
                       strlen(*template) - pfx_len);

        /* now config for dest dirs */
        if (strncmp(dest, "xdg/bin",  7) == 0) {
            if (debug) log_info("XDG bin dest: %s", dest);
            /* strip pfx 'xdg/bin' */
            dest = strndup(dest + 8, strlen(dest) - 8);
            if (debug) log_info("XDG bin dest: %s", dest);
            safe_dest = strndup(dest, strlen(dest));
            path = dirname(dest);   /* after this dest is unreliable */
            /* NB: path may be '.' but that's ok */

            utstring_renew(abs_dir);
            utstring_printf(abs_dir, "%s/%s",
                            xdg_bin_home(),
                            /* utstring_body(xdg_bin_home), */
                            path);
            if (debug) log_info("xdg dest: %s", utstring_body(abs_dir));

            int rc = access(utstring_body(abs_dir), R_OK);
            if (rc == 0) {
                /* printf("dir accessible: %s\n", utstring_body(abs_dir)); */
            } else {
                if (debug)
                    log_warn("dir " RED "inaccessible;" CRESET " creating: %s",
                             utstring_body(abs_dir));
                mkdir_r(utstring_body(abs_dir));
            }

            utstring_renew(abs_dest);
            utstring_printf(abs_dest, "%s/%s", utstring_body(abs_dir),
                            safe_dest);

            // common code
            if (verbose) log_debug("copy dest: %s", utstring_body(abs_dest));
            copyfile(*template, utstring_body(abs_dest));

            rc = chmod(utstring_body(abs_dest), S_IRWXU);

            if (verbose) log_info("");
            free(safe_dest);
            free(dest);

        } else if (strncmp(dest, "xdg/data/queries",  16) == 0) {
            /* strip pfx 'xdg/data/queries' */
            dest = strndup(dest + 17,
                           strlen(dest) - 17);
            if (debug) log_info("XDG data dest: %s", dest);
            safe_dest = strndup(dest, strlen(dest));
            path = dirname(dest);   /* after this dest is unreliable */
            /* NB: path may be '.' but that's ok */

            utstring_renew(abs_dir);
            utstring_printf(abs_dir, "%s/%s/obazl/queries",
                            xdg_data_home(),
                            /* utstring_body(xdg_data_home), */
                            path);
            if (debug) log_info("xdg dest: %s", utstring_body(abs_dir));

            int rc = access(utstring_body(abs_dir), R_OK);
            if (rc == 0) {
                /* printf("dir accessible: %s\n", utstring_body(abs_dir)); */
            } else {
                if (debug)
                    log_debug("dir " RED "inaccessible;" CRESET " creating: %s",
                              utstring_body(abs_dir));
                mkdir_r(utstring_body(abs_dir));
            }

            utstring_renew(abs_dest);
            utstring_printf(abs_dest, "%s/%s", utstring_body(abs_dir),
                            safe_dest);

            // common code
            if (verbose) log_debug("copy dest: %s", utstring_body(abs_dest));
            if (verbose) log_info("");
            copyfile(*template, utstring_body(abs_dest));

            free(safe_dest);
            free(dest);

        } else {
            /* log_info("Local dest: %s", dest); */
            /* dirname may modify its arg so we need a copy */
            safe_dest = strndup(dest, strlen(dest));
            path = dirname(dest);   /* after this dest is unreliable */
            utstring_renew(abs_dir);
            utstring_printf(abs_dir, "%s/%s", ws_dir, path);

            int rc = access(utstring_body(abs_dir), R_OK);
            if (rc == 0) {
                /* printf("dir accessible: %s\n", utstring_body(abs_dir)); */
            } else {
                if (debug)
                    log_debug("dir " RED "inaccessible;" CRESET " creating: %s",
                              utstring_body(abs_dir));
                mkdir_r(utstring_body(abs_dir));
            }

            utstring_renew(abs_dest);
            utstring_printf(abs_dest, "%s/%s", ws_dir, safe_dest);

            // common code
            if (verbose)
                log_debug("copy dest: %s\n", utstring_body(abs_dest));
            copyfile(*template, utstring_body(abs_dest));

            free(safe_dest);
            free(dest);
        }
        template++;
    }
}

/* void new_workspace_rf(char *pgm) */
/* { */
/*     if (debug) { */
/*         log_debug("new_workspace: %s\n", pgm); */
/*         log_debug("CWD: %s", getcwd(NULL, 0)); */
/*         log_debug("BUILD_WORKSPACE_DIRECTORY: %s", ws_dir); */
/*         log_debug("BUILD_WORKING_DIRECTORY: %s", build_wd); */
/*     } */

/*     runfiles = runfiles_create(pgm); */
/*     char *dest; */
/*     char *path; */

/*     int i = 0; */
/*     int pfx_len = 20; // "obazl/new/templates" */

/*     chdir(ws_dir); */
/*     if (debug) log_debug("CWD: %s", getcwd(NULL, 0)); */

/*     system("touch BUILD.bazel"); */

/*     while (runfiles[i].key != NULL) { */
/*         if (prefix_matches("obazl/new/templates/BUILD.bazel", runfiles[i].key)) { */
/*             ; // omit */
/*         } else { */
/*             if (prefix_matches("obazl/new/templates/", runfiles[i].key)) { */
/*                 /\* printf(RED "entry %d:" CRESET " %s -> %s\n", i, *\/ */
/*                 /\*        runfiles[i].key, runfiles[i].val); *\/ */
/*                 char *rp = realpath(runfiles[i].val, NULL); */
/*                 /\* printf("realpath %d: %s\n", i, rp); *\/ */
/*                 free(rp); */

/*                 dest = strndup(runfiles[i].key + pfx_len, */
/*                                strlen(runfiles[i].key) - pfx_len); */
/*                 if (prefix_matches("dot/", dest)) { */
/*                     dest = strndup(dest + 3, strlen(dest) - 3); */
/*                     dest[0] = '.'; */
/*                     /\* printf(BLU "Dest:" CRESET " %s\n", dest); *\/ */
/*                     path = dirname(dest); */
/*                     /\* printf(BLU "dest dir:" CRESET " %s\n", path); *\/ */
/*                     int rc = access(path, R_OK); */
/*                     if (rc == 0) { */
/*                         ; */
/*                         /\* printf("dir accessible: %s\n", path); *\/ */
/*                     } else { */
/*                         /\* printf("dir inaccessible: %s\n", path); *\/ */
/*                         mkdir_r(path); */
/*                     } */
/*                 } else { */
/*                     /\*     printf(BLU "dest:" CRESET " %s\n", dest); *\/ */
/*                     /\* } *\/ */
/*                     /\* if (prefix_matches("bzl/", dest)) { *\/ */
/*                     /\* dest = strndup(dest + 3, strlen(dest) - 3); *\/ */
/*                     /\* dest[0] = '.'; *\/ */
/*                     /\* printf(BLU "dest:" CRESET " %s\n", dest); *\/ */
/*                     path = dirname(dest); */
/*                     /\* printf(BLU "dest dir:" CRESET " %s\n", path); *\/ */
/*                     int rc = access(path, R_OK); */
/*                     if (rc == 0) { */
/*                         ;/\* printf("dir accessible: %s\n", path); *\/ */
/*                     } else { */
/*                         /\* printf("dir inaccessible: %s\n", path); *\/ */
/*                         mkdir_r(path); */
/*                     } */
/*                     /\* } else { *\/ */
/*                     /\* } *\/ */
/*                 } */
/*                 copyfile(rp, dest); */
/*                 free(dest); */

/*                 int rc = access(runfiles[i].val, R_OK); */
/*                 if (rc == 0) { */
/*                     /\* printf("accessible: %s\n", runfiles[i].val); *\/ */
/*                 } else { */
/*                     /\* printf("inaccessible: %s\n", runfiles[i].val); *\/ */
/*                 } */
/*             } */
/*         } */
/*         i++; */
/*     } */
/* } */

/* int main(int argc, char *argv[]) */
/* { */
/*     char *opts = "hdtv"; */
/*     int opt; */
/*     char *pkgarg = NULL; */

/*     while ((opt = getopt(argc, argv, opts)) != -1) { */
/*         switch (opt) { */
/*         case '?': */
/*             fprintf(stderr, "uknown opt: %c", optopt); */
/*             exit(EXIT_FAILURE); */
/*             break; */
/*         case ':': */
/*             fprintf(stderr, "uknown option: %c", optopt); */
/*             exit(EXIT_FAILURE); */
/*             break; */
/*         case 'd': */
/*             debug = true; */
/*             break; */
/*         case 'h': */
/*             /\* _print_usage(); *\/ */
/*             display_manpage("man1", "shared_refresh"); */
/*             exit(EXIT_SUCCESS); */
/*             break; */
/*         case 't': */
/*             trace = true; */
/*             break; */
/*         case 'v': */
/*             verbose = true; */
/*         case 'x': */
/*         default: */
/*             ; */
/*         } */
/*     } */
/*     if ((argc - optind) > 1) { */
/*         fprintf(stderr, "Too many options"); */
/*         exit(EXIT_FAILURE); */
/*     } else { */
/*         if ((argc-optind) == 1) { */
/*             pkgarg = argv[optind]; */
/*             printf("PATH: %s\n", pkgarg); */
/*         } */
/*     } */

/*     /\* char *cwd = getcwd(NULL, 0); *\/ */
/*     /\* printf("cwd: %s\n", cwd); *\/ */
/*     /\* printf("argv[0]: %s\n", argv[0]); *\/ */

/*     ws_dir = getenv("BUILD_WORKSPACE_DIRECTORY"); */
/*     build_wd = getenv("BUILD_WORKING_DIRECTORY"); */
/*     utstring_new(abs_dest); */
/*     utstring_new(abs_dir); */

/*     /\* config_xdg_dirs(); *\/ */

/*     optind = 1; */
/*     new_workspace(argv[0]); */

/*     if (debug) */
/*         log_debug("workspace exit..."); */
/*     return 0; */
/* } */
