#include <libgen.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "log.h"
#include "librunfiles.h"
#include "workspace.h"

bool debug;
bool trace;
bool verbose;

static struct runfiles_s *runfiles = NULL;

char *ws_dir;                  /* BUILD_WORKSPACE_DIRECTORY */
char *build_wd;                /* BUILD_WORKING_DIRECTORY */

bool prefix_matches(const char *pfx, const char *str)
{
    return strncmp(pfx, str, strlen(pfx)) == 0;
}

void new_workspace(char *pgm)
{
    if (debug) printf("new_workspace: %s\n", pgm);
    if (debug) log_debug("CWD: %s", getcwd(NULL, 0));
    if (debug) log_debug("BUILD_WORKSPACE_DIRECTORY: %s", ws_dir);
    if (debug) log_debug("BUILD_WORKING_DIRECTORY: %s", build_wd);

    runfiles = runfiles_new(pgm);
    char *dest;
    char *path;

    int i = 0;
    int pfx_len = 16; // "obazl/templates"

    chdir(ws_dir);
    if (debug) log_debug("CWD: %s", getcwd(NULL, 0));

    system("touch BUILD.bazel");

    while (runfiles[i].key != NULL) {
        if (prefix_matches("obazl/templates/BUILD.bazel", runfiles[i].key)) {
            ;
        } else {
            if (prefix_matches("obazl/templates/", runfiles[i].key)) {
                /* printf(RED "entry %d:" CRESET " %s -> %s\n", i, */
                /*        runfiles[i].key, runfiles[i].val); */
                char *rp = realpath(runfiles[i].val, NULL);
                /* printf("realpath %d: %s\n", i, rp); */
                free(rp);

                dest = strndup(runfiles[i].key + pfx_len,
                               strlen(runfiles[i].key) - pfx_len);
                if (prefix_matches("dot/", dest)) {
                    dest = strndup(dest + 3, strlen(dest) - 3);
                    dest[0] = '.';
                    /* printf(BLU "dest:" CRESET " %s\n", dest); */
                    path = dirname(dest);
                    /* printf(BLU "dest dir:" CRESET " %s\n", path); */
                    int rc = access(path, R_OK);
                    if (rc == 0) {
                        ;
                        /* printf("dir accessible: %s\n", path); */
                    } else {
                        /* printf("dir inaccessible: %s\n", path); */
                        mkdir_r(path);
                    }
                } else {
                    /*     printf(BLU "dest:" CRESET " %s\n", dest); */
                    /* } */
                    /* if (prefix_matches("bzl/", dest)) { */
                    /* dest = strndup(dest + 3, strlen(dest) - 3); */
                    /* dest[0] = '.'; */
                    /* printf(BLU "dest:" CRESET " %s\n", dest); */
                    path = dirname(dest);
                    /* printf(BLU "dest dir:" CRESET " %s\n", path); */
                    int rc = access(path, R_OK);
                    if (rc == 0) {
                        ;/* printf("dir accessible: %s\n", path); */
                    } else {
                        /* printf("dir inaccessible: %s\n", path); */
                        mkdir_r(path);
                    }
                    /* } else { */
                    /* } */
                }
                copyfile(rp, dest);
                free(dest);

                int rc = access(runfiles[i].val, R_OK);
                if (rc == 0) {
                    /* printf("accessible: %s\n", runfiles[i].val); */
                } else {
                    /* printf("inaccessible: %s\n", runfiles[i].val); */
                }
            }
        }
        i++;
    }
}

int main(int argc, char *argv[])
{
    char *opts = "hdtv";
    int opt;
    char *pkgarg = NULL;

    while ((opt = getopt(argc, argv, opts)) != -1) {
        switch (opt) {
        case '?':
            fprintf(stderr, "uknown opt: %c", optopt);
            exit(EXIT_FAILURE);
            break;
        case ':':
            fprintf(stderr, "uknown option: %c", optopt);
            exit(EXIT_FAILURE);
            break;
        case 'd':
            debug = true;
            break;
        case 'h':
            /* _print_usage(); */
            display_manpage("man1", "shared_refresh");
            exit(EXIT_SUCCESS);
            break;
        case 't':
            trace = true;
            break;
        case 'v':
            verbose = true;
        case 'x':
        default:
            ;
        }
    }
    if ((argc - optind) > 1) {
        fprintf(stderr, "Too many options");
        exit(EXIT_FAILURE);
    } else {
        if ((argc-optind) == 1) {
            pkgarg = argv[optind];
            printf("PATH: %s\n", pkgarg);
        }
    }

    /* char *cwd = getcwd(NULL, 0); */
    /* printf("cwd: %s\n", cwd); */
    /* printf("argv[0]: %s\n", argv[0]); */

    ws_dir = getenv("BUILD_WORKSPACE_DIRECTORY");
    build_wd = getenv("BUILD_WORKING_DIRECTORY");

    optind = 1;
    new_workspace(argv[0]);

    if (debug)
        log_debug("workspace exit...");
    return 0;
}
