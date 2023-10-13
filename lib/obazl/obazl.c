#include <libgen.h>             /* basename */
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "log.h"
#include "obazl.h"

#if defined(DEBUG_fastbuild)
bool libobazl_debug;
bool libobazl_trace;
#endif

bool libobazl_verbose;

/* EXPORT int libobazl_main(int argc, char *argv[]) */
/* { */
/*     log_debug("libobazl_main: %s", argv[0]); */
/*     return 0; */

/* #if defined(DEBUG_fastbuild) */
/*     char *opts = "hdtv"; */
/* #else */
/*     char *opts = "hv"; */
/* #endif */
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
/* #if defined(DEBUG_fastbuild) */
/*         case 'd': */
/*             libobazl_debug = true; */
/*             break; */
/* #endif */
/*         case 'h': */
/*             /\* _print_usage(); *\/ */
/*             display_manpage("man1", "shared_refresh"); */
/*             exit(EXIT_SUCCESS); */
/*             break; */
/* #if defined(DEBUG_fastbuild) */
/*         case 't': */
/*             libobazl_trace = true; */
/*             break; */
/* #endif */
/*         case 'v': */
/*             libobazl_verbose = true; */
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

/*     char *cwd = getcwd(NULL, 0); */
/*     printf("cwd: %s\n", cwd); */

/*     optind = 1; */
/*     /\* return opam_main(argc, argv, XDG); *\/ */

/*     printf("argv[0]: %s\n", argv[0]); */

/*     if (strncmp(basename(argv[0]), "workspace", 5) == 0) { */
/*         /\* new_workspace(); *\/ */
/*         printf("dispatching to new_workspace\n"); */
/*     } */

/*     printf("libobazl_main exit...\n"); */
/*     return 0; */
/* } */

int main(int argc, char *argv[])
{
    (void)argc;
    printf("obazl argv[0]: %s\n", argv[0]);

    char *action = strdup(basename(argv[0]));
    char *path = strdup(dirname(argv[0]));
    char *subcmd = strdup(basename(path));

    if (strncmp(subcmd, "ocaml", 5) == 0) {
        if (strncmp(action, "module", 6) == 0) {
            new_ocaml_module("test");
        }
        else if (strncmp(action, "pkg", 3) == 0) {
            new_ocaml_package("test");
        }
    }
    else if (strncmp(subcmd, "bazel", 5) == 0) {
        if (strncmp(action, "module", 6) == 0) {
            new_bazel_module("test");
        }
        else if (strncmp(action, "pkg", 3) == 0) {
            new_bazel_package("test");
        }
    }

    free(action);
    free(path);
    free(subcmd);
    printf("obazl main exit...\n");
    return 0;
}
