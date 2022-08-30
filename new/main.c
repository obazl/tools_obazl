#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* #include "ini.h" */
/* #include "log.h" */
/* #include "mibl.h" */
#include "new.h"

bool debug;
bool trace;
bool verbose;

int new_main(int argc, char *argv[])
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

    char *cwd = getcwd(NULL, 0);
    printf("cwd: %s\n", cwd);

    optind = 1;
    /* return opam_main(argc, argv, XDG); */

    printf("argv[0]: %s\n", argv[0]);

    if (strncmp(basename(argv[0]), "workspace", 5) == 0) {
        new_workspace();
    }

    printf("main exit...\n");
    return 0;
}
