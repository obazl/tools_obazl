#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* #include "ini.h" */
#include "log.h"

/* #include "linenoise.h" */
/* #include "s7.h" */

/* #include "utarray.h" */
/* #include "utstring.h" */

/* #include "load_dune.h" */
/* #include "bazel_config.h" */

#include "mibl.h"
#include "deploy.h"

/* extern s7_scheme *s7; */

/* char *history = ".ocamlark.history.txt"; */

extern bool debug;
extern bool trace;
extern bool verbose;
extern bool ini_error;
/* extern UT_string *obazl_ini_path; */
extern struct configuration_s obazl_config;

/* UT_array *opam_dirs;             /\* string list *\/ */
/* extern UT_string *codept_args_file; */

void print_usage(void)
{
    printf("Usage: mibl [-m | -k | -v | -h ]\n");
}

int main(int argc, char **argv)
{
    /* printf("deploy\n"); */
    int opt;
    while ((opt = getopt(argc, argv, "hdtvV")) != -1) {
        switch (opt) {
        case 'd':
            debug = true;
            break;
        case 'h':
            print_usage();
            exit(EXIT_SUCCESS);
            break;
        case 't':
            trace =true;
            break;
        case 'v':
            verbose =true;
            break;
        case 'V':
            printf("Version: 1.0\n");
            break;
        default:
            print_usage();
            exit(EXIT_FAILURE);
        }
    }

    bazel_configure(); // getcwd(NULL, 0));

    deploy();

    return 0;
}
