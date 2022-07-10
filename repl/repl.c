#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* #include "ini.h" */
#include "log.h"

#include "linenoise.h"
#include "s7.h"

/* #include "utarray.h" */
/* #include "utstring.h" */

/* #include "load_dune.h" */
/* #include "opam_config.h" */
/* #include "bazel_config.h" */
/* #include "s7_config.h" */

#include "mibl.h"
#include "repl.h"

extern s7_scheme *s7;

char *history = ".ocamlark.history.txt";

extern bool debug;
extern bool trace;
extern bool verbose;
extern bool ini_error;
/* extern UT_string *obazl_ini_path; */
extern struct configuration_s bazel_config;

/* UT_array *opam_dirs;             /\* string list *\/ */
/* extern UT_string *codept_args_file; */

void completion(const char *buf, linenoiseCompletions *lc) {
    if (buf[0] == 'h') {
        linenoiseAddCompletion(lc,"hello");
        linenoiseAddCompletion(lc,"hello there");
    }
}

char *hints(const char *buf, int *color, int *bold) {
    if (!strcasecmp(buf,"hello")) {
        *color = 35;
        *bold = 0;
        return " World";
    }
    return NULL;
}

void print_usage(void)
{
    printf("Usage: mibl [-m | -k | -v | -h ]\n");
}

void std_repl()
{
    char *line;
    char response[1024];        /* result of evaluating input */

    /* const char *errmsg = NULL; */

    /* list opam dirs, to parameterize codept */
    /* opam_dirs = inventory_opam(); */
    /* log_debug("OPAM dir ct: %d", utarray_len(opam_dirs)); */
    /* char **dir = NULL; */
    /* while ( (dir=(char**)utarray_next(opam_dirs,dir))) { */
    /*     log_debug("%s",*dir); */
    /* } */

    /* dir = NULL; */
    /* while ( (dir=(char**)utarray_next(bazel_config.src_dirs,dir))) { */
    /*     log_debug("src dir: %s",*dir); */
    /* } */

    /* log_debug("linenoise config"); */
    linenoiseSetMultiLine(1);   /* always support multiline */

    /* Set the completion callback. This will be called every time the
     * user uses the <tab> key. */
    linenoiseSetCompletionCallback(completion);
    linenoiseSetHintsCallback(hints);

    //FIXME: put history in ~/.obazl.d
    /* Load history from file. The history file is just a plain text file
     * where entries are separated by newlines. */
    linenoiseHistoryLoad(history); /* Load the history at startup */

    /* Now this is the main loop of the typical linenoise-based application.
     * The call to linenoise() will block as long as the user types something
     * and presses enter.
     *
     * The typed string is returned as a malloc() allocated string by
     * linenoise, so the user needs to free() it. */

    while((line = linenoise("s7> ")) != NULL) {

        if (line[0] != '\0' && line[0] != '/') {
            snprintf(response, 1024, "(write %s)", line);
            s7_eval_c_string(s7, response);
            printf("%s", "\n");

            linenoiseHistoryAdd(line); /* Add to the history. */
            linenoiseHistorySave(history); /* Save the history on disk. */
        } else if (!strncmp(line,"/historylen",11)) {
            /* The "/historylen" command will change the history len. */
            int len = atoi(line+11);
            linenoiseHistorySetMaxLen(len);
        } else if (!strncmp(line, "/mask", 5)) {
            linenoiseMaskModeEnable();
        } else if (!strncmp(line, "/unmask", 7)) {
            linenoiseMaskModeDisable();
        } else if (line[0] == '/') {
            printf("Unreconized command: %s\n", line);
        }
        free(line);
    }
}

int main(int argc, char **argv)
{
    printf("@obazl//repl v 0.1\n");
    /* Parse options, with --multiline we enable multi line editing. */
    int opt;
    while ((opt = getopt(argc, argv, "edmkhtvV")) != -1) {
        switch (opt) {
        case 'd':
            debug = true;
            break;
        case 't':
            trace = true;
            break;
        case 'm':
            linenoiseSetMultiLine(1);
            printf("Multi-line mode enabled.\n");
            break;
        case 'k':
            linenoisePrintKeyCodes();
            exit(0);
            break;
        case 'h':
            print_usage();
            exit(EXIT_SUCCESS);
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

   /* initialize in this order: bazel then s7 */
    bazel_configure(); // getcwd(NULL, 0));
    s7_configure();
    /* chdir(launch_dir); */
    /* if (debug) */
    /*     log_debug("Set CWD to launch dir: %s", launch_dir); */

    /* s7_repl(s7); */
    xen_repl(argc, argv);

    return 0;
}
