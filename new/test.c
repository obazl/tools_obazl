#include <libgen.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "unity.h"

/* #include "log.h" */

/* #include "librunfiles.h" */
#include "test.h"

bool debug;
bool trace;
bool verbose;

/* UT_string *buf; */

char *pgm;
static struct runfiles_s *runfiles = NULL;

void setUp(void) {
    printf("setUp: %s\n", pgm);
    if (runfiles != NULL) return;
    runfiles = runfiles_new(pgm);
    /* printf("runfiles:\n"); */
    int i = 0;
    while (runfiles[i].key != NULL) {
        printf("entry %d: %s -> %s\n", i,
               runfiles[i].key, runfiles[i].val);
        i++;
    }
    /* printf("done\n"); */
}

void tearDown(void) {
    printf("\ntearDown\n");
    /* free(runfiles); */
}

void test_runfiles(void) {
    int i = 0;
    while (runfiles[i].key != NULL) {
        char *lhs_basename = basename(runfiles[i].key);
        char *rhs_basename = basename(runfiles[i].val);
        TEST_ASSERT_EQUAL_STRING(lhs_basename, rhs_basename);
        i++;
    }
}

void test_runfile_count(void) {
    int i = 0;
    while (runfiles[i].key != NULL) {
        i++;
    }
    TEST_ASSERT_EQUAL_INT(28, i);
}

void test_runfile_search(void)
{
    char *needle = "obazl/templates/COSWITCH.bzl";
    int i = 0;
    while (runfiles[i].key != NULL) {
        if (strcmp(runfiles[i].key, needle) == 0)
            break;
        i++;
    }
    TEST_ASSERT_EQUAL_INT(16, i);
}

int main(int argc, char **argv, char **envp)
{
    /* for (char **env = envp; *env != 0; env++) { */
    /*     char *thisEnv = *env; */
    /*     printf("%s\n", thisEnv); */
    /* } */
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
    printf("argv[0]: %s\n", argv[0]);

    pgm = argv[0];

    UNITY_BEGIN();
    RUN_TEST(test_runfiles);
    RUN_TEST(test_runfile_count);
    RUN_TEST(test_runfile_search);
    UNITY_END();

    printf("test exit...\n");
    return 0;
}
