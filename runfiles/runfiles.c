#include <errno.h>
#include <spawn.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>

#include "log.h"
#include "utstring.h"

#include "runfiles.h"

/* api:
   runfiles_new() -> UT_hash of MANIFEST (relpath, realpath) pairs
   runfiles_lookup(runfiles, path)
   c:    runfiles_lookup(runfiles, "my_workspace/path/to/my/data.txt");
   c++:  runfiles->Rlocation("my_workspace/path/to/my/data.txt");
 */

#if EXPORT_INTERFACE
struct runfiles_s {
    char *key;
    char *val;
}
#endif

EXPORT struct runfiles_s *runfiles_new(char *argv0)
{
    if (debug)
        log_debug("runfiles_new");

    /* env vars: RUNFILES_MANIFEST_FILE, RUNFILES_DIR */
    /* at launch: argv[0] is pgmname path,
       and cwd is argv[0].runfiles/__main__/
       the manifest is in two places:
       argv[0].runfiles_manifest, and
       argv[0].runfiles/MANIFEST
       (== cwd/../MANIFEST)
     */

    /* strategy: check in order:
     RUNFILES_MANIFEST_FILE, RUNFILES_DIR,
     argv[0]-relative path
    */

    /* WARNING: context differs for 'bazel run' and 'bazel test' */

    char *runfiles_manifest_file;
    char *runfiles_dir;
    char *launch_dir;

    UT_string *manifest_file;
    utstring_new(manifest_file);

    launch_dir = getcwd(NULL, 0);

    runfiles_manifest_file = getenv("RUNFILES_MANIFEST_FILE");
    if (debug)
        log_debug("RUNFILES_MANIFEST_FILE: %s", runfiles_manifest_file);
    if (runfiles_manifest_file) {
        utstring_printf(manifest_file, "%s", runfiles_manifest_file);
        int rc = access(utstring_body(manifest_file), R_OK);
        if (rc == 0) {
            if (debug)
                log_debug("using RUNFILES_MANIFEST_FILE");
            goto found_manifest;
        }
    }

    /* $TEST_SRCDIR =? $RUNFILES_DIR - see https://github.com/bazelbuild/bazel/issues/6093 */
    char *test_srcdir = getenv("TEST_SRCDIR");
    if (debug)
        log_debug("TEST_SRCDIR: %s", test_srcdir);

    runfiles_dir = getenv("RUNFILES_DIR");
    if (debug)
        log_debug("RUNFILES_DIR: %s", runfiles_dir);
    if (runfiles_dir) {
        utstring_renew(manifest_file);
        utstring_printf(manifest_file, "%s/MANIFEST", runfiles_dir);
        if (debug)
            log_debug("accessing %s", utstring_body(manifest_file));
        int rc = access(utstring_body(manifest_file), R_OK);
        if (rc == 0) {
            if (debug)
                log_debug("using RUNFILES_DIR: %s", utstring_body(manifest_file));
            goto found_manifest;
        }
    }

    utstring_renew(manifest_file);
    utstring_printf(manifest_file, "%s/../MANIFEST", launch_dir);
    if (debug)
        log_debug("accessing launch_dir: %s", utstring_body(manifest_file));
    int rc = access(utstring_body(manifest_file), R_OK);
    if (rc == 0) {
        if (debug)
            log_debug("using launch_dir: %s", launch_dir);
        goto found_manifest;
    }

    printf("argv0: %s\n", argv0);
    utstring_renew(manifest_file);
    utstring_printf(manifest_file, "%s/MANIFEST", argv0);
    if (debug)
        log_debug("accessing launch_dir: %s", utstring_body(manifest_file));
    rc = access(utstring_body(manifest_file), R_OK);
    if (rc == 0) {
        if (debug)
            log_debug("using argv0: %s", launch_dir);
        goto found_manifest;
    }

 found_manifest: ;

    FILE * fpManifest;
    char * line = NULL;
    int ch, line_ct = 0;
    size_t len = 0;
    ssize_t read;
    fpManifest = fopen(utstring_body(manifest_file), "r");
    if (fpManifest == NULL) {
        log_error("fopen failure %s", utstring_body(manifest_file));
        exit(EXIT_FAILURE);
    }

    for (ch = getc(fpManifest); ch != EOF; ch = getc(fpManifest))
        if (ch == '\n') line_ct++;
    if (debug)
        log_debug(RED "line ct:" CRESET " %d", line_ct);

    struct runfiles_s *runfiles = calloc(sizeof(struct runfiles_s),
                                         line_ct + 1);

    rewind(fpManifest);

    if (debug) log_debug("Reading MANIFEST");
    int i = 0;
    while ((read = getline(&line, &len, fpManifest)) != -1) {
        line[strcspn(line, "\n")] = '\0';    /* trim trailing newline */

        /* two tokens per line, first is path relative to exec dir,
           second is corresponding absolute path */
        /* char *token; */
        const char *sep = " ";

        char **ap, *kv[2];
        for (ap = kv; (*ap = strsep(&line, sep)) != NULL;)
            if (**ap != '\0')
                if (++ap >= &kv[2])
                    break;
        if (debug) {
            printf(RED "kv[0]:" CRESET " %s\n", kv[0]);
            printf(RED "kv[1]:     %s\n", kv[1]);
        }
        runfiles[i].key = strdup(kv[0]);
        runfiles[i].val = strdup(kv[1]);
        i++;
    }
    fclose(fpManifest);
    if (debug)
        log_debug(BLU "DONE" CRESET);

    return runfiles;
}

EXPORT void runfiles_delete(struct runfiles_s *runfiles)
{
    free(runfiles);
}

/* EXPORT UT_array *runfiles_array(void) */
/* { */
/*     printf("manifest_path\n"); */

/*     /\* at launch: argv[0] is pgmname path, */
/*        and cwd is argv[0].runfiles/__main__/ */
/*        the manifest is in two places: */
/*        argv[0].runfiles_manifest, and */
/*        argv[0].runfiles/MANIFEST */
/*        (== cwd/../MANIFEST) */
/*      *\/ */

/*     char *runfiles_root = getcwd(NULL, 0); */
/*     printf("runfiles_root: %s\n", runfiles_root); */

/*     char *manifest = "../MANIFEST"; */
/*     rc = access(manifest, R_OK); */

/*     if (rc) { */
/*         if (verbose) log_error("must be run by bazel..."); */
/*         exit(EXIT_FAILURE); */
/*     } else { */
/*         if (verbose) log_info("Configuring for bazel env."); */
/*     } */
/* } */

/* lookup file in manifest  */
/* EXPORT void get_runfile(char *file) */
/* { */
/*     UT_string *runfile; */
/*     utstring_new(runfile); */
/*     utstring_printf(runfile, */
/*                     "%s/external/opam/man/%s/@%s.1", */
/*                     runfiles_root, */
/*                     section, */
/*                     manpage); */
/*     printf("page src: %s\n", utstring_body(runfile)); */

/*     char *argv[] = { */
/*         "man", */
/*         utstring_body(pagesrc), */
/*         NULL */
/*     }; */

/*     int argc = (sizeof(argv) / sizeof(argv[0])) - 1; */
/*     if (verbose) */
/*         printf("displaying manpage %s\n", utstring_body(pagesrc)); */
/*     result = spawn_cmd_with_stdout(exe, argc, argv); */
/*     if (result != 0) { */
/*         fprintf(stderr, "FAIL: spawn_cmd_with_stdout for man\n"); */
/*         exit(EXIT_FAILURE); */
/*         /\* } else { *\/ */
/*         /\*     printf("install result: %s\n", result); *\/ */
/*     } */
/*     return; */
/* } */
