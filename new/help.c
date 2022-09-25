#include <errno.h>
#include <spawn.h>
#include <stdbool.h>
#include <unistd.h>

#include "log.h"

#include "utstring.h"

#include "help.h"

int spawn_cmd_with_stdout(char *executable, int argc, char *argv[])
{
#if defined(DEBUG_TRACE)
    log_trace("spawn_cmd_with_stdout");
#endif
    if (verbose) { // || dry_run) {
        UT_string *cmd_str;
        utstring_new(cmd_str);
        for (int i =0; i < argc; i++) {
            utstring_printf(cmd_str, "%s ", (char*)argv[i]);
        }
        /* log_info("%s", utstring_body(cmd_str)); */
        /* log_info("obazl:"); */
        printf(YEL "EXEC: " CMDCLR);
        printf("%s ", utstring_body(cmd_str));
        printf(CRESET "\n");
    }

    /* if (dry_run) return 0; */

    pid_t pid;
    int rc;

    extern char **environ;

    errno = 0;
    rc = posix_spawnp(&pid, executable, NULL, NULL, argv, environ);

    if (rc == 0) {
#if defined(DEBUG_TRACE)
        /* log_trace("posix_spawn child pid: %i\n", pid); */
#endif
        errno = 0;
        int waitrc = waitpid(pid, &rc, WUNTRACED);
        if (waitrc == -1) {
            perror("spawn_cmd waitpid error");
            /* log_error("spawn_cmd"); */
            /* posix_spawn_file_actions_destroy(&action); */
            return -1;
        } else {
#if defined(DEBUG_TRACE)
        /* log_trace("waitpid rc: %d", waitrc); */
#endif
            // child exit OK
            if ( WIFEXITED(rc) ) {
                // terminated normally by a call to _exit(2) or exit(3).
#if defined(DEBUG_TRACE)
                /* log_trace("WIFEXITED(rc)"); */
                /* log_trace("WEXITSTATUS(rc): %d", WEXITSTATUS(rc)); */
#endif
                /* log_debug("WEXITSTATUS: %d", WEXITSTATUS(rc)); */
                /* "widow" the pipe (delivers EOF to reader)  */
                /* close(stdout_pipe[1]); */
                /* dump_pipe(STDOUT_FILENO, stdout_pipe[0]); */
                /* close(stdout_pipe[0]); */

                /* /\* "widow" the pipe (delivers EOF to reader)  *\/ */
                /* close(stderr_pipe[1]); */
                /* dump_pipe(STDERR_FILENO, stderr_pipe[0]); */
                /* close(stderr_pipe[0]); */

                fflush(stdout);
                fflush(stderr);
                return EXIT_SUCCESS;
            }
            else if (WIFSIGNALED(rc)) {
                // terminated due to receipt of a signal
                /* log_error("WIFSIGNALED(rc)"); */
                /* log_error("WTERMSIG: %d", WTERMSIG(rc)); */
                /* log_error("WCOREDUMP?: %d", WCOREDUMP(rc)); */
                return -1;
            } else if (WIFSTOPPED(rc)) {
                /* process has not terminated, but has stopped and can
                   be restarted. This macro can be true only if the
                   wait call specified the WUNTRACED option or if the
                   child process is being traced (see ptrace(2)). */
                /* log_error("WIFSTOPPED(rc)"); */
                /* log_error("WSTOPSIG: %d", WSTOPSIG(rc)); */
                return -1;
            }
        }
        /* else { */
        /*     log_error("spawn_cmd: stopped or terminated child pid: %d", */
        /*               waitrc); */
        /*     /\* posix_spawn_file_actions_destroy(&action); *\/ */
        /*     return -1; */
        /* } */
    } else {
        /* posix_spawnp rc != 0; does not set errno */
        /* log_fatal("spawn_cmd error rc: %d, %s", rc, strerror(rc)); */
        /* posix_spawn_file_actions_destroy(&action); */
        return rc;
    }
    //  should not reach here?
    /* log_error("BAD FALL_THROUGH"); */
    return rc;
}

EXPORT void display_manpage(char *section, char *manpage) {

    printf("display_manpage: %s\n", manpage);

    char *exe = "man";
    int result;

    char *runfiles_root = getcwd(NULL, 0);

    printf("runfiles_root: %s\n", runfiles_root);

    UT_string *pagesrc;
    utstring_new(pagesrc);
    utstring_printf(pagesrc,
                    /* "%s/man/%s/%s", */
                    "%s/external/opam/man/%s/@opam_%s.1",
                    runfiles_root,
                    section,
                    manpage);
    printf("page src: %s\n", utstring_body(pagesrc));

    char *argv[] = {
        "man",
        utstring_body(pagesrc),
        NULL
    };

    int argc = (sizeof(argv) / sizeof(argv[0])) - 1;
    if (verbose)
        printf("displaying manpage %s\n", utstring_body(pagesrc));
    result = spawn_cmd_with_stdout(exe, argc, argv);
    if (result != 0) {
        fprintf(stderr, "FAIL: spawn_cmd_with_stdout for man\n");
        exit(EXIT_FAILURE);
        /* } else { */
        /*     printf("install result: %s\n", result); */
    }
    return;
}

EXPORT void execlp_manpage(char *section, char *manpage) {

    printf("display_manpage: %s\n", manpage);

    char *runfiles_root = getcwd(NULL, 0);

    /* printf("runfiles_root: %s\n", runfiles_root); */

    UT_string *src;
    utstring_new(src);
    utstring_printf(src,
                    /* "%s/man/%s/%s", */
                    "%s/external/opam/man/%s/%s",
                    runfiles_root,
                    section,
                    manpage);
    /* int rc = access(utstring_body(src), F_OK); */
    /* if (rc != 0) { */
    /*     fprintf(stderr, "not found: %s\n", utstring_body(src)); */
    /*     return; */
    /* } */
    /* printf("found manpage: %s\n", utstring_body(src)); */

    /* system(utstring_body(src)); */
    execlp("man", "--", utstring_body(src), NULL);
    return;
}
