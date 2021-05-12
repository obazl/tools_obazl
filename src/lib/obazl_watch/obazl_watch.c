#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <libfswatch/c/libfswatch.h>

#if INTERFACE
#include "utstring.h"
#endif

#include "log.h"

#include "obazl_watch.h"

UT_string *proj_root;
UT_string *obazl_d;
UT_string *obazl_watch_fifo;
UT_string *obazl_watch_log;

/* opam_dirs is effectively read-only. we initialize it before we
   launch the daemon, and we do not monitor the OPAM repo - users who
   change the repo are responsible for restarting the obazl watch
   daemon. */
UT_array *opam_dirs;

/* proj_dirs is volatile. it will change when the user adds or removes
   directories.
 */
UT_array *proj_dirs;

/* src_files is volatile. it will change when the user adds or removes
   files.
 */
UT_array *src_files;

/**
 * The following function implements the fswatch callback functionality for testing
 * eventnumber send from the libdawatch library. See FSW_CEVENT_CALLBACK for
 * details.
 *
 * @param events
 * @param event_num
 * @param data
 */
void my_callback(fsw_cevent const *const events,
                 const unsigned int event_ct,
                 void *data)
{
    log_debug("\n");
    log_debug("my_callback: event ct: %d", event_ct);
    for (int i = 0; i < event_ct; i++) {
        log_debug("event %d", i);
        log_debug("path: %s", events[i].path);
        log_debug("flags ct: %d", events[i].flags_num);
        for (int j = 0; j < events[i].flags_num; j++) {
            log_debug("\tflag: %d: %s",
                      events[i].flags[j],
                      fsw_get_event_flag_name(events[i].flags[j]));
        }
    }
}

void *controller(void *param)
{
    log_debug("running controller thread");
    FSW_HANDLE *fsw_handle = (FSW_HANDLE *) param;
    int fd;
    char str1[80];

    log_debug("mkdir %s", utstring_body(obazl_d));
    int rc = mkdir(utstring_body(obazl_d), S_IRWXU | S_IRGRP | S_IWGRP);
    if (rc != 0) {
        if (errno != EEXIST) {
            perror(utstring_body(obazl_d));
            log_error("mkdir error");
        }
    }
    rc = mkfifo(utstring_body(obazl_watch_fifo), S_IRWXU | S_IRGRP | S_IWGRP);
    if (rc < 0) {
        if (errno != EEXIST) {
            perror(utstring_body(obazl_watch_fifo));
            log_error("mkdir error");
        } else {
            log_error("fifo exists");
        }
    }

    while (1) {
        str1[0] = '\0';
        fd = open(utstring_body(obazl_watch_fifo),O_RDWR);
        if (fd < 0) {
            perror(utstring_body(obazl_watch_fifo));
            log_fatal("failed to open obazl_watch_fifo for reading");
            exit(EXIT_FAILURE);
        }
        ssize_t sz = read(fd, str1, 80);
        if (sz < 0) {
            perror(utstring_body(obazl_watch_fifo));
            log_error("failed to read fifo %s", utstring_body(obazl_watch_fifo));
        }

        log_debug("controller readed: '%s'", str1);
        if (strncmp(str1, "stop", 4) == 0) {
            /* restore stdio */
            /* dup2(saved_stdout, STDOUT_FILENO); */
            /* dup2(saved_stderr, STDERR_FILENO); */

            rc = obazl_watch_stop(fsw_handle);
            log_debug("obazl watch stop: %d", rc);
            // write to pipe
            // if rc != 0 ???
            close(fd);
            remove(utstring_body(obazl_watch_fifo));
            return 0;
        } else {
            if (strncmp(str1, "status", 4) == 0) {
                // TODO: list dir under observation
                char msg[80];
                memset(&msg, '\0', 80);
                sprintf(msg, "obazl_watch status: up (pid %d)", getpid());
                log_debug(msg);
                write(fd, msg, strlen(msg) + 1);
                fsync(fd);
                close(fd);
            } else {
                if (strncmp(str1, "restart", 4) == 0) {
                    log_debug("restarting obazl watcher");
                }
            }
        }
        close(fd);
    }
}

/**
   fswatch monitoring
*/
void *start_monitor(void *param)
{
  FSW_HANDLE *handle = (FSW_HANDLE *) param;

  if (FSW_OK != fsw_start_monitor(*handle))
  {
    fprintf(stderr, "Error creating thread\n");
  }
  else
  {
    printf("Monitor stopped \n");
  }

  return NULL;
}

/* FSW_HANDLE handle; */

int obazl_watch_stop(FSW_HANDLE *handle)
{
    log_debug("stopping obazl fswatch");
    if (FSW_OK != fsw_stop_monitor(*handle)) {
        fprintf(stderr, "Error stopping monitor \n");
        return 1;
    }

    log_debug("destroying session");
    if (FSW_OK != fsw_destroy_session(*handle)) {
        fprintf(stderr, "Error destroying session\n");
        return 1;
    }
    return 0;
}

int obazl_watch_start(void *dir) // (char *dir)
{
        if (FSW_OK != fsw_init_library()) {
            fsw_last_error();
            printf("libfswatch cannot be initialised!\n");
            return 1;
        }

        /* const FSW_HANDLE handle = fsw_init_session( */
        FSW_HANDLE handle = fsw_init_session(
                                             /* system_default_monitor_type */
                                             fsevents_monitor_type /* OS X FSEvents monitor */
                                             );

        if (FSW_INVALID_HANDLE == (int)handle) {
            fsw_last_error();
            printf("Invalid fswatch handle: %d\n", handle);
            return 2;
        }


        /* for (int i = 1; i < argc; i++) */
        /*     { */
        /*         if (FSW_OK != fsw_add_path(handle, argv[i])) */
        /*             { */
        /*                 fsw_last_error(); */
        /*             } */
        /*     } */

        if (FSW_OK != fsw_add_path(handle, dir)) {
            fsw_last_error();
        }

        log_debug("adding watch filters");

        /* NB: event type filters do not seem to work on MacOS at least */
        /* At least, no evident way to filter with multiple flags */
        /* fsw_event_type_filter ev_filter; */
        /* enum fsw_event_flag evflag = Updated; */
        /* ev_filter.flag = evflag; */
        /* if (FSW_OK != fsw_add_event_type_filter(handle, ev_filter)) { */
        /*     log_error("Error on fsw_add_event_type_filter Updated | Removed"); */
        /*     fsw_last_error(); */
        /* } */

        /* ev_filter.flag = IsFile; */
        /* if (FSW_OK != fsw_add_event_type_filter(handle, ev_filter)) { */
        /*     log_error("Error on fsw_add_event_type_filter Updated | Removed"); */
        /*     fsw_last_error(); */
        /* } */

        // does not work:
        /* ev_filter.flag = Removed | IsFile; */
        /* if (FSW_OK != fsw_add_event_type_filter(handle, ev_filter)) { */
        /*     log_error("Error on fsw_add_event_type_filter Updated | Removed"); */
        /*     fsw_last_error(); */
        /* } */

        fsw_cmonitor_filter filter;
        filter.text = ".obazl.d/.*";
        filter.type = filter_exclude;
        filter.case_sensitive = false;
        filter.extended = false;
        if (FSW_OK != fsw_add_filter(handle, filter)) {
            fsw_last_error();
        }
        filter.text = "tmp/.*";
        if (FSW_OK != fsw_add_filter(handle, filter)) {
            fsw_last_error();
        }
        filter.text = ".*\\.git.*";
        if (FSW_OK != fsw_add_filter(handle, filter)) {
            fsw_last_error();
        }
        /* emacs stuff */
        filter.text = ".*/\\.#.*";
        if (FSW_OK != fsw_add_filter(handle, filter)) {
            fsw_last_error();
        }
        filter.text = ".*~$";
        if (FSW_OK != fsw_add_filter(handle, filter)) {
            fsw_last_error();
        }


        if (FSW_OK != fsw_set_callback(handle, my_callback, NULL)) {
            fsw_last_error();
        }

        fsw_set_allow_overflow(handle, 0);

        pthread_t controller_thread;

        if (pthread_create(&controller_thread, NULL, controller, (void *) &handle)) {
            fprintf(stderr, "Error creating controller thread\n");
            return 1;
        }

        log_debug("starting monitor");
        if (FSW_OK != fsw_start_monitor(handle)) {
            fprintf(stderr, "Error creating thread\n");
        } else {
            log_debug("Monitor stopped");
        }

        // Wait for the controller thread to finish
        if (pthread_join(controller_thread, NULL)) {
            fprintf(stderr, "Error joining controller thread\n");
            return 2;
        }
        log_debug("exiting obazl_watch_start");
        return 0;
}

int obazl_daemon_launch(void)
{
    /* char *bwd = getenv("BUILD_WORKING_DIRECTORY"); */
    /* log_debug("BUILD_WORKING_DIRECTORY: %s", bwd); */

    log_debug("calling obazl_watch_daemonize");
    int rc = obazl_watch_daemonize(utstring_body(proj_root), NULL);
    if ( rc != EXIT_SUCCESS ) {
        log_error("Failed to start obazl_watch daemon; rc: %d", rc);
        exit(EXIT_FAILURE);
    }
    return 0;
}

int obazl_watch(void) //char *dir)
{
    log_debug("obazl_watch");

    char *bwsd = getenv("BUILD_WORKSPACE_DIRECTORY");
    log_debug("BUILD_WORKSPACE_DIRECTORY: %s", bwsd);

    utstring_new(proj_root);
    utstring_printf(proj_root, "%s", bwsd);
    log_debug("proj_root: %s", utstring_body(proj_root));

    utstring_new(obazl_d);
    utstring_printf(obazl_d, "%s/%s", bwsd, ".obazl.d");
    log_debug("obazl_d: %s", utstring_body(obazl_d));

    utstring_new(obazl_watch_fifo);
    utstring_printf(obazl_watch_fifo, "%s/%s", bwsd, ".obazl.d/obazlw.fifo");
    log_debug("obazl_watch_info: %s", utstring_body(obazl_watch_fifo));

    utstring_new(obazl_watch_log);
    utstring_printf(obazl_watch_log, "%s/%s", bwsd, ".obazl.d/obazlw.log");
    log_debug("obazl_watch_log: %s", utstring_body(obazl_watch_log));

    opam_dirs = inventory_opam();
    log_debug("opam_dirs ct: %d", utarray_len(opam_dirs));
    char **p = NULL;
    /* while ( (p=(char**)utarray_next(opam_dirs, p))) { */
    /*     printf("\t%s\n", *p); */
    /* } */
    /* utarray_free(opam_dirs); */

    // write codept.args file with -L for opam dirs?

    utarray_new(proj_dirs, &ut_str_icd);
    dirseq(utstring_body(proj_root), proj_dirs);
    log_debug("proj_dirs ct: %d", utarray_len(proj_dirs));
    /* while ( (p=(char**)utarray_next(proj_dirs, p))) { */
    /*     printf("\t%s\n", *p); */
    /* } */
    /* utarray_free(proj_dirs); */

    // append to codept.args file with -I for proj dirs?

    utarray_new(src_files, &ut_str_icd);
    fileseq(utstring_body(proj_root), src_files);
    log_debug("src_files ct: %d", utarray_len(src_files));
    /* p = NULL; */
    /* while ( (p=(char**)utarray_next(src_files, p))) { */
    /*     printf("\t%s\n", *p); */
    /* } */
    /* utarray_free(src_files); */

    return obazl_daemon_launch();
}
