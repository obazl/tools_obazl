#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include <libfswatch/c/libfswatch.h>

#if INTERFACE
#include "utstring.h"
#include "utarray.h"
#endif

#include "log.h"

#include "obazl_watch.h"

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
    /* assumption: opam dirs are stable; user will restart daemon if they change, so no need to refresh */
    /* opam_dirs = inventory_opam(); */

    /* OPTIMIZATION: any changes to the list of srcs should be
       detected by fswatch, so use the change events to manage the
       list, instead of running fileseq every time. */

    /* refresh list of src files, used to parameterize codept */
    fileseq(utstring_body(proj_root), obazl_config.src_dirs, src_files);

    emit_codept_args(codept_args_file, opam_dirs, src_files);
    run_codept(utstring_body(codept_args_file), utstring_body(codept_deps_file));

    /* this routine also runs a diff to detect changes in the deps */
    /* a change to a src file does not imply a change in deps */
    obazl_deps_parse_file(utstring_body(codept_deps_file));

    for (int i = 0; i < event_ct; i++) {
        log_debug("event %d", i);
        log_debug("path: %s", events[i].path);
        log_debug("flags ct: %d", events[i].flags_num);
        for (int j = 0; j < events[i].flags_num; j++) {
            log_debug("\tflag: %d: %s",
                      events[i].flags[j],
                      fsw_get_event_flag_name(events[i].flags[j]));
        }
        l_on_change((fsw_cevent*)&events[i]);
    }

    /* dump_codept_dirty_filedeps(); */
    /* UT_array *strs; */
    /* char *s, **p = NULL; */
    /* char *result; */
    /* while ( (p=(char**)utarray_next(dirty_fdeps, p))) { */
    /*     log_debug("dirty: %s",*p); */
    /* result = (char*)emit_build_lua(*p); */
    /*     log_debug("emit_build_lua returned: %s", result); */
    /*     /\* load_lua(*p); *\/ */
    /* } */
    /* l_on_change(); */

    /* dump_codept_modules(); */
    /* dump_codept_filedeps(); */
}

/**
   fsw_controller: runs in daemon on a dedicated thread.
   manages communication with clients via named pipes.
   prereq: runs after obazl_watch_configure()
 */
void *fsw_controller(void *param)
{
    log_debug("running fsw_controller thread, pid: %d, tid: %d", getpid(), pthread_self());
    FSW_HANDLE *fsw_handle = (FSW_HANDLE *) param;
    char fifo_buf[80];

    /* Fifos should have been configured by obazl_watch_configure() */
    while (1) {
        /* log_debug("fsw_controller: opening %s for blocking read", utstring_body(fifo_to_server_path)); */
        fifo_to_server_fd = open(utstring_body(fifo_to_server_path), O_RDONLY ); //  | O_NONBLOCK);
        if (fifo_to_server_fd < 0) {
            perror(utstring_body(fifo_to_server_path));
            // if (errno == ENOENT) { /* No such file or directory */
            log_error("fopen fifo_to_server failure");
            exit(EXIT_FAILURE);
        } else {
            /* fifo_open = true; */
            log_debug("fsw_controller opened fifo %s for reading", utstring_body(fifo_to_server_path));
        }

        fifo_buf[0] = '\0';
        /* log_debug("executing blocking read on fifo_to_server"); */
        ssize_t sz = read(fifo_to_server_fd, fifo_buf, 80);
        if (sz < 0) {
            int errnum = errno;
            perror(utstring_body(fifo_to_server_path));
            log_error("fifo read fail errno %d, sz %d, on %s", errno, sz, utstring_body(fifo_to_server_path));
        }
        close(fifo_to_server_fd);

        log_debug("fsw_controller readed: '%s'", fifo_buf);
        if (strncmp(fifo_buf, "stop", 4) == 0) {
            /* restore stdio */
            /* dup2(saved_stdout, STDOUT_FILENO); */
            /* dup2(saved_stderr, STDERR_FILENO); */

            rc = obazl_watch_stop(fsw_handle);
            log_debug("obazl watch stop: %d", rc);
            // write to pipe
            // if rc != 0 ???
            close(fifo_to_server_fd);
            unlink(utstring_body(fifo_to_server_path));
            close(fifo_to_client_fd);
            unlink(utstring_body(fifo_to_client_path));
            fclose(obazl_watch_log_fp);
            return 0;
        } else {
            if (strncmp(fifo_buf, "status", 4) == 0) {
                log_debug("writing status %d to fifo", getpid());
                // TODO: list dirs under observation
                /* memset(&msg, '\0', 80); */
                /* sprintf(msg, "%d", getpid()); */
                /* log_debug(msg); */
                /* write(fifo_to_client_fd, msg, strlen(msg) + 1); */

                log_debug("opening fifo_to_client for writing (blocking)");
                fifo_to_client_fd = open(utstring_body(fifo_to_client_path), O_WRONLY ); // | O_NONBLOCK);
                if (fifo_to_client_fd < 0) {
                    // if (errno == ENOENT) { /* No such file or directory */
                    perror(utstring_body(fifo_to_client_path));
                    log_error("fopen fifo_to_client failure");
                    /* exit(EXIT_FAILURE); */
                } else {
                    /* fifo_open = true; */
                    log_debug("opened fifo %d for writing", fifo_to_client_fd);
                }
                log_debug("writing pid to %s", utstring_body(fifo_to_client_path));
                int n = getpid();
                write(fifo_to_client_fd, &n, sizeof(n));
                /* fsync(fd); */
                close(fifo_to_client_fd);
            } else {
                if (strncmp(fifo_buf, "restart", 4) == 0) {
                    log_debug("restarting obazl watcher");
                } else {
                    log_warn("unexpected fifo read");
                }
            }
        }
    }
    log_debug("closing watcher fifos");
    close(fifo_to_server_fd);
    close(fifo_to_client_fd);
}

/**
   fswatch monitoring
*/
/* void *start_monitor(void *param) */
/* { */
/*     log_debug("start_monitor"); */
/*     FSW_HANDLE *handle = (FSW_HANDLE *) param; */

/*     if (FSW_OK != fsw_start_monitor(*handle)) { */
/*         log_fatal("fsw_start_monitor failure"); */
/*         fprintf(stderr, "Error creating thread\n"); */
/*     } else { */
/*         log_info("Monitor stopped \n"); */
/*         printf("Monitor stopped \n"); */
/*     } */

/*     return NULL; */
/* } */

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

/**
   called from daemonize (FIXME: move to daemonize.c?)
 */
int obazl_fswatch_start(void *dir) // (char *dir)
{
    log_debug("obazl_fswatch_start, pid %d, tid %d", getpid(), pthread_self());
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

    /* if (0) { */
    if (obazl_config.watch_dirs) {
        fsw_cmonitor_filter filter;

        /* first exclude everything ... */
        filter.text = ".*";
        filter.type = filter_exclude;
        filter.case_sensitive = false;
        filter.extended = false;
        if (FSW_OK != fsw_add_filter(handle, filter)) {
            fsw_last_error();
        }

        /* problem: dune _build dir contains src files */
        /* problem: hidden dirs, e.g. .git */
        /* ... then include what we want */
        filter.type = filter_include;

        UT_string *workstr;
        utstring_new(workstr);
        char *projroot = utstring_body(proj_root);
        char **p = NULL;
        while ( (p=(char**)utarray_next(obazl_config.watch_dirs, p))) {
            utstring_clear(workstr);
            utstring_printf(workstr, "%s/%s/.*\\.ml$", projroot, *p);
            log_debug("adding filter: %s", utstring_body(workstr));
            filter.text = utstring_body(workstr);
            if (FSW_OK != fsw_add_filter(handle, filter)) {
                log_error("add filter error: %d", fsw_last_error());
                fsw_last_error();
            }
            utstring_clear(workstr);
            utstring_printf(workstr, "%s/%s/.*\\.mli$", projroot, *p);
            log_debug("adding filter: %s", utstring_body(workstr));
            filter.text = utstring_body(workstr);
            if (FSW_OK != fsw_add_filter(handle, filter)) {
                log_error("add filter error: %d", fsw_last_error());
                fsw_last_error();
            }
            utstring_clear(workstr);
            utstring_printf(workstr, "%s/%s/.*/dune$", projroot, *p);
            log_debug("adding filter: %s", utstring_body(workstr));
            filter.text = utstring_body(workstr);
            if (FSW_OK != fsw_add_filter(handle, filter)) {
                log_error("add filter error: %d", fsw_last_error());
                fsw_last_error();
            }

        }
        utstring_free(workstr);
    } else {
        log_info("No filters specified; watching everything");
    }

    /* filter.text = "\\.mli$"; */
    /* filter.type = filter_include; */
    /* filter.case_sensitive = false; */
    /* filter.extended = false; */
    /* if (FSW_OK != fsw_add_filter(handle, filter)) { */
    /*     fsw_last_error(); */
    /* } */

    /* filter.text = "/dune$"; */
    /* filter.type = filter_include; */
    /* filter.case_sensitive = false; */
    /* filter.extended = false; */
    /* if (FSW_OK != fsw_add_filter(handle, filter)) { */
    /*     fsw_last_error(); */
    /* } */

    /* filter.text = ".obazl.d/.*"; */
    /* filter.type = filter_exclude; */
    /* filter.case_sensitive = false; */
    /* filter.extended = false; */
    /* if (FSW_OK != fsw_add_filter(handle, filter)) { */
    /*     fsw_last_error(); */
    /* } */

    /* filter.text = "tmp/.*"; */
    /* if (FSW_OK != fsw_add_filter(handle, filter)) { */
    /*     fsw_last_error(); */
    /* } */
    /* filter.text = ".*\\.git.*"; */
    /* if (FSW_OK != fsw_add_filter(handle, filter)) { */
    /*     fsw_last_error(); */
    /* } */
    /* /\* emacs stuff *\/ */
    /* filter.text = ".*\/\\.#.*"; */
    /* if (FSW_OK != fsw_add_filter(handle, filter)) { */
    /*     fsw_last_error(); */
    /* } */
    /* filter.text = ".*~$"; */
    /* if (FSW_OK != fsw_add_filter(handle, filter)) { */
    /*     fsw_last_error(); */
    /* } */


    if (FSW_OK != fsw_set_callback(handle, my_callback, NULL)) {
        fsw_last_error();
    }

    fsw_set_allow_overflow(handle, 0);

    pthread_t fsw_controller_thread;

    log_debug("obazl_fswatch_start: creating fsw_controller_thread from pid %d, thread %d", getpid(), pthread_self());
    if (pthread_create(&fsw_controller_thread, NULL, fsw_controller, (void *) &handle)) {
        fprintf(stderr, "Error creating fsw_controller thread\n");
        return 1;
    }

    log_debug("starting monitor");
    if (FSW_OK != fsw_start_monitor(handle)) {
        log_debug("Error on fsw_start_monitor\n");
        fprintf(stderr, "Error on fsw_start_monitor\n");
    } else {
        log_debug("Monitor stopped");
    }

    // Wait for the fsw_controller thread to finish
    if (pthread_join(fsw_controller_thread, NULL)) {
        fprintf(stderr, "Error joining fsw_controller thread\n");
        return 2;
    }
    log_debug("exiting obazl_watch_start");
    return 0;
}

int obazl_daemon_launch(void)
{
    log_debug("obazl_daemon_launch pid %d, tid %d", getpid(), pthread_self());
    /* char *bwd = getenv("BUILD_WORKING_DIRECTORY"); */
    /* log_debug("BUILD_WORKING_DIRECTORY: %s", bwd); */

    /* log_debug("calling obazl_watch_daemonize"); */
    int rc = obazl_watch_daemonize(utstring_body(proj_root), NULL);
    if ( rc != EXIT_SUCCESS ) {
        log_error("Failed to start obazl_watch daemon; rc: %d", rc);
        exit(EXIT_FAILURE);
    }
    log_debug("obazl_daemon_launch exit");
    return 0;
}

int obazl_watch_start(void) //char *dir)
{
    log_debug("obazl_watch_start pid %d, tid %d", getpid(), pthread_self());

    obazl_watch_configure_server();

    /* FIFOs for communication with client */
    obazl_config_fifos();

    /* obazl_config_lua(proj_root, obazl_d); */

    /* initial write of build files, before we start watching for changes */
    /* list opam dirs, to parameterize codept */
    opam_dirs = inventory_opam();
    /* dump_opam_dirs(); */

    /* list source files, to parameterize codept */
    fileseq(utstring_body(proj_root), obazl_config.src_dirs, src_files);
    /* dump_src_files(); */

    emit_codept_args(codept_args_file, opam_dirs, src_files);
    run_codept(utstring_body(codept_args_file), utstring_body(codept_deps_file));
    obazl_deps_parse_file(utstring_body(codept_deps_file));

    /* dump_codept_modules(); */
    /* dump_codept_filedeps(); */

    l_init();           /* initial write build files */

    int rc =  obazl_daemon_launch();
    log_debug("obazl_watch_start exit pid %d, tid %d", getpid(), pthread_self());
}
