#include <errno.h>
#include <fcntl.h>              /* open, etc. */

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>             /* close */

#include "ini.h"
#include "log.h"
#include "utarray.h"
#include "uthash.h"

#include "obazl_watch_client.h"

void invalid_watch_arg(char *watch_arg)
{
    log_error("Invalid subcommand: %s", watch_arg);
    log_info("Allowed subcommands: start, stop, restart, status");
}

int get_daemon_status(void) // (UT_string *fifo_name, int fifo_fd)
{
    log_debug("get_daemon_status");

    /* Fifos created by daemon, client just opens them */

    if (fifo_to_server_path == NULL) {
        log_error("fifo_to_server_path == NULL");
        /* server down - obazl_watch_configure() has not been run */
        obazl_watch_configure_client();
        /* return -1; */
    }
    log_debug("opening %s for writing", utstring_body(fifo_to_server_path));

    fifo_to_server_fd = open(utstring_body(fifo_to_server_path), O_WRONLY | O_NONBLOCK);
    if (fifo_to_server_fd < 0) {
        if (errno == ENOENT) {
            /* server is down */
            log_warn("ENOENT: no such file or directory (i.e. server down)");
            return -1;
        } else {
            if (errno == ENXIO) {
                log_debug("ENXIO: Device not configured (i.e. server down)");
                return -1;
            } else {
                perror(utstring_body(fifo_to_server_path));
                log_fatal("client: failed to open fifo_to_server for writing");
                exit(EXIT_FAILURE);
            }
        }
    } else {
        log_debug("opened fifo %d for writing", fifo_to_server_fd);
    }

    log_debug("writing 'status' to fifo_to_server");
    ssize_t write_ct = write(fifo_to_server_fd, "status", 7);
    if (write_ct != 7) {
        int errnum = errno;
        perror(utstring_body(fifo_to_server_path));
        log_error("failed to write 'status' to fifo_to_server");
        return -1;
    }
    close(fifo_to_server_fd);

    /* get response */
    log_debug("opening fifo_to_client for reading");
    fifo_to_client_fd = open(utstring_body(fifo_to_client_path), O_RDONLY); // | O_NONBLOCK);
    if (fifo_to_client_fd < 0) {
        perror(utstring_body(fifo_to_client_path));
        log_error("client: failed to open fifo_to_client for reading");
        // if (errno == ENOENT) { /* No such file or directory */
        exit(EXIT_FAILURE);
    } else {
        log_debug("opened fifo_to_client %d for reading", fifo_to_client_fd);
    }

    /* Server sends pid in response to 'status' msg */
    int watcher_pid;
    log_debug("reading fifo_to_client");
    ssize_t sz = read(fifo_to_client_fd, &watcher_pid, sizeof(watcher_pid));
    /* log_debug("readed %d chars from fifo_fd", sz); */

    /* MacOS: non-blocking and interrupt i/o */
    /* #define EAGAIN          35              /\* Resource temporarily unavailable *\/ */
    /* #define EWOULDBLOCK     EAGAIN          /\* Operation would block *\/ */
    /* #define EINPROGRESS     36              /\* Operation now in progress *\/ */
    /* #define EALREADY        37              /\* Operation already in progress *\/ */
    if (sz < 0) {
        int errnum = errno;
        if (errnum == EAGAIN) {         /* Resource temporarily unavailable */
            log_error("EAGAIN: Resource temporarily unavailable");
            /* if (read_ct < 10) { */
                /* close(fifo_fd); */
                /* log_debug("closed fifo_fd"); */
                /* nanosleep((const struct timespec[]){{0, 500000000L}}, NULL); */
                /* fifo_fd = open(utstring_body(fifo_name), O_RDONLY | O_NONBLOCK); */
                /* if (fifo_fd < 0) { */
                /*     log_fatal("failed to open %s for read for %dd time: %d", fifo_name, read_ct, errno); */
                /*     exit(EXIT_FAILURE); */
                /* } */
                /* log_debug("repopened fifo_fd"); */
                /* read_ct++; */
                /* goto reread; */
            /* } */
        }
        perror(utstring_body(fifo_to_client_path));
        log_error("read fail %d on fifo %s", errnum, utstring_body(fifo_to_client_path));
        close(fifo_to_client_fd);
        exit(EXIT_FAILURE);
    } else {
        /* log_debug("obazl_watch status: up (pid %d)", watcher_pid); */
        close(fifo_to_client_fd);

        return watcher_pid;
    }
}

/**
   obazl_watch_controller: runs in client code, communicates with server via named pipes
   args:
   watch_arg: one of start, stop, restart, status
 */
void obazl_watch_controller(char *watch_arg)
{
    log_debug("obazl_watch_controller, cmd: %s", watch_arg);

    obazl_watch_configure_client();

    int rc;

    /* /\* server creates both fifos *\/ */

    if (strncmp(watch_arg, "start", 5) == 0) {
        if (strlen(watch_arg) == 5) {
            log_debug("starting");
            // Step 1: ping to see if its already running
            /* if (fifo_open) { */

            int pid = get_daemon_status(); //(obazl_watch_fifo, fifo_fd);
            if (pid < 0) {
                log_debug("starting obazl_watch service");
                fprintf(stdout, "starting obazl_watch service 1");
                fflush(stdout);

                rc = obazl_watch_start();

            } else {
                fprintf(stdout, "01 obazl_watch already up (pid %d); did you mean 'restart' or 'status'?\n", pid);
            }
        } else {
            invalid_watch_arg(watch_arg);
            /* if (fifo_open) close(fifo_fd); */
            exit(EXIT_FAILURE);
        }
    } else {
        if (strncmp(watch_arg, "stop", 4) == 0) {
            log_debug("stop...%d", strlen(watch_arg));
            if (strlen(watch_arg) == 4) {
                log_debug("stop fifo_to_server_path ptr %p", fifo_to_server_path);
                log_debug("stop - opening %s", utstring_body(fifo_to_server_path));
                /* if (fifo_open) { */
                /*     close(fifo_fd); */
                fifo_to_server_fd = open(utstring_body(fifo_to_server_path), O_WRONLY | O_NONBLOCK );
                if (fifo_to_server_fd < 0) {
                    perror(utstring_body(fifo_to_server_path));
                    log_fatal("failed to open fifo_to_server for writing");
                    exit(EXIT_FAILURE);
                } else {
                    log_debug("writing 'stop' to %s", utstring_body(fifo_to_server_path));
                    ssize_t ct = write(fifo_to_server_fd, "stop", 5);
                    errno = 0;
                    if (ct < 0) {
                        int errnum = errno;
                        perror("fifo_to_server");
                        log_error("write error %d on fifo_to_server_fd", errnum);
                    }
                    errno = 0;
                    ct = close(fifo_to_server_fd);
                    if (ct != 0) {
                        int errnum = errno;
                        perror("fifo_to_server");
                        log_error("close error %d on fifo_to_server_fd", errnum);
                    }
                    sleep(2);
                    fflush(stdout);
                    fflush(stderr);
                    _Exit(EXIT_SUCCESS);
                }
            } else {
                log_debug("xxxxxxxxxxxxxxxxy");
                invalid_watch_arg(watch_arg);
                /* if (fifo_open) close(fifo_fd); */
                exit(EXIT_FAILURE);
            }
        } else {
            if (strncmp(watch_arg, "status", 6) == 0) {
                if (strlen(watch_arg) > 6) {
                    invalid_watch_arg(watch_arg);
                    /* close(fifo_fd); */
                    exit(EXIT_FAILURE);
                } else {
                    int pid = get_daemon_status(); // (obazl_watch_fifo, fifo_fd);
                    if (pid < 0) {
                        /* fprintf(stdout, "starting obazl_watch service"); */
                        /* rc = obazl_watch_start(); */
                        fprintf(stdout, "00 obazl_watch status: dn\n");
                    } else {
                        fprintf(stdout, "01 obazl_watch status: up (pid %d)\n", pid);
                    }
                    /* if (pid) { */
                    /*     fprintf(stdout, "obazl_watch status 2: up (pid %d)\n", pid); */
                    /* } else { */
                    /*     fprintf(stdout, "starting obazl_watch service"); */
                    /*     rc = obazl_watch_start(); */
                    /* } */
                }
            } else {
                if (strncmp(watch_arg, "restart", 7) == 0) {
                    if (strlen(watch_arg) > 7) {
                        invalid_watch_arg(watch_arg);
                        /* close(fifo_fd); */
                        exit(EXIT_FAILURE);
                    } else {
                        fprintf(stdout, "restarting obazl_watch\n");
                        close(fifo_to_server_fd);
                        /* fifo_to_server_fd = open(utstring_body(fifo_to_server_path), O_WRONLY | O_NONBLOCK); */
                        /* /\* fifo_to_server_fd = open(utstring_body(fifo_to_server_path), O_WRONLY); *\/ */
                        /* if (fifo_to_server_fd < 0) { */
                        /*     perror(utstring_body(fifo_to_server_path)); */
                        /*     log_fatal("failed to open fifo_to_server for writing"); */
                        /*     exit(EXIT_FAILURE); */
                        /* } else { */
                        fifo_to_server_fd = open(utstring_body(fifo_to_server_path), O_WRONLY | O_NONBLOCK);
                        if (fifo_to_server_fd < 0) {
                            if (errno == ENOENT) {
                                /* server is down */
                                log_warn("ENOENT: no such file or directory (i.e. server down)");
                                /* return -1; */
                            } else {
                                if (errno == ENXIO) {
                                    log_debug("ENXIO: Device not configured (i.e. server down)");
                                    /* return -1; */
                                } else {
                                    perror(utstring_body(fifo_to_server_path));
                                    log_fatal("client: failed to open fifo_to_server for writing");
                                    exit(EXIT_FAILURE);
                                }
                            }
                        } else {
                            log_debug("opened fifo %d for writing", fifo_to_server_fd);
                        }
                        log_debug("writing 'stop' to %s", utstring_body(fifo_to_server_path));
                        ssize_t write_ct = write(fifo_to_server_fd, "stop", 5);
                        if (write_ct != 5) {
                            int errnum = errno;
                            perror(utstring_body(fifo_to_server_path));
                            log_error("failed to write 'status' to fifo_to_server");
                            /* return -1; */
                        }
                        fsync(fifo_to_server_fd);
                        close(fifo_to_server_fd);
                        /* now restart */
                        /* nanosleep((const struct timespec[]){{0, 5000000000L}}, NULL); */
                        /* sleep(4); */
                        log_debug("restarting obazl_watch service");
                        rc = obazl_watch_start();
                        exit(EXIT_SUCCESS);
                    }
                } else {
                    invalid_watch_arg(watch_arg);
                    exit(EXIT_FAILURE);
                }
            }
        }
        /* close(fifo_fd); */
    }
}
