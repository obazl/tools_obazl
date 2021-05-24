#include <errno.h>
#include <fcntl.h>
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>

#include "log.h"
#include "utstring.h"

#include "daemonize.h"

/* https://sandervanderburg.blogspot.com/2020/01/writing-well-behaving-daemon-in-c.html */
/* https://chaoticlab.io/c/c++/unix/2018/10/01/daemonize.html */
/* https://thinkiii.blogspot.com/2009/12/double-fork-to-avoid-zombie-process.html */
/* https://lloydrochester.com/post/c/unix-daemon-example/ */

/* https://sandervanderburg.blogspot.com/2020/01/writing-well-behaving-daemon-in-c.html
 *
 * Copyright 2020 Sander van der Burg
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/* https://github.com/KoynovStas/daemon_templates
 *
 * Copyright (c) 2015, Koynov Stas - skojnov@yandex.ru
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

#define TRUE 1
#define FALSE 0
#define BUFFER_SIZE 10

enum daemon_status_e
{
    STATUS_INIT_SUCCESS                  = 0x0,
    STATUS_CANNOT_ATTACH_STD_FDS_TO_NULL = 0x1,
    STATUS_CANNOT_CHDIR                  = 0x2,
    STATUS_CANNOT_CREATE_PID_FILE        = 0x3,
    STATUS_CANNOT_INIT_DAEMON            = 0x4,
    STATUS_CANNOT_UNLINK_PID_FILE        = 0x5,
    STATUS_CANNOT_CLOSE_NON_STD_FDS      = 0x6,
    STATUS_CANNOT_RESET_SIGNAL_HANDLERS  = 0x7,
    STATUS_CANNOT_CLEAR_SIGNAL_MASK      = 0x8,
    STATUS_CANNOT_CREATE_PIPE            = 0x9,
    STATUS_CANNOT_FORK_ONE               = 0xa,
    STATUS_CANNOT_READ_FROM_PIPE         = 0xb,
    STATUS_CANNOT_SET_SID                = 0xc,
    STATUS_CANNOT_FORK_FSWATCH_PROCESS    = 0xd,
    STATUS_UNKNOWN_DAEMON_ERROR          = 0xe
};

void daemon_error_exit(const char *format, ...)
{
    va_list ap;


    if( format &&  *format )
    {
        va_start(ap, format);
        fprintf(stderr, "%s: ", "obazl_watch_daemon");
        vfprintf(stderr, format, ap);
        va_end(ap);
    }


    _exit(EXIT_FAILURE);
}
/** unblock all signals
    clear_signal_mask
*/
static int clear_signal_mask(void)
{
    sigset_t set;

    int rc = sigemptyset(&set);
    /* "Currently, no errors are detected" for sigemptyset */
    rc = sigprocmask(SIG_SETMASK, &set, NULL);
    if (rc == 0) {
        return EXIT_SUCCESS;
    } else {
        if (rc == -1) {
            perror("sigprocmask with empty set");
            exit(EXIT_FAILURE);
        } else {
            log_fatal("unexpected rc for sigprocmask: %d", rc);
            return EXIT_FAILURE;
        }
    }
}

/* void sig_termination_handler(int sig) */
void  sig_termination_handler(int sig, siginfo_t * info, void * context)
{
    //Here we release resources
    /* unlink(daemon_info.pid_file); */
    // close sockets
    _exit(EXIT_FAILURE);
}

void set_sig_handler(int sig, struct sigaction *act)
{
    int rc;
    errno = 0;
    rc = sigaction(sig, act, NULL);
    if (rc == 0) {
        return; // EXIT_SUCCESS;
    } else {
        if (rc == -1) {
            daemon_error_exit("Can't set handler for signal %d: %s\n", sig, strerror (errno));
        }
        /* else ??? */
    }
}

static int reset_signal_handlers(void)
{
    struct sigaction act;
    memset(&act, 0, sizeof(act));
    sigemptyset(&act.sa_mask);

    act.sa_handler = SIG_DFL;
    act.sa_flags = SA_RESETHAND;

#if defined _NSIG               /* glibc only */
    log_debug("_NSIG #defined");
    unsigned int i;

    for(sig = 1; sig < _NSIG; sig++)
    {
         if(sig != SIGKILL && sig != SIGSTOP)
             /* signal(sig, SIG_DFL); */
             set_sig_handler(sig, &act);
    }
    return TRUE;
#else
    /* log_debug("_NSIG not #defined"); */
    /* signals whose default action is "discard signal" */
    set_sig_handler(SIGURG, &act);  /* urgent condition present on socket */
    set_sig_handler(SIGCONT, &act); /* continue after stop */
    set_sig_handler(SIGCHLD, &act); /* child status has changed */
    set_sig_handler(SIGIO, &act); /* I/O is possible on a descriptor */
    set_sig_handler(SIGWINCH, &act);  /* Window size change */
    set_sig_handler(SIGINFO, &act); /*  status request from keyboard */
    /* signals whose default action is "terminate process" */
    set_sig_handler(SIGXCPU, &act); /* cpu time limit exceeded */
    set_sig_handler(SIGXFSZ, &act); /* file size limit exceeded */
    set_sig_handler(SIGVTALRM, &act); /* virtual time alarm */
    set_sig_handler(SIGPROF, &act);   /* profiling timer alarm */
    /* signals whose default action is "create core image" */
    set_sig_handler(SIGILL,  &act); /* illegal instruction */
    set_sig_handler(SIGTRAP, &act); /* trace trap */
    set_sig_handler(SIGABRT, &act); /* abort(3) call (formerly SIGIOT) */
    set_sig_handler(SIGEMT,  &act); /* emulate instruction executed */
    set_sig_handler(SIGFPE,  &act); /* floating point exception */

    /* set_sig_handler(SIGSTOP, &act); /\* stop (cannot be caught or ignored) *\/ */
    /* set_sig_handler(SIGKILL, &act); /\* kill program (cannot be caught or ignored) *\/ */

    /* signals whose default action we want to ignore */
    act.sa_handler = SIG_IGN;
    act.sa_flags = SA_RESETHAND;

    /* "terminate process" (or "stop process") */
    set_sig_handler(SIGPIPE, &act); /* write on a pipe with no reader */
    set_sig_handler(SIGALRM, &act); /* real-time timer expired */
    set_sig_handler(SIGTSTP,  &act); /* stop signal generated from keyboard */
    set_sig_handler(SIGTTOU,  &act); /* background read attempted from control terminal */
    set_sig_handler(SIGTTIN,  &act); /* background write attempted to control terminal */
    set_sig_handler(SIGUSR1,  &act); /* User defined signal 1 */
    set_sig_handler(SIGUSR2,  &act); /* User defined signal 2 */

    /* "create core image" */
    set_sig_handler(SIGQUIT, &act); /* quit program */

    /* termination sigs we want to handle, to clean up resources */
    act.sa_sigaction = sig_termination_handler;
    act.sa_flags = SA_SIGINFO | SA_RESTART;
    set_sig_handler(SIGINT,   &act);   /* interrupt program; default: terminate process */
    set_sig_handler(SIGTERM,  &act); /* software termination signal; default: termination process */

    /* restart watcher? */
    /* act.sa_sigaction = sig_hup_handler; */
    /* act.sa_flags = SA_SIGINFO | SA_RESTART; */
    /* set_sig_handler(SIGHUP,   &act); /\* terminal line hangup *\/ */
#endif
    return EXIT_SUCCESS;
}

/* int create_pid_file(const char *pid_file_name) */
/* { */
/*     log_debug("create_pid_file: %s", pid_file_name); */

/*     char cwd[PATH_MAX]; */
/*     int cwd_len = 0; */
/*     if (getcwd(cwd, sizeof(cwd)) != NULL) { */
/*         log_debug("Current working dir: %s", cwd); */
/*         cwd_len = strlen(cwd); */
/*     } */
/*     fflush(stderr); */
/*     fflush(stdout); */

/*     int fd; */
/*     const int BUF_SIZE = 32; */
/*     char buf[BUF_SIZE]; */

/*     if( !pid_file_name ) */
/*     { */
/*         errno = EINVAL; */
/*         return -1; */
/*     } */

/*     fd = open(pid_file_name, O_RDWR | O_CREAT, 0644); */
/*     if(fd == -1) { */
/*         perror(pid_file_name); */
/*         log_error("open pid file failed"); */
/*         return -1; // Could not create on PID file */
/*     } */
/*     log_debug("opened pid file %s", pid_file_name); */

/*     if( lockf(fd, F_TLOCK, 0) == -1 ) */
/*     { */
/*         perror(pid_file_name); */
/*         log_error("lockf failed"); */
/*         close(fd); */
/*         return -1; // Could not get lock on PID file */
/*     } */


/*     if( ftruncate(fd, 0) != 0 ) */
/*     { */
/*         perror(pid_file_name); */
/*         log_error("ftruncate failed"); */
/*         close(fd); */
/*         return -1; // Could not truncate on PID file */
/*     } */


/*     snprintf(buf, BUF_SIZE, "%ld\n", (long)getpid()); */
/*     if( write(fd, buf, strlen(buf)) != (int)strlen(buf) ) */
/*     { */
/*         perror(pid_file_name); */
/*         log_error("pidfile write failed"); */
/*         close(fd); */
/*         return -1; // Could not write PID to PID file */
/*     } */
/*     return fd; //good job */
/* } */


int config_log(void)
{
    log_debug("config_log, pid %d", getpid());
    /* log_debug("mkdir %s", utstring_body(obazl_d)); */
    int rc = mkdir(utstring_body(obazl_d), S_IRWXU | S_IRGRP | S_IWGRP);
    if (rc != 0) {
        if (errno != EEXIST) {
            perror(".obazl.d");
            log_error("mkdir error");
        }
    }
    obazl_watch_log_fp = fopen(utstring_body(obazl_watch_log), "w");
    if (obazl_watch_log_fp == NULL) {
        perror(utstring_body(obazl_watch_log));
        log_fatal("fopen logfile failure");
        exit(EXIT_FAILURE);
    }
    log_debug("opened logfile %s", utstring_body(obazl_watch_log));
    log_debug("redirecting logging to logfile");
    log_add_fp(obazl_watch_log_fp, LOG_TRACE);
}

int close_file_descriptors(void)
{
    unsigned int i;

    struct rlimit rlim;
    int num_of_fds = getrlimit(RLIMIT_NOFILE, &rlim);

    if(num_of_fds == -1)
        return FALSE;

    for(i = 3; i < num_of_fds; i++)
        close(i);

    return TRUE;
}

int saved_stdout;
int saved_stderr;

//NOTE: once we nullify std fds, we cannot log debug msgs to stdout/stderr!
int config_std_fds(void)
{
    log_debug("nulling std fds, pid %d", getpid());
    saved_stdout = dup(STDOUT_FILENO);
    saved_stderr = dup(STDERR_FILENO);

    /* log_debug("config_std_fds"); */
    int null_fd_read, null_fd_write;
#define NULL_DEV_FILE "/dev/null"
    return(((null_fd_read = open(NULL_DEV_FILE, O_RDONLY)) != -1)
      && (dup2(null_fd_read, STDIN_FILENO) != -1)
      && ((null_fd_write = open(NULL_DEV_FILE, O_WRONLY)) != -1)
      && (dup2(null_fd_write, STDOUT_FILENO) != -1)
      && (dup2(null_fd_write, STDERR_FILENO) != -1));
}

static void notify_parent_process(int writefd, enum daemon_status_e message)
{
    char byte = (char)message;
    while(write(writefd, &byte, 1) == 0);
    close(writefd);
}

static pid_t fork_fswatch_process(int pipefd,
                                 const char *pid_file,
                                 char *rootdir,
                                 void *data)
                                 /* int (*initialize_daemon) (void *data), */
                                 /* int (*run_main_loop) (void *data) */
                                 /* ) */
{
    log_debug("fork_fswatch_process entry, pid %d, tid %d", getpid(), pthread_self());
    pid_t pid = fork();
    if(pid == 0)
    {
        log_debug("fork_fswatch_process child entry, pid %d", getpid());
        /* NB: once we nullify std fds we cannot use them for log msgs! */
        /* if(!config_std_fds()) */
        /* { */
        /*     notify_parent_process(pipefd, STATUS_CANNOT_ATTACH_STD_FDS_TO_NULL); */
        /*     exit(STATUS_CANNOT_ATTACH_STD_FDS_TO_NULL); */
        /* } */

        /* config_log(); */
        umask(0);

        log_debug("fork_fswatch_process child sending SUCCESS notification");
        notify_parent_process(pipefd, STATUS_INIT_SUCCESS);

        log_debug("fork_fswatch_process child: starting watcher from pid %d, tid %d", getpid(), pthread_self());
        /* int exit_status = run_main_loop(data); */

        int exit_status = obazl_fswatch_start(rootdir);
        log_debug("obazl_fswatch_process child exit, pid: %d", getpid());
        /* fflush(stdout); */
        /* fflush(stderr); */
        exit(EXIT_SUCCESS);
    }
    /* log_debug("fork_fswatch_process child pid: %d", pid); */
    return pid;
}

/**
   returns: 0 for success
 */
static pid_t fork_one(int pipefd[2], const char *pid_file, char *rootdir, void *data)
                      /* int (*initialize_daemon) (void *data), */
                      /* int (*run_main_loop) (void *data) */
                      /* ) */
{
    log_debug("fork_one entry, pid %d", getpid());
    pid_t child_pid = fork();

    if(child_pid == 0)
    {
        log_debug("fork_one child process entry, pid %d", getpid());
        close(pipefd[0]); /* Close unneeded read-end */

        if(setsid() == -1)
        {
            notify_parent_process(pipefd[1], STATUS_CANNOT_SET_SID);
            exit(STATUS_CANNOT_SET_SID);
        }

        /* Fork again, so that the terminal can not be acquired again */
        /* log_debug("fork_one child calling fork_fswatch_process"); */
        pid_t gcpid = fork_fswatch_process(pipefd[1], pid_file, rootdir, data);
        if(gcpid == -1) {
            notify_parent_process(pipefd[1], STATUS_CANNOT_FORK_FSWATCH_PROCESS);
            log_error("child child fork failed, exiting");
            exit(STATUS_CANNOT_FORK_FSWATCH_PROCESS);
        /* } else { */
        /*     /\* fork_fswatch_process child will notify_parent_process *\/ */
        /*     /\* Exit fork_one child process, so that the grandchild (daemon) process gets adopted by PID 1 *\/ */
        /*     exit(EXIT_SUCCESS); */
        }
        log_debug("fork_one child process exiting, pid %d", getpid());
        exit(EXIT_SUCCESS);
    }
    log_debug("fork_one parent pid %d returning, child_pid: %d", getpid(), child_pid);
    return 0;
}

/*
  blocks until msg recd from child process
*/
static enum daemon_status_e wait_for_notification_message(int readfd)
{
    char buf[BUFFER_SIZE];
    ssize_t bytes_read = read(readfd, buf, 1);

    if(bytes_read == -1)
        return STATUS_CANNOT_READ_FROM_PIPE;
    else if(bytes_read == 0)
        return STATUS_UNKNOWN_DAEMON_ERROR;
    else
        return buf[0];
}

/* int daemonize(void (*run_main_loop)(void *), void *data) */
int obazl_watch_daemonize(char *rootdir, void *data)
{
    log_debug("obazl_watch_daemonize");
    int rc;
    int pipefd[2];
    char *pid_file = "obazlw.pid";

    /* if(!close_non_standard_file_descriptors()) */
    /*     return STATUS_CANNOT_CLOSE_NON_STD_FDS; */
    /* if( !daemon_info.no_close_stdio && (redirect_stdio_to_devnull() != 0) ) */
    /*     daemon_error_exit("Can't redirect stdio to /dev/null: %m\n"); */
    /* rc = close_file_descriptors(); */
    /* log_debug("close_file_descriptors rc: %d"); */

    config_log();

    /* if(!reset_signal_handlers_to_default()) */
    /*     return STATUS_CANNOT_RESET_SIGNAL_HANDLERS; */
    /* if(!clear_signal_mask()) */
    /*     return STATUS_CANNOT_CLEAR_SIGNAL_MASK; */
    /* reset signal mask, set sig handlers */
    /* config_signals(); */
    rc = clear_signal_mask();
    /* log_debug("clear_signal_mask rc: %d", rc); */
    /* rc = 0; */
    rc = reset_signal_handlers();
    /* log_debug("reset_signal_handlers rc: %d", rc); */

    /* char cwd[PATH_MAX]; */
    /* if (getcwd(cwd, sizeof(cwd)) != NULL) { */
    /*     log_debug("Current working dir: %s", cwd); */
    /* } */

    /* log_debug("changing dir to %s", utstring_body(proj_root)); */
    rc = chdir(utstring_body(proj_root));
    if (rc) {
        perror("chdir");
        log_fatal("Unable to chdir to projroot %", utstring_body(proj_root));
        exit(EXIT_FAILURE);
    }

    if(pipe(pipefd) == -1)
        return STATUS_CANNOT_CREATE_PIPE;
    else {

        /* log_debug("daemonize calling fork_one"); */
        rc = fork_one(pipefd, pid_file, rootdir, data);

        if (rc != 0) {
            log_error("fork_one error, returned %d", rc);
            return STATUS_CANNOT_FORK_ONE;
        } else {
            enum daemon_status_e exit_status;

            close(pipefd[1]); /* Close unneeded write end */
            /* wait for child of child to send notification */
            /* log_debug("daemonize awaiting notification from fork_one child pid %d", cpid); */
            exit_status = wait_for_notification_message(pipefd[0]);
            close(pipefd[0]);
            if (exit_status == STATUS_INIT_SUCCESS) {
                log_debug("daemonize fork parent recd success notification: %d", exit_status);
            } else {
                log_error("daemonize fork parent recd fail notification from child: %d", exit_status);
                exit(EXIT_FAILURE); /* FIXME */
            }
            log_debug("daemonize returning, pid: %d", getpid());
            return exit_status;
        }
    }

    /* Reset the file mode creation mask */
    /* umask(0); */

    /* Create a new process group(session) (SID) for the child process */
    /* if( (setsid() == -1) ) */
    /*     daemon_error_exit("Can't setsid: %m\n"); */

    log_debug("daemonize returning, pid: %d", getpid());
    return EXIT_SUCCESS;

    // call user functions for the optional initialization
    // before closing the standardIO (STDIN, STDOUT, STDERR)
    /* if( optional_init ) */
    /*     optional_init(data); */

    /* daemon_info.daemonized = 1; //good job */
}
