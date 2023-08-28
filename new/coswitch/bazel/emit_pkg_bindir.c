#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>             /* access */

#if EXPORT_INTERFACE
#include "libs7.h"
#endif

#include "utarray.h"
#include "utstring.h"

#include "log.h"

/* #include "libdune.h" */
#include "emit_pkg_bindir.h"

extern s7_scheme *s7;

/* extern UT_string *opam_switch_lib; */

static char *tostr1 = NULL;
LOCAL char *tostr2 = NULL;
#define TO_STR(x) s7_object_to_c_string(s7, x)
#define LOG_S7_DEBUG(msg, obj) ((tostr1 = TO_STR(obj)), (fprintf(stderr, GRN " S7: " CRESET "%s:%d " #msg ": %s\n", __FILE__, __LINE__, tostr1)), (free(tostr1)))

/* **************************************************************** */
/* s7_scheme *s7;                  /\* GLOBAL s7 *\/ */
/* /\* const char *errmsg = NULL; *\/ */

/* static int level = 0; */
extern int spfactor;
extern char *sp;

#if defined(TRACING)
extern int indent;
extern int delta;
#endif

bool stdlib_root = false;

/* char *buildfile_prefix = "@//" HERE_OBAZL_ROOT "/buildfiles"; */
/* was: "@//.opam.d/buildfiles"; */

long *KPM_TABLE;

FILE *opam_resolver;

extern UT_string *repo_name;

UT_array *_get_pkg_stublibs(s7_scheme *s7, char *pkg, void *_stanzas)
/* UT_string *dune_pkg_file) */
{
    /* TRACE_ENTRY; */
    (void)pkg;
    log_debug("_get_pkg_stublibs");

    s7_pointer stanzas = (s7_pointer) _stanzas;
/* #if defined(DEVBUILD) */
/*     log_debug("stanzas: %s", TO_STR(stanzas)); */
/* #endif */

    UT_string *outpath;
    UT_string *opam_bin;
    utstring_new(outpath);
    utstring_new(opam_bin);

    UT_array *stubs;
    utarray_new(stubs, &ut_str_icd);

    /* s7_pointer stanzas = read_dune_package(dune_pkg_file); */

    s7_pointer iter, stublib_file;

    s7_pointer e = s7_inlet(s7,
                            s7_list(s7, 1,
                                    s7_cons(s7,
                                            s7_make_symbol(s7, "stanzas"),
                                            stanzas)));

    char * stublibs_sexp =
        "(let ((files (assoc 'files (cdr stanzas))))"
        "  (if files"
        "      (let ((bin (assoc 'stublibs (cdr files))))"
        "          (if bin (cadr bin)))))";

    s7_pointer stublibs = s7_eval_c_string_with_environment(s7, stublibs_sexp, e);

    if (stublibs == s7_unspecified(s7)) {
        log_debug("NO STUBLIBS");
        return stubs;
    }

#if defined(DEVBUILD)
    if (mibl_debug) {
        /* log_debug("Pkg: %s", utstring_body(dune_pkg_file)); */
        LOG_S7_DEBUG(RED "STUBLIBS" CRESET, stublibs);
    }
#endif

    /* result is list of stublibs installed in $PREFIX/bin */
    /* if (s7_is_list(s7, stublibs)) { */
    /*     if (verbose) { */
    /*         log_info(GRN "%s stublibs:" CRESET " %s", */
    /*                  pkg, */
    /*                  /\* " for %s: %s\n", *\/ */
    /*                  /\* utstring_body(dune_pkg_file), *\/ */
    /*                  TO_STR(stublibs)); */
    /*     } */
    /* } */
    iter = s7_make_iterator(s7, stublibs);
        //gc_loc = s7_gc_protect(s7, iter);
    if (!s7_is_iterator(iter)) {
        log_error("s7_is_iterator fail");
#if defined(DEVBUILD)
        LOG_S7_DEBUG("not an iterator", iter);
#endif
    }
    if (s7_iterator_is_at_end(s7, iter)) {
        log_error("s7_iterator_is_at_end prematurely");
#if defined(DEVBUILD)
        LOG_S7_DEBUG("iterator prematurely done", iter);
#endif
    }
    char *f;
    while (true) {
        stublib_file = s7_iterate(s7, iter);
        if (s7_iterator_is_at_end(s7, iter)) break;
#if defined(DEVBUILD)
        LOG_S7_DEBUG("stublib_file", stublib_file);
#endif
        f = TO_STR(stublib_file);
        utarray_push_back(stubs, &f);
        free(f);
    }
    return stubs;
}

/* ######################################################### */
LOCAL char *_dunefile_to_string(const char *dunefile_name)
{
    /* TRACE_ENTRY; */
/* #if defined(TRACING) */
    log_trace("dunefile_to_string: %s", dunefile_name);
                  //utstring_body(dunefile_name));
/* #endif */
    /* core/dune file size: 45572 */
    // 2K

#define DUNE_BUFSZ 131072
    /* static char inbuf[DUNE_BUFSZ]; */
    /* memset(inbuf, '\0', DUNE_BUFSZ); */
    static char outbuf[DUNE_BUFSZ + 20];
    memset(outbuf, '\0', DUNE_BUFSZ);

    off_t file_size;
    char *inbuf;
    struct stat stbuf;
    int fd;
    FILE *instream = NULL;

    fd = open(dunefile_name, O_RDONLY);
    if (fd == -1) {
        /* Handle error */
        log_error("fd open error");
        goto cleanup;
    }

    if ((fstat(fd, &stbuf) != 0) || (!S_ISREG(stbuf.st_mode))) {
        /* Handle error */
        log_error("fstat error");
        goto cleanup;
    }

    file_size = stbuf.st_size;
#if defined(DEVBUILD)
    if (mibl_debug) log_debug("filesize: %d", file_size);
#endif

    inbuf = (char*)malloc(file_size);
    if (inbuf == NULL) {
        /* Handle error */
        log_error("malloc file_size fail");
        goto cleanup;
    }

    /* FIXME: what about e.g. unicode in string literals? */
    errno = 0;
    /* FILE *instream = fopen(dunefile_name, "r"); */
    instream = fdopen(fd, "r");
    if (instream == NULL) {
        /* Handle error */
        printf(RED "ERROR" CRESET "fdopen failure: %s\n",
               dunefile_name);
               /* utstring_body(dunefile_name)); */
        perror(NULL);
        close(fd);
        goto cleanup;
    } else {
#if defined(DEVBUILD)
        if (mibl_debug) log_debug("fdopened %s",
                                  dunefile_name);
                                  /* utstring_body(dunefile_name)); */
#endif
    }

    // now read the entire file
    size_t read_ct = fread(inbuf, 1, file_size, instream);
#if defined(DEVBUILD)
    if (mibl_debug) log_debug("read_ct: %d", read_ct);
#endif
    if ((off_t)read_ct != file_size) {
        if (ferror(instream) != 0) {
            printf(RED "ERROR" CRESET "fread error 2 for %s\n",
                   dunefile_name);
            /* utstring_body(dunefile_name)); */
            log_error("fread error 2 for %s\n",
                      dunefile_name);
            /* utstring_body(dunefile_name)); */
            exit(EXIT_FAILURE); //FIXME: exit gracefully
        } else {
            if (feof(instream) == 0) {
                printf(RED "ERROR" CRESET "fread error 3 for %s\n",
                       dunefile_name);
                /* utstring_body(dunefile_name)); */
                log_error("fread error 3 for %s\n",
                          dunefile_name);
                /* utstring_body(dunefile_name)); */
                exit(EXIT_FAILURE); //FIXME: exit gracefully
            } else {
                log_error("WTF????????????????");
                goto cleanup;
            }
        }
    } else {
        close(fd);
        fclose(instream);
    }


/*     if (instream == NULL) { */
/*         printf(RED "ERROR" CRESET "fopen failure: %s\n", */
/*                dunefile_name); */
/*                /\* utstring_body(dunefile_name)); *\/ */
/*         perror(NULL); */
/*         exit(EXIT_FAILURE); */
/*     } else { */
/* #if defined(DEVBUILD) */
/*         if (mibl_debug) log_debug("fopened %s", */
/*                                   dunefile_name); */
/*                                   /\* utstring_body(dunefile_name)); *\/ */
/* #endif */
/*     } */
    //FIXME: do not use fseek/ftell to get file size, use fstat
    // https://wiki.sei.cmu.edu/confluence/display/c/FIO19-C.+Do+not+use+fseek%28%29+and+ftell%28%29+to+compute+the+size+of+a+regular+file
    /* fseek(instream, 0, SEEK_END); */
    /* if (fseek(instream, 0, SEEK_END) != 0) { */
    /*     goto cleanup; */
    /* } */
    /* uint64_t fileSize = ftell(instream); */
/* #if defined(DEVBUILD) */
/*     if (mibl_debug) log_debug("filesize: %d", fileSize); */
/* #endif */

    /* if (fileSize > DUNE_BUFSZ) { */
    /*     printf(RED "ERROR:" CRESET */
    /*            " dune file '%s' size (%" PRIu64 " > DUNE_BUFSZ (%d)\n", */
    /*            dunefile_name, // utstring_body(dunefile_name), */
    /*            fileSize, DUNE_BUFSZ); */
    /*     log_error("dune file size (%d) > DUNE_BUFSZ (%d)", fileSize, DUNE_BUFSZ); */
    /*     exit(EXIT_FAILURE);     /\* FIXME: exit gracefully *\/ */
    /* } */
    /* rewind(instream); */

    /* char *outbuf = malloc(fileSize + 1); */
    /* memset(outbuf, '\0', fileSize); */

/*     uint64_t outFileSizeCounter = fileSize; */

/*     /\* we fread() bytes from instream in COPY_BUFFER_MAXSIZE increments, */
/*        until there is nothing left to fread() *\/ */
/*     int read_ct = 0; */
/*     do { */
/*         /\* printf("reading...\n"); *\/ */
/*         if (outFileSizeCounter > DUNE_BUFSZ) { */
/*             /\* probably won't see a 16K dune file *\/ */
/*             read_ct = fread(inbuf, 1, (size_t) DUNE_BUFSZ, instream); */
/*             if (read_ct != DUNE_BUFSZ) { */
/*                 if (ferror(instream) != 0) { */
/*                     printf(RED "ERROR" CRESET " fread error 1 for %s\n", */
/*                            dunefile_name); */
/*                               /\* utstring_body(dunefile_name)); *\/ */
/*                     log_error("fread error 1 for %s\n", */
/*                               dunefile_name); */
/*                               /\* utstring_body(dunefile_name)); *\/ */
/*                     exit(EXIT_FAILURE); //FIXME: exit gracefully */
/*                 } else { */
/*                     // readed < DUNE_BUFSZ? */
/*                 } */
/*             } else { */
/*                 // readed DUNE_BUFSZ bytes? */
/*             } */
/*             /\* log_debug("writing"); *\/ */
/*             outFileSizeCounter -= DUNE_BUFSZ; */
/*         } */
/*         else { */
/*             read_ct = fread(inbuf, 1, (size_t) outFileSizeCounter, instream); */
/* #if defined(DEVBUILD) */
/*             if (mibl_debug) log_debug("read_ct: %d", read_ct); */
/* #endif */
/*             if (read_ct != outFileSizeCounter) { */
/*                 if (ferror(instream) != 0) { */
/*                     printf(RED "ERROR" CRESET "fread error 2 for %s\n", */
/*                            dunefile_name); */
/*                               /\* utstring_body(dunefile_name)); *\/ */
/*                     log_error("fread error 2 for %s\n", */
/*                               dunefile_name); */
/*                               /\* utstring_body(dunefile_name)); *\/ */
/*                     exit(EXIT_FAILURE); //FIXME: exit gracefully */
/*                 } else { */
/*                     if (feof(instream) == 0) { */
/*                         printf(RED "ERROR" CRESET "fread error 3 for %s\n", */
/*                                dunefile_name); */
/*                               /\* utstring_body(dunefile_name)); *\/ */
/*                         log_error("fread error 3 for %s\n", */
/*                                   dunefile_name); */
/*                                   /\* utstring_body(dunefile_name)); *\/ */
/*                         exit(EXIT_FAILURE); //FIXME: exit gracefully */
/*                     } else { */
/*                         /\* printf("bbbbbbbbbbbbbbbb\n"); *\/ */
/*                     } */
/*                 } */
/*             } */
/*             outFileSizeCounter = 0ULL; */
/*         } */
/*     } while (outFileSizeCounter > 0); */
/* #if defined(DEVBUILD) */
/*     if (mibl_debug) { */
/*         log_debug(RED "readed" CRESET " %d bytes", read_ct); */
/*         /\* log_debug(RED "readed string:" CRESET " '%s'", inbuf); *\/ */
/*     } */
/* #endif */
    /* fclose(instream); */

    // FIXME: loop over the entire inbuf
    char *inptr = (char*)inbuf;
    char *outptr = (char*)outbuf;
    char *cursor = inptr;

    while (true) {
        cursor = strstr(inptr, ".)");

/* https://stackoverflow.com/questions/54592366/replacing-one-character-in-a-string-with-multiple-characters-in-c */

        if (cursor == NULL) {
/* #if defined(DEVBUILD) */
/*             if (mibl_debug) log_debug("remainder: '%s'", inptr); */
/* #endif */
            size_t ct = strlcpy(outptr, (const char*)inptr, file_size); // strlen(outptr));
            (void)ct;           /* prevent -Wunused-variable */
/* #if defined(DEVBUILD) */
/*             if (mibl_debug) log_debug("concatenated: '%s'", outptr); */
/* #endif */
            break;
        } else {
#if defined(DEVBUILD)
            if (mibl_debug) log_error("FOUND and fixing \".)\" at pos: %d", cursor - inbuf);
#endif
            size_t ct = strlcpy(outptr, (const char*)inptr, cursor - inptr);
#if defined(DEVBUILD)
            if (mibl_debug) {
                log_debug("copied %d chars", ct);
                /* log_debug("to buf: '%s'", outptr); */
            }
#endif
            if (ct >= DUNE_BUFSZ) {
                printf("output string has been truncated!\n");
            }
            outptr = outptr + (cursor - inptr) - 1;
            outptr[cursor - inptr] = '\0';
            ct = strlcat(outptr, " ./", DUNE_BUFSZ);
            outptr += 3;

            inptr = inptr + (cursor - inptr) + 1;
            /* printf(GRN "inptr:\n" CRESET " %s\n", inptr); */

            if (ct >= DUNE_BUFSZ) {
                printf(RED "ERROR" CRESET "write count exceeded output bufsz\n");
                exit(EXIT_FAILURE);
                // output string has been truncated
            }
        }
    }
    return outbuf;

cleanup:
    //FIXME
    if (instream != NULL)
    {
        fclose(instream);
        close(fd);
    }
    return NULL;
}

s7_pointer _read_dune_package(s7_scheme *s7, UT_string *dunefile_name)
{
    /* TRACE_ENTRY; */
    //FIXME: this duplicates the code in read_dunefile
/* #if defined(TRACING) */
    log_trace("read_dune_package: %s", utstring_body(dunefile_name));
/* #endif */

    char *dunestring = _dunefile_to_string(utstring_body(dunefile_name));
/* #if defined(DEVBUILD) */
/*     if (mibl_debug) log_debug("readed str: %s", dunestring); */
/* #endif */

    /* stanza accumulator */
    s7_pointer stanzas = s7_list(s7, 0);

    s7_pointer sport = s7_open_input_string(s7, dunestring);

    const char *errmsg;
    if (!s7_is_input_port(s7, sport)) {
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        if ((errmsg) && (*errmsg)) {
            printf(RED "ERROR" CRESET "s7_open_input_string failed\n");
            log_error("[%s\n]", errmsg);
            s7_quit(s7);
            exit(EXIT_FAILURE);
        }
    }

#if defined(TRACING)
    /* if (mibl_debug) */
    log_debug("s7 reading stanzas");
#endif

    /* read all stanzas in dunefile */
    while(true) {
/* #if defined(TRACING) */
/*         if (mibl_debug) log_debug("iter"); */
/* #endif */
        s7_pointer stanza = s7_read(s7, sport);
        /* FIXME: error checks */
        /* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */
        /* if ((errmsg) && (*errmsg)) { */
        /*     if (mibl_debug) log_error("[%s\n]", errmsg); */
        /*     s7_close_input_port(s7, sport); */
        /*     s7_quit(s7); */
        /*     exit(EXIT_FAILURE); */
        /*     break; */
        /* } */
        if (stanza == s7_eof_object(s7)) break;
        if (s7_is_null(s7,stanzas)) {
            stanzas = s7_list(s7, 1, stanza);
        } else{
            stanzas = s7_append(s7,stanzas, s7_list(s7, 1, stanza));
        }
    }
    s7_close_input_port(s7, sport);
#if defined(TRACING)
    /* if (mibl_debug) */
        log_debug("finished reading");
#endif

    /* s7_gc_unprotect_at(s7, baddot_gc_loc); */
    /* close_error_config(); */

    /* leave error config as-is */
    /* free(dunestring); */
    return stanzas;
}

UT_array *_get_pkg_executables(s7_scheme *s7, void *_stanzas)
/* UT_string *dune_pkg_file) */
{
    /* TRACE_ENTRY; */
    log_trace("_get_pkg_executables");
    /* LOG_S7_DEBUG("stanzas", _stanzas); */

    s7_pointer stanzas = (s7_pointer) _stanzas;
    UT_string *outpath;
    UT_string *opam_bin;
    utstring_new(outpath);
    utstring_new(opam_bin);

    UT_array *bins;
    utarray_new(bins, &ut_str_icd);

    /* s7_pointer stanzas = _read_dune_package(dune_pkg_file); */

    s7_pointer iter, binfile;

    s7_pointer e = s7_inlet(s7,
                            s7_list(s7, 1,
                                    s7_cons(s7,
                                            s7_make_symbol(s7, "stanzas"),
                                            stanzas)));

    char * exec_sexp =
        "(let ((files (assoc 'files stanzas)))"
        "  (if files"
        "      (let ((bin (assoc 'bin (cdr files))))"
        "          (if bin (cadr bin)))))";

        /* "(format #t \"FILES: ~A~%\" files)))"; */

        /* "(let ((files (assoc 'files (cdr stanzas))))" */


    /* char * exec_sexp = */
    /*     "(format #t \"XXXX ~A~%\" stanzas)"; */

    /* LOG_S7_DEBUG("stanzas", _stanzas); */

    s7_pointer executables = s7_eval_c_string_with_environment(s7, exec_sexp, e);
    /* LOG_S7_DEBUG("execs", executables); */

    if (executables == s7_unspecified(s7)) {
        log_debug("NO BINARIES");
        return bins;
    }

#if defined(DEVBUILD)
    if (mibl_debug) {
        /* log_debug("Pkg: %s", utstring_body(dune_pkg_file)); */
        LOG_S7_DEBUG("executables", executables);
    }
#endif

    /* /\* result is list of executables installed in $PREFIX/bin *\/ */
    /* if (s7_is_list(s7, executables)) { */
    /*     if (verbose) { */
    /*     } */
    /* } */
    iter = s7_make_iterator(s7, executables);
        //gc_loc = s7_gc_protect(s7, iter);
    if (!s7_is_iterator(iter)) {
        log_error("s7_make_iterator failed");
#if defined(DEVBUILD)
        LOG_S7_DEBUG("not an iterator", iter);
#endif
    }
    if (s7_iterator_is_at_end(s7, iter)) {
        log_error("s7_iterator_is_at_end prematurely");
#if defined(DEVBUILD)
        LOG_S7_DEBUG("iterator prematurely done", iter);
#endif
    }
    char *f;
    while (true) {
        binfile = s7_iterate(s7, iter);
        if (s7_iterator_is_at_end(s7, iter)) break;
#if defined(DEVBUILD)
        LOG_S7_DEBUG("binfile", binfile);
#endif
        f = s7_object_to_c_string(s7, binfile);
        utarray_push_back(bins, &f);
        free(f);
    }
        /* utstring_renew(opam_bin); */
        /* utstring_printf(opam_bin, "%s/%s", */
        /*                 utstring_body(opam_switch_bin), */
        /*                 TO_STR(binfile)); */

        /* utstring_renew(outpath); */
        /* utstring_printf(outpath, "%s/%s/bin/%s", */
        /*                 obazl, pkg, TO_STR(binfile)); */
        /* rc = symlink(utstring_body(opam_bin), */
        /*              utstring_body(outpath)); */
        /* if (rc != 0) { */
        /*     if (errno != EEXIST) { */
        /*         perror(NULL); */
        /*         fprintf(stderr, "exiting\n"); */
        /*         exit(EXIT_FAILURE); */
        /*     } */
        /* } */
        /* if (!emitted_bootstrapper) */
        /*     emit_local_repo_decl(bootstrap_FILE, pkg); */

        /* fprintf(ostream, "exports_files([\"%s\"])\n", TO_STR(binfile)); */
        /* fprintf(ostream, "## src: %s\n", utstring_body(opam_bin)); */
        /* fprintf(ostream, "## dst: %s\n", utstring_body(outpath)); */
    /* } */
    return bins;
}

/* FIXME: same in mibl/error_handler.c */
void emit_opam_pkg_bindir(char *switch_pfx,
                          char *coswitch_lib,
                          const char *pkg
                          ) // UT_string *dune_pkg_file)
                          /* char *switch_lib, */
                          /* char *pkg, */
                          /* char *obazl, */
                          /* bool emitted_bootstrapper) */
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_trace("emit_opam_pkg_bindir");
#endif

    /* read dune-package file. if it exports executables:
       1. write bin/BUILD.bazel with a rule for each
       2. symlink from opam switch
     */

    utstring_renew(dune_pkg_file);
    utstring_printf(dune_pkg_file, "%s/%s/dune-package",
                    switch_pfx, /* global */
                    pkg);

    s7_pointer stanzas = _read_dune_package(s7, dune_pkg_file);

    /* LOG_S7_DEBUG("stanzas", stanzas); */

    /* s7_pointer executables = _get_pkg_executables(dune_pkg_file); */
    // in dune_readers
    UT_array *executables = _get_pkg_executables(s7, stanzas); // dune_pkg_file);

    if (utarray_len(executables) == 0) goto stublibs;

    log_debug("AAAAAAAAAAAAAAAA");
    /* if executables not null:
       1. create 'bin' subdir of pkg
       2. add WORKSPACE.bazel to <pkg>/bin
       3. symlink executables to <pkg>/bin
       4. add BUILD.bazel with exports_files for linked executables
     */

    UT_string *outpath;
    utstring_new(outpath);
    UT_string *opam_bin;
    utstring_new(opam_bin);
    /* s7_pointer iter, binfile; */

    /* for most pkgs, WORKSPACE.bazel is already written,
       but for some containing only executables (e.g. menhir),
       we need to write it now
    */
    utstring_renew(outpath);
    utstring_printf(outpath, "%s/%s/WORKSPACE.bazel",
                    coswitch_lib,
                    pkg);
#if defined(TRACING)
    if (mibl_debug)
        log_debug("checking ws: %s", utstring_body(outpath));
#endif
    if (access(utstring_body(outpath), F_OK) != 0) {
#if defined(TRACING)
        log_debug("creating %s\n", utstring_body(outpath));
#endif
        /* if obazl/pkg not exist, create it with WORKSPACE */
        /* utstring_renew(outpath); */
        /* utstring_printf(outpath, "%s/%s", */
        /*                 coswitch_lib, */
        /*                 pkg); */
        /* errno = 0; */
        /* if (access(utstring_body(outpath), F_OK) != 0) { */
        /*     utstring_printf(outpath, "/%s", "WORKSPACE.bazel"); */
        emit_workspace_file(outpath, pkg);
    }
    errno = 0;
    utstring_renew(outpath);
    utstring_printf(outpath, "%s/%s/bin",
                    coswitch_lib,
                    //rootws, /* obazl, */
                    pkg);
    mkdir_r(utstring_body(outpath));

    /* create <pkg>/bin/BUILD.bazel */
    utstring_renew(outpath);
    utstring_printf(outpath, "%s/%s/bin/BUILD.bazel",
                    coswitch_lib,
                    pkg);
    /* rc = access(utstring_body(build_bazel_file), F_OK); */
#if defined(TRACING)
    log_debug("fopening: %s", utstring_body(outpath));
#endif

    errno = 0;
    FILE *ostream;
    ostream = fopen(utstring_body(outpath), "w");
    if (ostream == NULL) {
        printf(RED "ERROR" CRESET "fopen failure for %s", utstring_body(outpath));
        log_error("fopen failure for %s", utstring_body(outpath));
        perror(utstring_body(outpath));
        log_error("Value of errno: %d", errno);
        log_error("fopen error %s", strerror( errno ));
        exit(EXIT_FAILURE);
    }
    fprintf(ostream, "## generated file - DO NOT EDIT\n");

    fprintf(ostream, "exports_files([");

    /* For each executable, create symlink and exports_files entry */
    char **p = NULL;
    while ( (p=(char**)utarray_next(executables,p))) {
#if defined(TRACING)
        log_debug("bin: %s",*p);
#endif
        fprintf(ostream, "\"%s\",", *p);
        /* symlink */
        utstring_renew(opam_bin);
        utstring_printf(opam_bin, "%s/bin/%s",
                        switch_pfx,
                        /* utstring_body(switch_bin), */
                        *p);
#if defined(TRACING)
        log_debug("SYMLINK SRC: %s", utstring_body(opam_bin));
#endif
        utstring_renew(outpath);
        utstring_printf(outpath, "%s/%s/bin/%s",
                        coswitch_lib,
                        pkg,
                        *p);
#if defined(TRACING)
        log_debug("SYMLINK DST: %s", utstring_body(outpath));
#endif
        int rc = symlink(utstring_body(opam_bin),
                     utstring_body(outpath));
        symlink_ct++;
        if (rc != 0) {
            if (errno != EEXIST) {
                perror(NULL);
                fprintf(stderr, "exiting\n");
                exit(EXIT_FAILURE);
            }
        }
    }
    fprintf(ostream, "])\n");
    fclose(ostream);

    if (verbose && verbosity > 1) {
        utstring_renew(outpath);
        utstring_printf(outpath, "%s/%s/bin",
                        coswitch_lib,
                        pkg);

        log_info("Created %s containing symlinked pkg executables",
                 utstring_body(outpath));
    }


 stublibs:
    ;
    log_debug("STUBLIBS");

    // FIXME: get stublibs dir from opam_switch_stublibs()

    /* opam dumps all stublibs ('dllfoo.so') in lib/stublibs; they are
       not found in the pkg's lib subdir. But the package's
       dune-package file lists them, so we read that and then symlink
       them from lib/stublibs to lib/<pkg>/stublibs.
     */

    UT_array *stublibs = _get_pkg_stublibs(s7, (char*)pkg, stanzas);
    if (utarray_len(stublibs) == 0) goto exit;

    UT_string *opam_stublib;
    utstring_new(opam_stublib);

    /* s7_pointer iter, binfile; */

    /* for most pkgs, WORKSPACE.bazel is already written,
       but for some containing only stublibs (e.g. menhir),
       we need to write it now
    */
    utstring_new(outpath);
    utstring_printf(outpath, "%s/%s/WORKSPACE.bazel",
                    coswitch_lib,
                    pkg);

#if defined(TRACING)
    /* if (mibl_debug) */
        log_debug("checking ws: %s", utstring_body(outpath));
#endif
    if (access(utstring_body(outpath), F_OK) != 0) {
        log_debug("creating %s\n", utstring_body(outpath));
#if defined(TRACING)
#endif
        /* if obazl/pkg not exist, create it with WORKSPACE */
        /* utstring_renew(outpath); */
        /* utstring_printf(outpath, "%s/%s", */
        /*                 coswitch_lib, */
        /*                 pkg); */
        /* errno = 0; */
        /* if (access(utstring_body(outpath), F_OK) != 0) { */
        /*     utstring_printf(outpath, "/%s", "WORKSPACE.bazel"); */
        emit_workspace_file(outpath, pkg);
    }
    errno = 0;
    utstring_renew(outpath);
    utstring_printf(outpath, "%s/%s/stublibs",
                    coswitch_lib,
                    pkg);
    mkdir_r(utstring_body(outpath));
    log_debug("stublibs: %s", utstring_body(outpath));
    utstring_renew(outpath);
    utstring_printf(outpath, "%s/%s/stublibs/BUILD.bazel",
                    coswitch_lib,
                    pkg);
    /* rc = access(utstring_body(build_bazel_file), F_OK); */
#if defined(TRACING)
    log_debug("fopening: %s", utstring_body(outpath));
#endif

    /* FILE *ostream; */
    errno = 0;
    ostream = fopen(utstring_body(outpath), "w");
    if (ostream == NULL) {
        printf(RED "ERROR" CRESET "fopen failure for %s", utstring_body(outpath));
        log_error("fopen failure for %s", utstring_body(outpath));
        perror(utstring_body(outpath));
        log_error("Value of errno: %d", errno);
        log_error("fopen error %s", strerror( errno ));
        exit(EXIT_FAILURE);
    }
    fprintf(ostream, "## generated file - DO NOT EDIT\n");

    fprintf(ostream, "exports_files([");

    /* For each stublib, create symlink and exports_files entry */
    p = NULL;
    while ( (p=(char**)utarray_next(stublibs,p))) {
#if defined(TRACING)
        log_debug("stublib: %s",*p);
#endif
        fprintf(ostream, "\"%s\",", *p);
        /* symlink */
        utstring_renew(opam_stublib);
        utstring_printf(opam_stublib, "%s/lib/stublibs/%s",
                        switch_pfx,
                        *p);
#if defined(TRACING)
        log_debug("SYMLINK SRC: %s", utstring_body(opam_stublib));
#endif
        utstring_renew(outpath);
        utstring_printf(outpath, "%s/%s/stublibs/%s",
                        coswitch_lib,
                        pkg,
                        *p);
#if defined(TRACING)
        log_debug("SYMLINK DST: %s", utstring_body(outpath));
#endif
        int rc = symlink(utstring_body(opam_stublib),
                     utstring_body(outpath));
        symlink_ct++;
        if (rc != 0) {
            if (errno != EEXIST) {
                perror(NULL);
                fprintf(stderr, "ERROR,exiting\n");
                exit(EXIT_FAILURE);
            }
        }
    }
    fprintf(ostream, "])\n");
    fclose(ostream);

    if (verbose && verbosity > 1) {
        utstring_renew(outpath);
        utstring_printf(outpath, "%s/%s/stublibs",
                        coswitch_lib,
                        pkg);

        log_info("Created %s containing symlinked stublibs",
                 utstring_body(outpath));
    }

 exit: ;
    /* utstring_free(outpath); */
#if defined(TRACING)
    if (mibl_trace) printf("exiting emit_opam_pkg_bindir\n");
#endif
}

UT_string *dune_pkg_file;

/* pkg always relative to (global) opam_switch_lib */
EXPORT void emit_pkg_bindir(char *opam_switch_lib,
                            char *coswitch_lib,
                            const char *pkg)
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_trace("emit_pkg_bindir: %s", pkg);
#endif

    utstring_renew(dune_pkg_file);
    utstring_printf(dune_pkg_file, "%s/%s/dune-package",
                    opam_switch_lib, /* global */
                    pkg);

#if defined(TRACING)
    if (mibl_trace)
        log_debug("CHECKING DUNE-PACKAGE: %s\n", utstring_body(dune_pkg_file));
#endif
    if (access(utstring_body(dune_pkg_file), F_OK) == 0) {
        emit_opam_pkg_bindir(opam_switch_lib,
                             coswitch_lib,
                             pkg); // dune_pkg_file);
                             /* switch_lib, */
                             /* pkgdir, */
                             /* obazl_opam_root, */
                             /* emitted_bootstrapper); */
    }
}
