#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <libgen.h>
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#if EXPORT_INTERFACE
#include <stdbool.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "log.h"

#include "_obazl_fs.h"

#if INTERFACE
#include "uthash.h"
#include "utarray.h"
#include "utstring.h"
#endif


LOCAL int verbosity = 0;
LOCAL int errnum;
LOCAL int rc;

#if INTERFACE
typedef void (*file_handler)(char *rootdir, char *pkg, char *metafile);
#endif

char work_buf[PATH_MAX];

/* UT_array *opam_packages; */

/* char basedir[PATH_MAX]; */
/* char coqlib[PATH_MAX]; */

char symlink_src[PATH_MAX];
char symlink_tgt[PATH_MAX];

#if EXPORT_INTERFACE
struct fileset_s {
    char name[512];             /* mname = pkg:target */
    int type;                   /* bitfield */
    int filect;
    /* char *files;[16]; */
    UT_array *files;
    UT_hash_handle hh;         /* makes this structure hashable */
} ;

struct package_s {
    char name[512];
    struct fileset_s *filesets;
    /* struct fileset_s *coq_filesets; */
    /* struct fileset_s *ocaml_filesets; */
    int rule_types;              /* used by emit to decide what rules to load */
    UT_hash_handle hh;         /* makes this structure hashable */
} ;
#endif

struct buildfile_s {
    char *name;                 /* set from strndup optarg; must be freed */
    char *path;                 /* set from getenv; do not free */
    UT_hash_handle hh;
} ;
/* struct buildfile_s *buildfiles = NULL; */

/* struct fileset_s *filesets = NULL; */

struct package_s *packages = NULL;

#if EXPORT_INTERFACE
enum extension {
    NOEXT, BAZEL,
    ML, MLI, MLG, MLL, MLY, MLLIB, MLPACK,
    CMI, CMX, CMO, CMA, CMXA, CMXS, CMT, CMTI,
    VO, VOS, VOK, V,
    GLOB,
    SO, A, O, H
};

/* module_type_e won't work if we put it in a .c file, maybe because we assign values? */
enum module_type_e {
    UNKNOWN_TYPE,
    COQ_MODULE = 1,
    OCAML_MODULE = 2,
    OCAML_SIG = 4,
    OCAML_ARCHIVE = 8,
    OCAML_CMT = 16,
    OCAML_SRC = 32,
    COQ_SRC   = 64,
    OCAML_SHARED = 128,
    C_HEADER = 256
};
/*
  file types:  opam, foo.opam, config, META, foo.py, CoqMakefile.in, .css, .sty, .js
*/


#define BYTE_TO_BINARY_PATTERN "%c%c%c%c%c%c%c%c %c%c%c%c%c%c%c%c"

#define BYTE_TO_BINARY(byte)  \
  (byte & 0x8000 ? '1' : '0'), \
  (byte & 0x4000 ? '1' : '0'), \
  (byte & 0x2000 ? '1' : '0'), \
  (byte & 0x1000 ? '1' : '0'), \
  (byte & 0x0800 ? '1' : '0'), \
  (byte & 0x0400 ? '1' : '0'), \
  (byte & 0x0200 ? '1' : '0'), \
  (byte & 0x0100 ? '1' : '0'), \
  (byte & 0x0080 ? '1' : '0'), \
  (byte & 0x0040 ? '1' : '0'), \
  (byte & 0x0020 ? '1' : '0'), \
  (byte & 0x0010 ? '1' : '0'), \
  (byte & 0x0008 ? '1' : '0'), \
  (byte & 0x0004 ? '1' : '0'), \
  (byte & 0x0002 ? '1' : '0'), \
  (byte & 0x0001 ? '1' : '0')

#define CHECK_BIT(var,pos) ((var) & (pos))
#endif

int extlen[26] = {
    [MLPACK]= 7,
    [MLLIB] = 6,
    [BAZEL] = 6,
    [CMXA]  = 5,
    [CMTI]  = 5,
    [CMXS]  = 5,
    [GLOB]  = 5,
    [CMI]   = 4,
    [CMX]   = 4,
    [CMO]   = 4,
    [CMA]   = 4,
    [CMT]   = 4,
    [MLG]   = 4,
    [MLI]   = 4,
    [MLL]   = 4,
    [MLY]   = 4,
    [VOS]   = 4,
    [VOK]   = 4,
    [SO]    = 3,
    [VO]    = 3,
    [ML]    = 3,
    [A]     = 2,
    [O]     = 2,
    [V]     = 2,
    [H]     = 2,
    [NOEXT] = 0,
};

/* https://stackoverflow.com/questions/2736753/how-to-remove-extension-from-file-name */
enum extension fext(const char *filename) {
    //FIXME: start at the end, iterate backwards with ptr--?
    const char *dot = strrchr(filename, '.');
    if(!dot || dot == filename) return NOEXT;
    /* printf("converting ext %s\n", dot); */
    if (0 == strncmp(".cmxa", dot, 5)) {
        return CMXA;
    }
    if (0 == strncmp(".cmxs", dot, 5)) {
        return CMXS;
    }
    if (0 == strncmp(".cmti", dot, 5)) {
        return CMTI;
    }
    if (0 == strncmp(".cmx", dot, 4)) {
        return CMX;
    }
    if (0 == strncmp(".cmo", dot, 4)) {
        return CMO;
    }
    if (0 == strncmp(".cma", dot, 4)) {
        return CMA;
    }
    if (0 == strncmp(".cmt", dot, 4)) {
        return CMT;
    }
    if (0 == strncmp(".cmi", dot, 4)) {
        return CMI;
    }
    if (0 == strncmp(".mli", dot, 4)) {
        return MLI;
    }
    if (0 == strncmp(".vok", dot, 4)) {
        return VOK;
    }
    if (0 == strncmp(".vos", dot, 4)) {
        return VOS;
    }
    if (0 == strncmp(".vo", dot, 3)) {
        return VO;
    }
    if (0 == strncmp(".mli", dot, 4)) {
        return MLI;
    }
    if (0 == strncmp(".ml", dot, 4)) {
        return ML;
    }
    if (0 == strncmp(".so", dot, 4)) {
        return SO;
    }
    if (0 == strncmp(".a", dot, 2)) {
        return A;
    }
    if (0 == strncmp(".o", dot, 2)) {
        return O;
    }
    if (0 == strncmp(".h", dot, 2)) {
        return H;
    }
    if (0 == strncmp(".v", dot, 2)) {
        return V;
    }
    if (0 == strncmp(".glob", dot, 5)) {
        return GLOB;
    }
#ifdef DEBUG
    /* printf("UNRECOGNIZED ext: %s\n", filename); */
#endif
    return NOEXT;
}

/* int strsort(const void *_a, const void *_b) */
/* { */
/*     const char *a = *(const char* const *)_a; */
/*     const char *b = *(const char* const *)_b; */
/*     /\* printf("strsort: %s =? %s\n", a, b); *\/ */
/*     return strcmp(a,b); */
/* } */

/* char* mystrcat( char* dest, char* src ) */
/* { */
/*      while (*dest) dest++; */
/*      while ( (*dest++ = *src++) ); */
/*      return --dest; */
/* } */

/* **************************************************************** */
/*
  equivalent to 'mkdir -p'. caller must free result.
  assumption: base already exists
 */
EXPORT char *mkdir_r(char *base, char *path)
{
    /* log_debug("entering mkdir_r base: '%s', path: '%s'\n", base, path); */

    char *buf_dirname[PATH_MAX];
    char *buf_basename[PATH_MAX];

    if ( access(base, R_OK) ) {
        /* base does not exist */
        /* log_debug("mkdir_r recurring to create base: %s, path: %s", */
        /*           dirname_r(base, buf_dirname), basename_r(base, buf_basename)); */
        mkdir_r(dirname_r(base, (char*)buf_dirname), basename_r(base, (char*)buf_basename));
    }
    /* now base should exist */
    if ( access(base, R_OK) ) {
        log_fatal("no base: %s", base);
        exit(EXIT_FAILURE);
    /* } else { */
        /* log_info("base exists: %s, %s", base, path); */
    }

    if ( strlen(path) == 0 ) return base;

    if ( !strncmp(path, ".", 1) ) return base;

    char work[PATH_MAX];
    work[0] = '\0';
    char last_seg[PATH_MAX];
    last_seg[0] = '\0';

    char *bn = basename(path);
    mystrcat(last_seg, bn);
    /* printf("mkdir_r last_seg: %s\n", last_seg); */
    if ( ! strncmp(last_seg, path, PATH_MAX) ) { // 0 (false) means equal, so !0 means true
        /* printf("mkdir_r bottomed out at %s\n", path); */
        sprintf(work, "%s/%s", base, last_seg);
        /* log_debug("making dir1: %s\n", work); */
        rc = mkdir(work, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
        if (rc != 0) {
            errnum = errno;
            if (errnum == EEXIST) {
                /* already exists */
                ;
            } else {
                fprintf(stderr, "mkdir failure\n");
                perror(work);
                fprintf(stderr, "Value of errno: %d\n", errnum);
                fprintf(stderr, "mkdir error %s\n", strerror( errnum ));
                free(work);
                exit(EXIT_FAILURE);
            }
        }
        /* log_debug("mkdired 1: %s\n", work); */
        char *real = realpath(work, NULL);
        /* log_debug("\trealpath: %s\n", real); */
        free(real);
        return strndup(work, PATH_MAX);
    } else {
        /* chop off last seg and recur */
        char *d = dirname(path);

        // RECUR
        /* log_debug("mkdir_r recurring on %s with pending last_seg %s\n", d, last_seg); */
        char *so_far = mkdir_r(base, d);
        /* log_debug("mkdir_r resuming after %s with pending last_seg %s\n", d, last_seg); */

        /* log_debug("mkdir_r so far: %s\n", so_far); */
        sprintf(work, "%s/%s", so_far, last_seg);
        if ( access(work, R_OK) ) {
            /* work does not exist */
            /* log_info("mkdir_r mking dir: %s\n", work); */
            rc = mkdir(work, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
            if (rc != 0) {
                errnum = errno;
                if (errnum == EEXIST) {
                    /* already exists (should not happen) */
                    ;
                } else {
                    fprintf(stderr, "mkdir failure");
                    perror(work);
                    free(so_far);
                    free(work);
                    exit(EXIT_FAILURE);
                }
            }
        }
        /* log_debug("mkdired 2: %s\n", work); */
        free(so_far);
        return strndup(work, PATH_MAX);
    }
}

/* **************************************************************** */
/*
  adds file to global list
 */
void register_file(struct package_s *the_pkg, char *dir, char *pkg_base, char *pkg, char *file)
{
    /* printf("REGISTER_FILE %s\n", file); */
    /* printf("curdir %s\n", dir); */
    /* printf("package_base %s, package: %s\n", pkg_base, pkg); */
    /* printf("the_pkg %p\n", the_pkg); */

    enum extension ext = fext(file);
    /* printf("FEXT %d\n", ext); */
    int the_type = 0;
    switch(ext) {
    case CMX:
    case CMO:
    case O:
        the_type = OCAML_MODULE;
        break;
    case ML:
        the_type = OCAML_SRC;
        break;
    case CMXA:
    case CMA:
    case A:
        the_type = OCAML_ARCHIVE;
        break;
    case CMTI:
    case CMT:
        the_type = OCAML_CMT | OCAML_MODULE;
        break;
    case CMI:
    case MLI:
        the_type = OCAML_SIG | OCAML_MODULE;
        break;
    case CMXS:
        the_type = OCAML_SHARED;
        break;
    case H:
        the_type = C_HEADER;
        break;
    case V:
    case VO:
    case VOS:
    case VOK:
    case GLOB:
        the_type = COQ_MODULE;
        break;
    default:
        the_type = UNKNOWN_TYPE;
    }

    /* printf("oldfile: %s\n", file); */
    int flen = strnlen(file, PATH_MAX);
    char *newfile = (char *)calloc(1, flen + 1);
    if (newfile == NULL) {
        errnum = errno;
        printf("calloc failure for char *newfile *\n");
        fprintf(stderr, "Value of errno: %d\n", errnum);
        fprintf(stderr, "malloc error %s\n", strerror( errnum ));
        exit(1);
    }

    /* copy filepath, so we can add it to filesets->files */
    strncpy(newfile, file, flen + 1);
    /* printf("newfile: %s\n", newfile); */

    static char mname[256];
    memset(mname, 0, sizeof mname);
    /* int mlen = flen - extlen[ext]; */
    if (the_type == C_HEADER)
        strncpy(mname, "_C_HEADER", 9);
    else
        strncpy(mname, file, flen - extlen[ext]);

    mname[0] = toupper(mname[0]);
    /* printf("fileset name: %s\n", mname); */

    char tgt_label[512];
    tgt_label[0] = '\0';
    mystrcat(tgt_label, pkg_base);
    mystrcat(tgt_label, ":");
    mystrcat(tgt_label, mname);
    /* printf("tgt_label: %s\n", tgt_label); */
    /* struct fileset_s *test_s, *test_tgt_label; */
    /* HASH_ITER(hh, filesets, test_s, test_tgt_label) { */
    /*     printf("%s\n", test_s->name); */
    /* } */
    /* HASH_FIND_STR(filesets, tgt_label, the_fileset);  /\* id already in the hash? *\/ */
    /* the_fileset = NULL; */

    struct fileset_s *the_fileset = NULL;
    if ( the_pkg->filesets != 0 ) {
        /* printf("the_pkg: %p\n", the_pkg); */
        /* printf("pkg filesets ptr: %p\n", the_pkg->filesets); */
        /* struct fileset_s *ms = the_pkg->filesets; */
        /* struct fileset_s *tmp_s, *tmp_tmp; */
        /* HASH_ITER(hh, the_pkg->filesets, tmp_s, tmp_tmp) { */
        /*     printf("\tfileset nm: %s, ct: %d\n", tmp_s->name, tmp_s->filect); */
        /* } */
        HASH_FIND_STR(the_pkg->filesets, mname, the_fileset);
        /* HASH_FIND_STR(the_pkg->filesets, tgt_label, the_fileset); */
    }
    /* printf("found? %p\n", the_fileset); */
    if (the_fileset == NULL) {
        the_fileset = (struct fileset_s *)calloc(1, sizeof *the_fileset);
        if (the_fileset == NULL) {
            errnum = errno;
            printf("malloc failure for struct fileset_s *\n");
            fprintf(stderr, "Value of errno: %d\n", errnum);
            fprintf(stderr, "malloc error %s\n", strerror( errnum ));
            exit(1);
        }
        utarray_new(the_fileset->files, &ut_str_icd);

        /* the_fileset->filect = 0; */
        /* memset(the_fileset, 0, sizeof *the_fileset); */
        /* printf("the_fileset->filect: %d\n", the_fileset->filect); */
        /* strncpy(the_fileset->name, tgt_label, strlen(tgt_label)); */
        /* printf("adding new fileset: %s\n", mname); */
        strncpy(the_fileset->name, mname, strnlen(mname, PATH_MAX));
        the_fileset->type = the_type;
        /* the_fileset->tgt = strrchr(the_fileset->name, ':'); */
        /* the_fileset->tgt++;            /\* skip ':' *\/ */
        HASH_ADD_STR(the_pkg->filesets, name, the_fileset);

        /* if ( the_pkg->filesets == 0 ) { */
        /*     /\* printf("inserting: %s\n", s->name); *\/ */
        /*     HASH_ADD_STR(the_pkg->filesets, name, the_fileset); */
        /*     /\* printf("inserted: %s\n", s->name); *\/ */
        /*     /\* printf("inserted2: %s\n", the_pkg->filesets->name); *\/ */
        /* } else { */
        /*     /\* printf("adding: %s\n", s->name); *\/ */
        /*     HASH_ADD_STR((the_pkg->filesets), name, the_fileset); */
        /* } */
    } else {
        the_fileset->type |= the_type;
    }
    /* printf("adding file %s\n", newfile); */
    utarray_push_back(the_fileset->files, &newfile);
    /* the_fileset->files[the_fileset->filect] = newfile; */
    the_fileset->filect++;
    the_pkg->rule_types |= the_fileset->type;

    /* printf("Fileset:\n"); */
    /* printf("\tpkg:%s\n", the_pkg->name); */
    /* printf("\tname:%s\n", the_fileset->name); */
    /* printf("\ttype: "); */
    /* printf(BYTE_TO_BINARY_PATTERN, BYTE_TO_BINARY(the_fileset->type)); */
    /* printf("\n"); */
    /* printf("\tfilect: %d\n", the_fileset->filect); */
    /* for (int i=0; i < the_fileset->filect; i++) */
    /*     printf("\t\t%s\n", the_fileset->files[i]); */
}

EXPORT int mirror_tree(char *srcroot,
                       bool linkfiles,
                       char *file_to_handle,
                       file_handler handle_meta)
{
    return link_dir_rec(srcroot, "", "",
                        /* tgtroot, */
                        linkfiles, file_to_handle, handle_meta);
}

int link_dir_rec(char *basedir,
                 char *bazel_pkg,
                 char *directory,
                 /* char *out_directory, */
                 bool linkfiles,
                 char *file_to_handle,
                 file_handler handle_meta)
{
    /* log_trace("link_dir_rec %s\n", basedir); //, out_directory); */
    /* log_trace("link_dir_rec directory: %s\n", directory); */
    /* log_trace("link_dir_rec bazel_pkg: %s\n", bazel_pkg); */

    char currdir[PATH_MAX];
    currdir[0] = '\0';
    mystrcat(currdir, basedir);
    if (strnlen(directory, PATH_MAX) > 0) {
        mystrcat(currdir, "/");
        mystrcat(currdir, directory);
    }

    char currpkg[PATH_MAX];
    currpkg[0] = '\0';
    if (strnlen(bazel_pkg, PATH_MAX) > 0) {
        mystrcat(currpkg, bazel_pkg);
        if (strnlen(directory, PATH_MAX) > 0) {
            mystrcat(currpkg, "/");
            mystrcat(currpkg, directory);
        }
    } else {
        mystrcat(currpkg, directory);
    }
    /* log_debug("currpkg: %s", currpkg); */

    /* char outdir[PATH_MAX]; */
    /* outdir[0] = '\0'; */
    /* mystrcat(outdir, out_directory); */
    /* mkdir_r(outdir, currpkg); // directory); */


    /* rc = mkdir(outdir, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH); */
    /* if (rc != 0) { */
    /*     errnum = errno; */
    /*     if (errnum != EEXIST) { */
    /*         perror(outdir); */
    /*         log_error("mkdir failure for '%s'", outdir); */
    /*         exit(1); */
    /*     } */
    /* } */

    DIR *d;
    struct dirent *dir_entry;
    errno = 0;
    /* printf("opening dir %s\n", currdir); */
    /* d = opendir("/usr/local/lib/coq/plugins"); // coqlib); */
    d = opendir(currdir);
    if (d == NULL) {
        errnum = errno;
        printf("opendir failure for %s", currdir);
        fprintf(stderr, "Value of errno: %d\n", errnum);
        fprintf(stderr, "opendir error %s: %s\n", currdir, strerror( errnum ));
        exit(1);
        /* return(-1); */
    }
    /* printf("opened dir %s\n", currdir); */

    struct package_s *the_pkg = NULL;
    if (strlen(currpkg) > 0) {
        HASH_FIND_STR(packages, currpkg, the_pkg);  /* pkg already in the hash? */
        /* printf("found pkg? %d\n", p); */
    }
    if (the_pkg == NULL) {
        the_pkg = (struct package_s *)calloc(sizeof *the_pkg, 1);
        if (the_pkg == NULL) {
            errnum = errno;
            printf("malloc failure for struct package_s *\n");
            fprintf(stderr, "Value of errno: %d\n", errnum);
            fprintf(stderr, "malloc error %s\n", strerror( errnum ));
            exit(1);
        }
        /* memset(the_pkg, 0, sizeof *the_pkg); */
        strncpy(the_pkg->name, currpkg, strlen(currpkg));

        /* s = (struct fileset_s *)malloc(sizeof *s); */
        /* if (s == NULL) { */
        /*     errnum = errno; */
        /*     printf("malloc failure for struct fileset_s *\n"); */
        /*     fprintf(stderr, "Value of errno: %d\n", errnum); */
        /*     fprintf(stderr, "malloc error %s\n", strerror( errnum )); */
        /*     exit(1); */
        /* } */
        /* /\* s->filect = 0; *\/ */
        /* memset(s, 0, sizeof *s); */
        /* the_pkg->filesets = s; */
        HASH_ADD_STR(packages, name, the_pkg);
    }

    // FIXME: correctly handle csdk dirs: lib/ocaml/caml, lib/ocaml/stublibs, lib/ctypes, etc.

    if (d) {
        while ((dir_entry = readdir(d)) != NULL) {
            if ( dir_entry->d_type == DT_DIR
                 && ( (dir_entry->d_name[0] != '.')
                      || // deal with gramlib/.pack
                      ((dir_entry->d_name[1] != '.')
                      &&
                      (strlen(dir_entry->d_name) > 1)
                       ))
                 ) {
                /* printf("handling subdir, currdir: %s\n", currdir); */
                /* printf("handling subdir, currpkg: %s\n", currpkg); */
                /* printf("handling subdir, dir: %s\n", dir_entry->d_name); */
                /* log_debug("handling subdir, outdir: %s\n", outdir); */
                link_dir_rec(currdir, currpkg, dir_entry->d_name,
                             /* outdir, */
                             linkfiles,
                             file_to_handle, handle_meta);
                /* log_debug("handled subdir %s\n", dir_entry->d_name); */
                /* printf("handled subdir, directory: %s\n", directory); */
            } else {
                if (dir_entry->d_type == DT_REG) {
                    /* log_info("entry: %s/%s", currdir, dir_entry->d_name); */
                    /* if META file then emit BUILD.bazel file */
                    /* (or invoke callback) */
                    if ( handle_meta ) {
                        if (strncmp(dir_entry->d_name, file_to_handle, PATH_MAX) == 0) {
                            handle_meta(basedir, currpkg, dir_entry->d_name);
                        }
                        // FIXME: check for foo.META
                    }

                    /* log_debug("THE_PKG: %p\n", the_pkg); */

                    /* register_file(the_pkg, currdir, currpkg, directory, dir_entry->d_name); */

                    /* printf("after register_file, bazel_pkg filesets ptr: %p\n", the_pkg->filesets); */
                    /* printf("\t nm: %s\n", the_pkg->filesets->name); */

                    // why are we searching here?
                    /* struct fileset_s *tmps; */
                    /* HASH_FIND_STR(the_pkg->filesets, the_pkg->filesets->name, tmps); */
                    /* printf("\t found the_pkg->filesets->name: %d\n", tmps); */

                    /* log_debug("currpkg: %s", currpkg); */
                    /* log_debug("currdir: %s", currdir); */

                    /* if (linkfiles) { */
                    /*     symlink_src[0] = '\0'; */
                    /*     mystrcat(symlink_src, currdir); */
                    /*     mystrcat(symlink_src, "/"); */
                    /*     mystrcat(symlink_src, dir_entry->d_name); */
                    /*     /\* log_debug("symlink_src: %s", symlink_src); *\/ */

                    /*     symlink_tgt[0] = '\0'; */
                    /*     mystrcat(symlink_tgt, outdir); */
                    /*     mystrcat(symlink_tgt, "/"); */
                    /*     mystrcat(symlink_tgt, currpkg); */
                    /*     mystrcat(symlink_tgt, "/"); */
                    /*     mystrcat(symlink_tgt, dir_entry->d_name); */
                    /*     /\* log_debug("symlink_tgt: %s", symlink_tgt); *\/ */

                    /*     /\* log_debug("making symlink: %s -> %s, symlink_src, symlink_tgt"); *\/ */

                    /*     rc = symlink(symlink_src, symlink_tgt); */
                    /*     if (rc != 0) { */
                    /*         errnum = errno; */
                    /*         if (errnum != EEXIST) { */
                    /*             perror(symlink_tgt); */
                    /*             log_error("symlink failure for %s -> %s\n", symlink_src, symlink_tgt); */
                    /*             /\* fprintf(stderr, "Value of errno: %d\n", errnum); *\/ */
                    /*             /\* fprintf(stderr, "symlink error %s\n", strerror( errnum )); *\/ */
                    /*             exit(EXIT_FAILURE); */
                    /*             /\* return(-1); *\/ */
                    /*         } */
                    /*     } else { */
                    /*         /\* printf("Created symlink %s -> %s\n", symlink_tgt, symlink_src); *\/ */
                    /*     } */
                    /* } */
                }
            }
        }
        closedir(d);
    }
    return(0);
}

/* int link_dir_rec(char *basedir, */
/*                  char *bazel_pkg, */
/*                  char *directory, */
/*                  /\* char *out_directory, *\/ */
/*                  bool linkfiles, */
/*                  char *file_to_handle, */
/*                  file_handler handle_meta) */
/* } */

int dirseq(char *curr_dir, UT_array *dirs)
{
    /* log_debug("dirseq %s, %p", curr_dir, dirs); */
    /* UT_string *dir; */
    /* utstring_new(dir); */
    /* utstring_printf(dir, "%s", curr_dir); */
    /* log_debug("dirseq len (preimpl): %d", utarray_len(dirs)); */

    char dir[PATH_MAX];
    memset(dir, '\0', PATH_MAX);
    strncat(dir, curr_dir, strlen(curr_dir));

    int rc = dirseq_impl(dir, dirs);

    /* log_debug("dirseq len (postimpl): %d", utarray_len(dirs)); */
    /* char **p = NULL; */
    /* log_debug("dirseq impl result:"); */
    /* while ( (p=(char**)utarray_next(dirs, p))) { */
    /*     printf("\t%s\n", *p); */
    /* } */

    // FIXME: remove root dir?

    return rc;
}

/* int dirseq_impl(UT_string *curr_dir, UT_array *dirs) */
int dirseq_impl(char *curr_dir, UT_array *dirs)
{
    /* log_debug("dirseq_impl %p", dirs); */

    DIR *the_dir;
    struct dirent *dir_entry;
    errno = 0;
    the_dir = opendir(curr_dir);
    if (the_dir == NULL) {
        errnum = errno;
        printf("opendir failure for %s", curr_dir);
        fprintf(stderr, "Value of errno: %d\n", errnum);
        fprintf(stderr, "opendir error %s: %s\n", curr_dir, strerror( errnum ));
        exit(1);
        /* return(-1); */
    }
    /* log_debug("opened dir %s", curr_dir); */
    utarray_push_back(dirs, &curr_dir);

    if (the_dir) {
        while ((dir_entry = readdir(the_dir)) != NULL) {
            if ( dir_entry->d_type == DT_DIR ) {
                if ( dir_entry->d_name[0] == '.' ) {
                    if (strncmp(dir_entry->d_name, ".pack", 5) == 0) {
                        ;
                        /* what about .private in OPAM repo? found in ounit2, dune-configurator */
                    } else {
                        /* log_debug("ignoring dir: %s", dir_entry->d_name); */
                        continue;
                    }
                }
                strncat(curr_dir, "/", 1);
                strncat(curr_dir, dir_entry->d_name, strlen(dir_entry->d_name));
                /* log_debug("recurring on: %s", curr_dir); */
                dirseq_impl(curr_dir, dirs);
                curr_dir[strlen(curr_dir) - strlen(dir_entry->d_name) - 1] = '\0';
            } else {
                if (dir_entry->d_type == DT_REG) {
                    ; /* log_debug("ignoring regular file: %s", dir_entry->d_name);           /\* ignore *\/ */
                }
            }
        }
        closedir(the_dir);
    }
    return(0);
}

/* emulate a closure */
char *root_dir;

int fileseq(char *_root_dir,
            UT_array *subdirs,     /* string list */
            UT_array *files)
{
    log_debug("fileseq");
    /* UT_string *dir; */
    /* utstring_new(dir); */
    /* utstring_printf(dir, "%s", curr_dir); */
    /* log_debug("fileseq len (preimpl): %d", utarray_len(files)); */

    root_dir = _root_dir;

    char curr_dir[PATH_MAX];
    char rel_dir[PATH_MAX];

    /* char **p = NULL; */
    /* while ( (p=(char**)utarray_next(, p)) ) { */
    /*     free(p); */
    /* } */
    utarray_clear(files);

    char **p = NULL;
    while ( (p=(char**)utarray_next(subdirs, p)) ) {

        /* memset(curr_dir, '\0', PATH_MAX); */
        /* strncat(curr_dir, root_dir, strlen(root_dir)); */
        /* strncat(curr_dir, "/", 1); */
        /* strncat(curr_dir, *p, strlen(*p)); */

        memset(rel_dir, '\0', PATH_MAX);
        /* strncat(reldir_dir, "/", 1); */
        strncat(rel_dir, *p, strlen(*p));
        /* rel_dir[0] = '.'; */

        /* log_debug("fileseq for %s/%s", root_dir, rel_dir); */

        int rc = fileseq_impl(rel_dir, files);

        if (rc) {
            log_error("fileseq_impl rc: %d", rc);
        } else {
        }
        // FIXME: remove root dir?
    }

    /* log_debug("fileseq len (postimpl): %d", utarray_len(files)); */
    /* char **p = NULL; */
    /* log_debug("fileseq impl result:"); */
    /* while ( (p=(char**)utarray_next(files, p))) { */
    /*     printf("\t%s\n", *p); */
    /* } */

    return 0;
}

int fileseq_impl(char* rel_dir, UT_array *files)
{
    /* log_debug("fileseq_impl rootdir: %s, reldir: %s", root_dir, rel_dir); */

    static curr_dir[PATH_MAX];
    memset(curr_dir, '\0', PATH_MAX);
    strncat(curr_dir, root_dir, strlen(root_dir));
    strncat(curr_dir, "/", 1);
    strncat(curr_dir, rel_dir, strlen(rel_dir));

    DIR *the_dir;
    struct dirent *dir_entry;
    errno = 0;
    the_dir = opendir(curr_dir);
    if (the_dir == NULL) {
        errnum = errno;
        perror(curr_dir);
        log_fatal("fileseq_impl opendir");
        /* printf("opendir failure for %s", curr_dir); */
        /* fprintf(stderr, "Value of errno: %d\n", errnum); */
        /* fprintf(stderr, "opendir error %s: %s\n", curr_dir, strerror( errnum )); */
        exit(1);
        /* return(-1); */
    }
    /* log_debug("opened dir %s", curr_dir); */

    if (the_dir) {
        while ((dir_entry = readdir(the_dir)) != NULL) {
            if (dir_entry->d_type == DT_REG) {
                if ( (fext(dir_entry->d_name) == ML)
                     || (fext(dir_entry->d_name) == MLI) ) {

                    strncat(rel_dir, "/", 1);
                    strncat(rel_dir, dir_entry->d_name, strlen(dir_entry->d_name));
                    /* log_debug("pushing %s", rel_dir); */
                    utarray_push_back(files, &rel_dir);
                    rel_dir[strlen(rel_dir) - strlen(dir_entry->d_name) - 1] = '\0';

                } else {
                    /* log_debug("skipping %s/%s", curr_dir, dir_entry->d_name); */
                }

            } else {
                if ( dir_entry->d_type == DT_DIR ) {
                    if ( (dir_entry->d_name[0] == '.') ) {
                        if (strncmp(dir_entry->d_name, ".pack", 5) == 0) {
                            ;
                            /* what about .private in OPAM repo? found in ounit2, dune-configurator */
                        } else {
                            /* log_debug("ignoring dir: %s", dir_entry->d_name); */
                            continue;
                        }
                    }
                    /* strncat(curr_dir, "/", 1); */
                    /* strncat(curr_dir, dir_entry->d_name, strlen(dir_entry->d_name)); */
                    strncat(rel_dir, "/", 1);
                    strncat(rel_dir, dir_entry->d_name, strlen(dir_entry->d_name));

                    /* log_debug("recurring on: %s", rel_dir); */
                    fileseq_impl(rel_dir, files);

                    /* curr_dir[strlen(curr_dir) - strlen(dir_entry->d_name) - 1] = '\0'; */
                    rel_dir[strlen(rel_dir) - strlen(dir_entry->d_name) - 1] = '\0';
                } else {
                    ; /* skip all other types */
                }
            }
        }
        closedir(the_dir);
    }
    return 0;
}
