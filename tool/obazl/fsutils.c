/* fs_utils.c */
#include <assert.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>              /* open() */
#include <libgen.h>             /* for basename() */
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>

#include "utarray.h"
#include "uthash.h"

#include "fsutils.h"

int verbosity;
int errnum;
int rc;

char work_buf[PATH_MAX];

static char bazel_output_base[PATH_MAX];
static int bazel_output_base_len;

enum extension {
    NOEXT, BAZEL,
    ML, MLI, MLG, MLL, MLY, MLLIB, MLPACK,
    CMI, CMX, CMO, CMA, CMXA, CMXS, CMT, CMTI,
    VO, VOS, VOK, V,
    GLOB,
    SO, A, O, H
};

/* int extlen[22] = { */
/*     [NOEXT] = 0, */
/*     [MLPACK]= 7, */
/*     [MLLIB] = 6, */
/*     [BAZEL] = 6, */
/*     [CMXA]  = 5, */
/*     [CMXS]  = 5, */
/*     [GLOB]  = 5, */
/*     [CMI]   = 4, */
/*     [CMX]   = 4, */
/*     [CMO]   = 4, */
/*     [CMA]   = 4, */
/*     [MLG]   = 4, */
/*     [MLI]   = 4, */
/*     [MLL]   = 4, */
/*     [MLY]   = 4, */
/*     [VOS]   = 4, */
/*     [VOK]   = 4, */
/*     [VO]    = 3, */
/*     [ML]    = 3, */
/*     [A]     = 2, */
/*     [O]     = 2, */
/*     [V]     = 2 */
/* }; */

/* char* mystrcat( char* dest, char* src ) */
/* { */
/*      while (*dest) dest++; */
/*      while ( (*dest++ = *src++) ); */
/*      return --dest; */
/* } */

/* https://stackoverflow.com/questions/2736753/how-to-remove-extension-from-file-name */
/* const enum extension fext(const char *filename) { */
/*     const char *dot = strrchr(filename, '.'); */
/*     if(!dot || dot == filename) return NOEXT; */
/*     if (0 == strncmp(".ml", dot, 4)) { */
/*         return ML; */
/*     } */
/*     if (0 == strncmp(".mlpack", dot, 7)) { */
/*         return MLPACK; */
/*     } */
/*     if (0 == strncmp(".mllib", dot, 6)) { */
/*         return MLLIB; */
/*     } */
/*     if (0 == strncmp(".mli", dot, 4)) { */
/*         return MLI; */
/*     } */
/*     if (0 == strncmp(".mlg", dot, 4)) { */
/*         return MLG; */
/*     } */
/*     if (0 == strncmp(".mll", dot, 4)) { */
/*         return MLL; */
/*     } */
/*     if (0 == strncmp(".mly", dot, 4)) { */
/*         return MLY; */
/*     } */

/*     if (0 == strncmp(".cmxa", dot, 5)) { */
/*         return CMXA; */
/*     } */
/*     if (0 == strncmp(".cmxs", dot, 5)) { */
/*         return CMXS; */
/*     } */
/*     if (0 == strncmp(".cmi", dot, 4)) { */
/*         return CMI; */
/*     } */
/*     if (0 == strncmp(".cmx", dot, 4)) { */
/*         return CMX; */
/*     } */
/*     if (0 == strncmp(".cmo", dot, 4)) { */
/*         return CMO; */
/*     } */
/*     if (0 == strncmp(".cma", dot, 4)) { */
/*         return CMA; */
/*     } */

/*     if (0 == strncmp(".vok", dot, 4)) { */
/*         return VOK; */
/*     } */
/*     if (0 == strncmp(".vos", dot, 4)) { */
/*         return VOS; */
/*     } */
/*     if (0 == strncmp(".vo", dot, 3)) { */
/*         return VO; */
/*     } */
/*     if (0 == strncmp(".a", dot, 2)) { */
/*         return A; */
/*     } */
/*     if (0 == strncmp(".o", dot, 2)) { */
/*         return O; */
/*     } */
/*     if (0 == strncmp(".v", dot, 2)) { */
/*         return V; */
/*     } */
/*     if (0 == strncmp(".glob", dot, 5)) { */
/*         return GLOB; */
/*     } */

/*     if (0 == strncmp(".bazel", dot, 6)) { */
/*         return BAZEL; */
/*     } */

/*     /\* printf("UNRECOGNIZED ext: %s\n", dot); *\/ */
/*     return NOEXT; */
/* } */

void set_bazel_output_base()
{
    /* printf("set_bazel_output_base\n"); */
    char *cmd = "bazel info output_base 2>/dev/null";
    FILE *fp;
    if ((fp = popen(cmd, "r")) == NULL) {
        printf("set_bazel_output_base: error opening pipe\n");
        perror(cmd);
        exit(-1);
    }
    bazel_output_base[0] = '\0';
    if (fgets(bazel_output_base, sizeof bazel_output_base, fp) != NULL) {
        rc = strcspn(bazel_output_base, "\n"); /* remove newline */
        bazel_output_base[rc] = '\0';
        mystrcat(bazel_output_base, "/external");
        bazel_output_base_len = strlen(bazel_output_base);
    } else {
        fprintf(stderr, "fgets: ");
        perror(cmd);
        exit(EXIT_FAILURE);
    }
    rc = pclose(fp);
    if (rc) {
        perror("set_bazel_output_base pclose");
        exit(EXIT_FAILURE);
    }
    /* printf("bazel_output_base: %s\n", bazel_output_base); */
}

/*
  client must free
 */
char *get_workspace_dir(char *the_dir)
{
    /* printf("get_workspace_dir: %s\n", the_dir); */
    char *the_path = calloc(1, bazel_output_base_len + strlen(the_dir) + 1); /* add 1 for '/' */

    mystrcat(the_path, bazel_output_base);
    mystrcat(the_path, "/");
    mystrcat(the_path, the_dir);

    rc = access(the_path, R_OK);
    if (rc != 0) {
        fprintf(stderr, "get_workspace_dir: ");
        perror(the_path);
        exit(EXIT_FAILURE);
    }
    return the_path;
}

/*
  get_pkg - removes fs prefix and basename from absolute path, leaving Bazel package path.
  prefix is cwd (?)
  in:  path  ptr to absolute path
 */
char *get_pkg(char *path)
{
    /* printf("get_pkg for %s\n", path); */
    char *final_slash = strrchr(path, '/');
    char *pkg = strndup(path + cwd_len + 1, (final_slash - (path + cwd_len + 1)));
    /* printf("computed pkg: %s\n", pkg); */
    return pkg;
}

/*
  returns name of copied file
 */
char *mycp(char *src, char *destdir)
{
    /* char content[ BUFSIZE ]; */

    mkdir_r(".", destdir);
    char *pkg = get_pkg(src);
    mkdir_r(destdir, pkg);

    char *bname = basename(src);

    /* printf("copying %s to %s/%s/%s\n", src, destdir, pkg, bname); */

    /* FILE *fin; */
    /* fin = fopen(src, "r"); */
    /* if (fin == NULL) { */
    /*     perror( "open for read failed" ); */
    /*     fprintf(stderr, "fopen fail for input file %s\n", src); */
    /*     exit( EXIT_FAILURE ); */
    /* } */
    int fin;
    errno = 0;

    /* file copy using sendfile */
    /* https://www.informit.com/articles/article.aspx?p=23618&seqNum=13 */

    /* printf("mycp opening for read: %s\n", src); */
    fin = open(src, O_RDONLY | O_NONBLOCK); //, 0666);
    if (fin <= 0) {
        errnum = errno;
        fprintf(stderr, "Error opening file '%s': %s\n", src, strerror( errnum ));
        perror( "open for read failed" );
        fprintf(stderr, "open fail rc %d for input file %s\n", fin, src);
        exit( EXIT_FAILURE );
    }
    /* printf("opened %s for reading, fd: %d\n", src, fin); */

    /* struct stat stat_buf; */
    /* off_t offset = 0; */
    /* fstat(fin, &stat_buf); */
    /* int sz_fin = st.st_size; */

    char *outfname = (char*)calloc(1, PATH_MAX);
    if (outfname == NULL) {
        perror("calloc failure");
        close(fin);
        exit(EXIT_FAILURE);
    }
    /* printf("pkg: %s\n", pkg); */
    /* printf("bname: %s\n", bname); */
    mystrcat(outfname, destdir);
    mystrcat(outfname, "/");
    mystrcat(outfname, pkg);
    mystrcat(outfname, "/");
    mystrcat(outfname, bname);
    /* printf("outfname: %s\n", outfname); */

    /* FILE *fout; */
    /* fout = fopen(outfname, "w"); */
    /* if (fout == NULL) { */
    /*     perror(outfname); */
    /*     fprintf(stderr, "mycp fopen(%s, 'w')\n", outfname); */
    /*     fclose(fin); */
    /*     exit(EXIT_FAILURE); */
    /* } */
    int fout;
    fout = open(outfname, O_CREAT | O_TRUNC | O_WRONLY, 0666); //, stat_buf.st_mode); // same perms as input
    if (fout < 0) {
        perror(outfname);
        fprintf(stderr, "mycp open(%s, O_CREAT | O_TRUNC | O_WRONLY)\n", outfname);
        close(fin);
        exit(EXIT_FAILURE);
    }
    /* printf("opened %s for writing with fd %d\n", outfname, fout); */

    ssize_t inbytes;
    ssize_t outbytes;
    char inbuf[32000];
    while(1) {
        inbytes = read(fin, &inbuf[0], sizeof(inbuf));
        /* printf("read %d bytes\n", inbytes); */
        if (!inbytes) break;
        assert(inbytes > 0);
        if (inbytes < 0) {
            perror("reading for copy");
            fprintf(stderr, "mycp read fin error\n");
            close(fin);
            close(fout);
            exit(EXIT_FAILURE);
        } else {
            outbytes = write(fout, &inbuf[0], inbytes);
            /* assert(outbytes == inbytes); */
            if (outbytes != inbytes) {
                fprintf(stderr, "inbytes: %ld, outbytes: %ld", inbytes, outbytes);
                close(fin);
                close(fout);
                exit(EXIT_FAILURE);
            }
        }
    }
    // assumption: input is writable (ascii)
    /* char ch; */
    /* while( ( ch = fgetc(fin) ) != EOF ) */
    /*     fputc(ch, fout); */

    /* errno = 0; */
    /* static char buf[PATH_MAX]; */
    /* buf[0] = '\0'; */
    /* int rc = 0; */
    /* while (fgets(buf, BUFSIZE, fin) != NULL) { */
    /*     rc = fputs(buf, fout); */
    /*     /\* if (rc == EOF) *\/ */
    /*     /\* errno = 0; *\/ */
    /* } */

    /* without flushing, coqpp may run before the content has been
       written to disk. just closing the fd does not mean the write
       has finished.
    */
    /* fflush(fout); */
    /* printf("copied %s\n", src); */

    /* fclose(fin); */
    /* fclose(fout); */
    close(fin);
    close(fout);

    free(pkg);
    return outfname;
}

/*
  base_path changes as we recur
 */
void dir_seq(struct repo_s *repo, char *base_path, char *segment)
{
    if (verbosity == 1)
        printf(":");
    /* printf("dir_seq: base_path(%s), segment(%s)\n", base_path, segment); */
    errno = 0;
    /* char *this_path = (char*)calloc(1, PATH_MAX); */
    char this_path[PATH_MAX];
    memset(this_path, 0, sizeof this_path);

    /* if (this_path == NULL) { */
    /*     perror("calloc failure"); */
    /*     fprintf(stderr, "enumerate_files(%s, %s)\n", base_path, segment); */
    /*     exit(EXIT_FAILURE); */
    /* } */

    mystrcat(this_path, base_path);
    if ( *segment != '\0' ) {
        mystrcat(this_path, "/");
        mystrcat(this_path, segment);
    }

    /* printf("....enumerating %s\n", this_path); */
    struct dirent *dir_entry;
    errno = 0;
    DIR *dir = opendir(this_path);

    if (!dir) {
        errnum = errno;
        /* printf("opendir fail for %s, rc: %d\n", this_path, errnum); */
        /* if (errnum == ENOENT) */
        /*     fprintf(stderr, "opendir ENOENT: %d\n", errnum); */
        /* fprintf(stderr, "opendir error: %d\n", errnum); */
        fprintf(stderr, "Error opening dir %s: %s\n", this_path, strerror( errnum ));
        exit(1);
    }

    while ((dir_entry = readdir(dir)) != NULL)
    {
        /* printf("checking '%s/%s' ... ", this_path, dir_entry->d_name); */
        /* printf("dir_entry->d_name[0]: %c (%d)\n", dir_entry->d_name[0], (dir_entry->d_name[0] != '.')); */
        if ((dir_entry->d_name[0] != '.') // ignore all hidden dirs
            /* && strcmp(dir_entry->d_name, ".") != 0 */
            /* && strcmp(dir_entry->d_name, "..") != 0 */
            && strcmp(dir_entry->d_name, "_build") != 0  // dune output dir
            && strcmp(dir_entry->d_name, ".git") != 0
            && strcmp(dir_entry->d_name, ".obazl.d") != 0
            )
        {
            /* printf(" processing "); // , this_path, dir_entry->d_name); */
            if (dir_entry->d_type == DT_DIR) {
                /* printf("(directory %s)\n", dir_entry->d_name); */
                /* memset(work_buf, 0, sizeof work_buf); */
                sprintf(work_buf, "%s/%s", this_path, dir_entry->d_name);
                /* static struct sdk_dir *sd = NULL; */
                /* HASH_FIND_STR(sdk_dirs, work_buf, sd);  /\* already in the hash? *\/ */
                /* printf("pushing %s to %s\n", work_buf, repo->name); */

                /* push makes a copy of the src string, but passing &work_buf does not work (why?) */
                char *test = strdup(work_buf);
                utarray_push_back(repo->dir_seq, &test); // &work_buf);

                /* if (sd == NULL) { */
                /*     /\* if (verbosity >= 3) *\/ */
                /*     /\* printf("adding sdk dir: %s\n", work_buf); *\/ */
                /*     sd = malloc(sizeof(struct sdk_dir)); */
                /*     strcpy(sd->dir, work_buf); */
                /*     HASH_ADD_STR(sdk_dirs, dir, sd); */
                /* } */
                /* printf(" enum recurring on directory: this_path(%s), segment(%s)\n", this_path, dir_entry->d_name); */
                dir_seq(repo, this_path, dir_entry->d_name);
                /* printf(" enum resuming after this_path(%s), segment(%s)\n", this_path, dir_entry->d_name); */
            /* } else { */
            /*     printf("skipping %s\n", dir_entry->d_name); */
            }
        }
    }
    /* free(this_path); */
    closedir(dir);
}

/*
 */
void enumerate_files_r(char *base_path, char *segment)
{
    if (verbosity == 1)
        printf(":");
    /* printf("enumerate_files_r: base_path(%s), segment(%s)\n", base_path, segment); */
    errno = 0;
    /* char *this_path = (char*)calloc(1, PATH_MAX); */
    char this_path[PATH_MAX];
    memset(this_path, 0, sizeof this_path);

    /* if (this_path == NULL) { */
    /*     perror("calloc failure"); */
    /*     fprintf(stderr, "enumerate_files(%s, %s)\n", base_path, segment); */
    /*     exit(EXIT_FAILURE); */
    /* } */

    mystrcat(this_path, base_path);
    if ( *segment != '\0' ) {
        mystrcat(this_path, "/");
        mystrcat(this_path, segment);
    }

    /* printf("....enumerating %s\n", this_path); */
    struct dirent *dir_entry;
    errno = 0;
    DIR *dir = opendir(this_path);

    if (!dir) {
        errnum = errno;
        /* printf("opendir fail for %s, rc: %d\n", this_path, errnum); */
        /* if (errnum == ENOENT) */
        /*     fprintf(stderr, "opendir ENOENT: %d\n", errnum); */
        /* fprintf(stderr, "opendir error: %d\n", errnum); */
        fprintf(stderr, "Error opening dir %s: %s\n", this_path, strerror( errnum ));
        exit(1);
    }

    while ((dir_entry = readdir(dir)) != NULL)
    {
        /* printf("checking '%s/%s' ...", this_path, dir_entry->d_name); */
        /* printf("dir_entry->d_name[0]: %c (%d)\n", dir_entry->d_name[0], (dir_entry->d_name[0] != '.')); */
        if ((dir_entry->d_name[0] != '.') // ignore all hidden dirs
            /* && strcmp(dir_entry->d_name, ".") != 0 */
            /* && strcmp(dir_entry->d_name, "..") != 0 */
            && strcmp(dir_entry->d_name, "_build") != 0  // dune output dir
            && strcmp(dir_entry->d_name, ".git") != 0
            && strcmp(dir_entry->d_name, ".obazl.d") != 0
            )
        {
            /* printf(" processing "); // , this_path, dir_entry->d_name); */
            if (dir_entry->d_type == DT_DIR) {
                /* printf("(directory)\n"); */
                /* printf(" enum recurring on directory: this_path(%s), segment(%s)\n", this_path, dir_entry->d_name); */
                enumerate_files_r(this_path, dir_entry->d_name);
                /* printf(" enum resuming after this_path(%s), segment(%s)\n", this_path, dir_entry->d_name); */
            } else {
                if (verbosity == 1)
                    printf(".");
                sprintf(work_buf, "%s/%s", this_path, dir_entry->d_name);
                char *realp = realpath(work_buf, NULL);
                /* printf("%s\n", realp); */

                enum extension ext = fext(realp + cwd_len + 1);
                static struct preproc *pp = NULL;
                static struct codept_src *cd = NULL;
                switch(ext) {
                case ML:
                case MLI:
                    /* utarray_push_back(codept_srcs, &this_path); */
                    HASH_FIND_STR(codept_srcs, work_buf, cd);  /* already in the hash? */
                    if (cd == NULL) {
                        if (verbosity >= 3)
                            printf("adding codept src: %s (%s)\n", work_buf, realp);
                        cd = malloc(sizeof(struct codept_src));
                        strcpy(cd->dir, work_buf);
                        HASH_ADD_STR(codept_srcs, dir, cd);
                    }
                    break;
                case MLG:
                case MLL:
                case MLY:
                case MLLIB:
                case MLPACK:
                    HASH_FIND_STR(preprocs, realp, pp);  /* already in the hash? */
                    if (pp == NULL) {
                        pp = malloc(sizeof(struct preproc));
                        memset(pp, 0, sizeof *pp);
                        strcpy(pp->name, realp);
                        pp->mlout = NULL;
                        pp->type = ext;
                        HASH_ADD_STR(preprocs, name, pp);
                    }
                    break;
                default:
                    break;
                }

                // FIXME: save ocaml and coq files, to drive codept processing later

                free(realp);


            }
        /* } else { */
        /*     printf("skipping\n"); */
        }
    }
    /* free(this_path); */
    closedir(dir);
}
