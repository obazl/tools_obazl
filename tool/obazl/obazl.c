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

/* #include "utringbuffer.h" */
#include "utarray.h"
#include "uthash.h"

#include "ini.h"

#include "enums.h"
#include "obazl.h"

int verbosity = 0;
int errnum;
int rc;

bool keep = false;
bool coq;
bool coq_sdk;

static const char obazl_ini[] = ".obazlrc";

static const char preproc_outbase[] = ".obazl.d/_preproc";

/* /\* #define BUFSIZE 1024 *\/ */
/* /\* #if INTERFACE *\/ */
/* int BUFSIZE; */
/* /\* #endif *\/ */

/* PATH_MAX: 1024 on macos */

LOCAL char cwd[PATH_MAX];
int cwd_len = 0;

LOCAL char src_base[PATH_MAX];
LOCAL char out_base[PATH_MAX];  /* for intermediate output files */

char coqlib[PATH_MAX];
char lib_prefix[6][PATH_MAX]; /* assumption: nobody will depend on more than n local libs */

LOCAL char symlink_src[PATH_MAX];
LOCAL char symlink_tgt[PATH_MAX];

#if INTERFACE
struct preproc {
    char name[PATH_MAX];             /* key (string is WITHIN the structure) */
    char *pkg;
    char *tgt;
    char *mlout;                  /* output file of preprocessing */
    /* int  filect; */
    /* char *files[16]; */
    enum extension type;
    UT_hash_handle hh;         /* makes this structure hashable */
};
#endif

struct preproc *preprocs = NULL;

struct module {
    char name[512];             /* key (string is WITHIN the structure) */
    char *pkg;                  /* ptr into name fld */
    char *tgt;                  /* ptr into name fld */
    int  filect;
    char *files[16];
    /* char cma[128]; */
    /* char cmxa[128]; */
    /* char cmx[128]; */
    /* char cmo[128]; */
    /* char cmi[128]; */
    /* char a[128]; */
    /* char cmxs[128]; */
    UT_hash_handle hh;         /* makes this structure hashable */
} ;

struct module *modules = NULL;

#if INTERFACE
struct package {
    char name[512];             /* key (string is WITHIN the structure) */
    struct module *modules;
    UT_hash_handle hh;         /* makes this structure hashable */
} ;
#endif

struct package *packages = NULL;

void which_tool(char *tool)
{
    if (verbosity == 1)
        printf(".");
    static FILE *fp;
    static char buf[PATH_MAX];

    fp = NULL;
    buf[0] = '\0';
    mystrcat(buf, "which ");
    mystrcat(buf, tool);
    fp = popen(buf, "r");
    if (fp == NULL) {
        /* manpage: "The popen() function does not reliably set errno." */
        fprintf(stderr, "check_tools error opening pipe for cmd 'which %s'\n", tool);
        exit(EXIT_FAILURE);
    }
    errno = 0;
    buf[0] = '\0';
    while (fgets(buf, PATH_MAX, fp) != NULL) {
        if ((verbosity >= 2)) {
            printf("which %-10s: %s", tool, buf);
        }
        errno = 0;
    }
    rc = ferror(fp); /* does not set errno */
    if (rc) {
        fprintf(stderr, "ferror on popen cmd 'which %s': %d\n", tool, rc);
    } else {
        // rc = feof(fp);
    }

    rc = pclose(fp);
    if(rc) {
        fprintf(stderr, "check_tools: 'which %s' failure, rc: %d\n", tool, rc);
        exit(EXIT_FAILURE);
    }
}

void check_tools(void)
{
    if (verbosity == 1)
        printf("Checking tools .");

    which_tool("ocamllex");
    which_tool("ocamlyacc");
    which_tool("menhir");
    which_tool("codept");

    if (coq) {
        which_tool("coqpp");
        which_tool("coqc");
    }

    if (verbosity == 1)
        printf("\n");
}

/* /\* https://stackoverflow.com/questions/2736753/how-to-remove-extension-from-file-name *\/ */
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

void register_file(struct package *pkg_hash, char *dir, char *pkg_base, char *pkg, char *file)
{
    struct module *s = NULL;
    printf("Registering file %s\n", file);
    printf("curdir %s\n", dir);
    printf("package_base %s\n", pkg_base);
    printf("package: %s\n", pkg);
    /* printf("pkg_hash %p\n", pkg_hash); */

    enum extension ext = fext(file);
    /* printf("FEXT %d\n", ext); */

    /* printf("oldfile: %s\n", file); */
    int flen = strlen(file);
    char *newfile = (char *)calloc(1, flen + 1);
    if (newfile == NULL) {
        errnum = errno;
        printf("calloc failure for char *newfile *\n");
        fprintf(stderr, "Value of errno: %d\n", errnum);
        fprintf(stderr, "calloc error %s\n", strerror( errnum ));
        exit(1);
    }

    strncpy(newfile, file, flen + 1);
    /* printf("newfile: %s\n", newfile); */

    char mname[128] = {0};
    int mlen = strlen(file) - extlen[ext];
    strncpy(mname, file, mlen);

    char tgt_label[512] = {0};
    mystrcat(tgt_label, pkg_base);
    mystrcat(tgt_label, ":");
    mystrcat(tgt_label, mname);
    /* printf("tgt_label: %s\n", tgt_label); */
    /* struct module *test_s, *test_tgt_label; */
    /* HASH_ITER(hh, modules, test_s, test_tgt_label) { */
    /*     printf("%s\n", test_s->name); */
    /* } */
    /* HASH_FIND_STR(modules, tgt_label, s);  /\* id already in the hash? *\/ */
    /* s = NULL; */

    if ( pkg_hash->modules != 0 ) {
        /* printf("pkg modules ptr: %p\n", pkg_hash->modules); */
        /* struct module *ms = pkg_hash->modules; */
        /* struct module *tmp_s, *tmp_tmp; */
        /* HASH_ITER(hh, pkg_hash->modules, tmp_s, tmp_tmp) { */
        /*     printf("\tmodules hash: %s\n", tmp_s->name); */
        /* } */
        HASH_FIND_STR(pkg_hash->modules, tgt_label, s);
    }
    /* printf("found? %d\n", s); */
    if (s == NULL) {
        /* first we insert, then we update */
        /* printf("malloc new struct module *\n"); */
        s = (struct module *)malloc(sizeof *s);
        if (s == NULL) {
            errnum = errno;
            printf("malloc failure for struct module *\n");
            fprintf(stderr, "Value of errno: %d\n", errnum);
            fprintf(stderr, "malloc error %s\n", strerror( errnum ));
            exit(1);
        }
        /* s->filect = 0; */
        memset(s, 0, sizeof *s);
        /* printf("s->filect: %d\n", s->filect); */
        strncpy(s->name, tgt_label, strlen(tgt_label));
        s->tgt = strrchr(s->name, ':');
        s->tgt++;            /* skip ':' */
        if ( pkg_hash->modules == 0 ) {
            /* printf("inserting: %s\n", s->name); */
            HASH_ADD_STR(pkg_hash->modules, name, s);
            /* printf("inserted: %s\n", s->name); */
            /* printf("inserted2: %s\n", pkg_hash->modules->name); */
        } else {
            /* printf("adding: %s\n", s->name); */
            HASH_ADD_STR((pkg_hash->modules), name, s);
        }
    }
    s->files[s->filect] = newfile;
    s->filect++;
}

/* void open_file(void) */
/* { */
/*    int num; */
/*    FILE *fptr; */

/*    fptr = fopen(f, "r"); */
/*    if (fptr == NULL){ */
/*        errnum = errno; */
/*        fprintf(stderr, "Value of errno: %d\n", errno); */
/*        fprintf(stderr, "Error opening file %s: %s\n", f, strerror( errnum )); */
/*        exit(1); */
/*    } */

/*    fscanf(fptr,"%d", &num); */

/*    printf("Value of n=%d", num); */
/*    fclose(fptr); */
/* } */

void emit_build_file(struct package *p, FILE *fp)
{
    char fname[128] = {0};
    char depname[128] = {0};
    mystrcat(fname, out_base);
    mystrcat(fname, "/");
    mystrcat(fname, p->name);
    mystrcat(fname, "/BUILD.bazel");
    /* printf("Writing %s\n", fname); */

    fp = fopen(fname, "w");
    if (fp == NULL) {
        errnum = errno;
        fprintf(stderr, "fopen fail for %s\n", fname);
        fprintf(stderr, "Value of errno: %d\n", errno);
        fprintf(stderr, "Error opening file %s: %s\n", fname, strerror( errnum ));
        exit(1);
    }
    fputs("load(\n", fp);
    fputs("    \"@obazl_rules_ocaml//ocaml:rules.bzl\",\n", fp);
    fputs("    \"ocaml_import\"\n", fp);
    fputs(")\n\n", fp);
    fputs("package(default_visibility = [\"//visibility:public\"])\n\n", fp);

    struct module *s, *tmp;
    HASH_ITER(hh, p->modules, s, tmp) {
        fputs("ocaml_import(\n", fp);
        depname[0] = '\0';
        mystrcat(depname, s->tgt);
        /* depname[0] = depname[0]; // - 32; /\* uppercase first char *\/ */
        depname[0] = toupper(depname[0]);
        fprintf(fp, "    name = \"%s\",\n", depname);
        fputs("    srcs = [\n", fp);
        for (int i = 0; i < s->filect; i++) {
            fprintf(fp, "        \"%s\",\n", s->files[i]);
        }
        fputs("    ]\n", fp);
        fputs(")\n\n", fp);
    }
    fclose(fp);
}


/*
  returns name of processed output file; caller must free
 */
char *process_mlg(char *p)
{
    /* printf("process_mlg: %s\n", p); */
    if (verbosity == 1)
        printf(".");
    /* copy src to tmp dir, since coqpp does not support redirecting output */
    char *mlg = mycp(p, (char *)preproc_outbase);

    FILE *fp;
    char cmd[PATH_MAX];
    sprintf(cmd, "coqpp %s", mlg);
    /* sprintf(cmd, "coqpp %s", p); */
    /* printf("Processing %s\n", cmd); */
    /* system(cmd); */
    if ((fp = popen(cmd, "r+")) == NULL) {
        printf("Error opening pipe for coqpp!\n");
        exit(-1);
    }
    if (verbosity >= 2)
        printf("action: %s\n", cmd);
    if(pclose(fp))  {
        printf("Command not found or exited with error status\n");
        exit(-1);
    }
    /* free(mlg); */
    mlg[strlen(mlg) - 1] = '\0';
    return mlg;
}

/*
  returns name of processed output; caller must free
 */
char *process_mll(char *src)
{
    /* printf("process_mll: %s\n", src); */

    mkdir_r(".", (char *)preproc_outbase);

    char *pkg = get_pkg(src);
    mkdir_r((char*)preproc_outbase, pkg);

    char *bname = basename(src);
    bname[strlen(bname)-1] = '\0';

    char *preproc_outdir = calloc(1, PATH_MAX);
    memset(preproc_outdir, 0, sizeof *preproc_outdir);

    mystrcat(preproc_outdir, (char*)preproc_outbase);
    mystrcat(preproc_outdir, "/");
    mystrcat(preproc_outdir, pkg);
    mystrcat(preproc_outdir, "/");
    mystrcat(preproc_outdir, bname);
    /* printf("ocamlex out: %s\n", preproc_outdir); */

    char cmd[PATH_MAX];
    sprintf(cmd, "ocamllex -q %s -o %s", src, preproc_outdir);
    /* system(cmd); */

    FILE *fp;
    if ((fp = popen(cmd, "r+")) == NULL) {
        printf("Error opening pipe for coqpp!\n");
        exit(-1);
    }
    if (verbosity >= 2)
        printf("action: %s\n", cmd);
    /* char buf[1024]; */
    /* while (fgets(buf, sizeof(buf), fp) != NULL) { */
    /*     printf("popen output: %s\n", buf); */
    /* } */
    rc = pclose(fp);
    if (rc) {
        exit(-1);
    }
    return preproc_outdir;
}

char *process_mllib(char *src)
{
    if (verbosity >= 2)
        printf("action: process_mllib: %s\n", src);
    return NULL;
}

char *process_mlpack(char *src)
{
    if (verbosity >= 2)
        printf("action: process_mlpack: %s\n", src);
    return NULL;

}

void process_preprocs(void) //char *base_path)
{
    /* printf("process_preprocs\n"); */
    if (verbosity == 1)
        printf("Preprocessing ");
    /*
      1. find all .mlg files
      2. process with coqpp to produce ml files
      3. run codept (globally) to generate deps
     */

    struct preproc *pp;
    char *mlout;
    for (pp = preprocs; pp != NULL; pp = pp->hh.next) {
        switch(pp->type) {
        case MLG:
            if (coq) {
                mlout = process_mlg(pp->name);
                pp->mlout = mlout;
            }
            break;
        case MLL:
            mlout = process_mll(pp->name);
            pp->mlout = mlout;
            break;
        case MLLIB:
            mlout = process_mllib(pp->name);
            pp->mlout = mlout;
            break;
        case MLPACK:
            mlout = process_mlpack(pp->name);
            pp->mlout = mlout;
            break;
        default:
            if (verbosity >= 2)
                printf("action: pending: %s\n", pp->name);
        }
    }
    if (verbosity == 1)
        printf("\n");

    /* char *cmd = "coqpp"; */

    /* while (fgets(coqlib, BUFSIZE, fp) != NULL) { */
    /*     /\* printf("OUTPUT: %s\n", coqlib); *\/ */
    /*     coqlib[strcspn(coqlib, "\n")] = 0; */
    /* } */
}

/* void config_coq_sdk(char *out_base) */
/* { */
/*     find_coqc(); */

/*     /\* repo rule config *\/ */
/*     /\* now symlink the internal dir tree paralleling the installed opam dir tree *\/ */
/*     link_dir(coqlib, "", "", out_base); */

/*     struct package *p, *ptmp; */
/*     char fname[512] = {0}; */
/*     FILE *fp; */
/*     HASH_ITER(hh, packages, p, ptmp) { */
/*         /\* printf("PKG: %s\n", p->name); *\/ */
/*         emit_build_file(p, fp); */
/*         fname[0] = '\0'; */
/*         mystrcat(fname, out_base); */
/*         mystrcat(fname, "/"); */
/*         mystrcat(fname, p->name); */
/*         mystrcat(fname, "/MANIFEST"); */
/*         /\* printf("Writing %s\n", fname); *\/ */
/*         fp = fopen(fname, "w"); */
/*         if (fp != NULL) { */
/*             /\* fputs(fname, fp); *\/ */
/*             /\* fputs("\n", fp); *\/ */
/*             struct module *s, *tmp; */
/*             HASH_ITER(hh, p->modules, s, tmp) { */
/*                 fputs(s->name, fp); */
/*                 fputs("\n", fp); */
/*                 for (int i = 0; i < s->filect; i++) { */
/*                     fputs(s->files[i], fp); */
/*                     fputs("\n", fp); */
/*                 } */
/*             } */
/*             fclose(fp); */
/*         } else { */
/*             errnum = errno; */
/*             fprintf(stderr, "fopen fail for %s\n", fname); */
/*             fprintf(stderr, "Value of errno: %d\n", errno); */
/*             fprintf(stderr, "Error opening file %s: %s\n", fname, strerror( errnum )); */
/*             exit(1); */
/*         } */
/*         HASH_DEL(packages, p); */
/*     } */

/*     struct module *s, *tmp; */
/*     HASH_ITER(hh, modules, s, tmp) { */
/*         /\* printf("%s\n", s->name); *\/ */
/*         /\* use precision specifier to set nbr chars to print *\/ */
/*         /\* printf("\tPKG: %.*s\n", (s->tgt - s->name - 1), s->name); *\/ */
/*         /\* printf("\tTGT: %s\n", s->tgt); *\/ */
/*         for (int i = 0; i < s->filect; i++) { */
/*             /\* printf("\t%s\n", s->files[i]); *\/ */
/*         } */
/*         /\* printf("\tcmxa: %s\n", s->cmxa); *\/ */
/*         /\* printf("\tcmx:  %s\n", s->cmx); *\/ */
/*         /\* printf("\tcmi:  %s\n", s->cmi); *\/ */
/*         /\* printf("\tcmo:  %s\n", s->cmo); *\/ */
/*         /\* printf("\ta:    %s\n", s->a); *\/ */
/*         HASH_DEL(modules, s); */
/*         free(s); */
/*     } */
/* } */

void display_usage(void)
{
    fprintf(stderr, "Usage: obazl [-s dir] [-o dir] [-chkv]\n"
            "\t-s: source dir (default: './')\n"
            "\t-o: output dir for intermediate files (default: './.obazl.d')\n"
            "\t-k: keep intermediate files\n"
            "\t-c: coq support\n"
            "\t-h: help\n"
            "\t-v: verbosity level 1 (add more vs for more verbosity, up to 5)\n"
            );
}

int main(int argc, char *argv[])
{
    int opt;

    /* if (argc < 2) { */
    /*     mystrcat(src_base, "."); */
    /*     mystrcat(out_base, "."); */
    /*     fprintf(stderr, "WARNING: no src or output dirs specified; using '.'\n"); */
    /* } */

    while ((opt = getopt(argc, argv, "d:hs:o:Cckv")) != -1) {
        switch (opt) {
        case 'c':
            coq = true;
            break;
        case 'C':
            coq_sdk = true;
            break;
        case 'd':
            parse_dunefile(argc, argv);
            break;
        case 'h':
            display_usage();
            exit(EXIT_SUCCESS);
        case 'k':
            keep = true;
            break;
        case 's':
            /* printf("option s: %s\n", optarg); */
            mystrcat(src_base, optarg);
            break;
        case 'o':
            /* printf("option o: %s\n", optarg); */
            mystrcat(out_base, optarg);
            break;
        case 'v':
            verbosity++;
            break;
        default:
            display_usage();
            exit(EXIT_FAILURE);
        }
    }
    if (*src_base == '\0') {
        printf("WARNING: no src dir specified (-s); using '.'\n");
        mystrcat(src_base, ".");
    }
    if (*out_base == '\0') {
        printf("WARNING: no out dir specified (-o); using %s\n", preproc_outbase);
        mystrcat(out_base, (char*)preproc_outbase);
    }

    if (getcwd(cwd, sizeof(cwd)) != NULL) {
        if (verbosity == 1) {
            printf("PATH_MAX: %d\n", PATH_MAX);
            printf("cwd: %s\n", cwd);
        }
        cwd_len = strlen(cwd);
    }

    if (coq_sdk) {
        /* config_coq_sdk(out_base); */
    } else {
        /* enum_workspace_repos(); */
        /* the_repo = NULL; */
        /* tmp_repo = NULL; */
        /* HASH_ITER(hh, repo_map, the_repo, tmp_repo) { */
        /*     printf("WS repo: %s\n", the_repo->name); */
        /* } */

        /* read_CoqProject(); */ /* read the _CoqProject file */

        struct configuration_s config = {.obazl_version = 0, .libct = 0};

        set_bazel_output_base();

        if (ini_parse(obazl_ini, config_handler, &config) < 0) {
            //FIXME: deal with missing .obazl
            printf("Can't load %s\n", obazl_ini);
            return 1;
        }
        printf("Config loaded from %s\n", obazl_ini);
        /* for (int i = 0; i < config.libct; i++) { */
        /*     /\* printf("coqlib %d (%p): \n", i, config.coqlibs[i]); *\/ */
        /*     printf("%s -> %s\n", config.coqlibs[i]->name, config.coqlibs[i]->path); */
        /* } */

        printf("Resolved repos:\n");
        HASH_ITER(hh, repo_map, the_repo, tmp_repo) {
            printf("\ttype %d: %s\n", the_repo->lib_type, the_repo->name);
            if (the_repo->has_CoqProject)
                printf("\t\t_CoqProject: %s/%s\n", the_repo->base_path, "_CoqProject");
            char **p = NULL;
            while ( (p=(char**)utarray_next(the_repo->dir_seq, p)) ) {
                printf("\t\t%s\n",*p);
            }
        }

        check_tools();

        /* if (coq) */
        /*     find_coqc(); */

        /* utarray_new(codept_srcs,&ut_str_icd); */

        if (verbosity == 1)
            printf("Enumerating files ");
        enumerate_files_r(src_base, "");
        if (verbosity == 1)
            printf("\n");

        process_preprocs(); // src_base);

        enumerate_lib_deps();

        /* printf("lib deps:\n"); */
        /* HASH_ITER(hh, repo_map, the_repo, tmp_repo) { */
        /*     printf("\trepo:%s\n", the_repo->name); */
        /*     char **p = NULL; */
        /*     while ( (p=(char**)utarray_next(the_repo->dir_seq, p)) ) { */
        /*         printf("\t\t%s\n",*p); */
        /*     } */
        /* } */

        codept();

        if (verbosity == 4)
            printf("freeing codept_srcs:\n");
        struct codept_src *cd, *cdtmp;
        HASH_ITER(hh, codept_srcs, cd, cdtmp) {
            if (verbosity == 4)
                printf("\t%s\n", cd->dir);
            HASH_DEL(codept_srcs, cd);
            free(cd);
        }

        if (verbosity == 4)
            printf("freeing preproc srcs:\n");
        struct preproc *pp, *pptmp;
        HASH_ITER(hh, preprocs, pp, pptmp) {
            if (verbosity == 4) {
                printf("\t%s\n", pp->name);
            }
            if (pp->mlout) {
                /* printf("\t%s\n", pp->mlout); */
                free(pp->mlout);
            }
            HASH_DEL(preprocs, pp);
            free(pp);
        }
    }

    return 0;
}
