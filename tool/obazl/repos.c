#include <ctype.h>
#include <dirent.h>
#include <errno.h>
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

#include <sys/types.h>
#include <sys/stat.h>

#include "utarray.h"
#include "uthash.h"

#include "repos.h"

int errnum;
int rc;

#if INTERFACE
struct sdk_dir {
    char dir[PATH_MAX];
    UT_hash_handle hh;
};
#endif

struct sdk_dir *sdk_dirs = NULL;

#if INTERFACE
enum lib_type_e { OCAMLLIB, COQLIB } // , COQPLUGIN }
struct repo_s {
    char name[128];
    char *base_path;
    enum lib_type_e lib_type;
    bool is_coq_plugin;
    bool has_CoqProject;           /* _CoqProject path inferrable from base_path */
    UT_array *dir_seq;
    UT_array *coq_srcs;
    UT_array *ocaml_srcs;
    UT_hash_handle hh;
};
#endif

struct repo_s *repo_map = NULL;

struct repo_s *the_repo = NULL; /* work struct */
struct repo_s *tmp_repo = NULL; /* work struct */

/* static const char f[] = "data/test.META"; */

/* #define BUFSIZE 1024 */

/* char output_base[BUFSIZE] = {0}; */

/* char basedir[BUFSIZE] = {0}; */
char coqlib[PATH_MAX];

/* char symlink_src[BUFSIZE] = {0}; */
/* char symlink_tgt[BUFSIZE] = {0}; */

int find_coqc(void)
{
    char *cmd = "coqc -where";

    FILE *fp;

    if ((fp = popen(cmd, "r")) == NULL) {
        printf("Error opening pipe!\n");
        return -1;
    }

    while (fgets(coqlib, PATH_MAX, fp) != NULL) {
        /* printf("OUTPUT: %s\n", coqlib); */
        coqlib[strcspn(coqlib, "\n")] = 0;
    }

    if(pclose(fp))  {
        printf("Command not found or exited with error status\n");
        return -1;
    }

    return 0;
}

/* void enum_workspace_repos() */
/* { */
/*     static char *bazel_output_base = get_bazel_output_base(); */
/*     int bazel_output_base_len = strlen(bazel_output_base); */
/*     printf("bazel_output_base: '%s'\n", bazel_output_base); */
/*     char cmd[512]; */
/*     sprintf(cmd, "ls %s/external/", bazel_output_base); */
/*     printf("cmd: %s\n", cmd); */
/*     FILE *fp; */
/*     if ((fp = popen(cmd, "r+")) == NULL) { */
/*         perror("enum_repos"); */
/*         printf("Error opening pipe for repo enum!\n"); */
/*         exit(EXIT_FAILURE); */
/*     } */

/*     if (verbosity >= 2) */
/*         printf("action: %s\n", cmd); */
/*     char buf[1024]; */
/*     while (fgets(buf, sizeof(buf), fp) != NULL) { */
/*         /\* if (keep) *\/ */

/*         if (buf[0] != '@' */
/*             && (strncmp(buf, "ppx", 3) != 0) */
/*             && (strncmp(buf, "ocaml", 5) != 0) */
/*             && (strncmp(buf, "bazel_", 6) != 0) */
/*             && (strncmp(buf, "local_", 6) != 0) */
/*             && (strncmp(buf, "rules_", 6) != 0) */
/*             && (strncmp(buf, "obazl_", 6) != 0) */
/*             && (strncmp(buf, "platforms", 9) != 0) */
/*             && (strncmp(buf, "commands_overloads", 18) != 0) ) { */
/*             rc = strcspn(buf, "\n"); */
/*             buf[rc] = '\0'; */

/*             the_repo = calloc(sizeof(struct repo_s), 1); */
/*             strncpy(the_repo->name, buf, */
/*                     strcspn(buf, "\n")); /\* remove newline *\/ */
/*             /\* bazel_output_base is ptr to static work_dir *\/ */
/*             mystrcat(bazel_output_base, "/external/"); */
/*             mystrcat(bazel_output_base, buf); */
/*             the_repo->base_path = strdup(bazel_output_base); */
/*             HASH_ADD_STR(repo_map, name, the_repo); */
/*             utarray_new(the_repo->dir_seq, &ut_str_icd); */

/*             /\* str was strduped, so we truncate it back to the base *\/ */
/*             bazel_output_base[bazel_output_base_len] = '\0'; */
/*         } */
/*     } */
/*     rc = pclose(fp); */
/*     if (rc) { */
/*         exit(-1); */
/*     } */
/* } */

void enumerate_lib_deps() // FILE *fp)
{
    printf("enumerate_lib_deps\n");
    /* find_coqc(); */

    the_repo = NULL;
    tmp_repo = NULL;
    HASH_ITER(hh, repo_map, the_repo, tmp_repo) {

        /* printf("enumerate_lib_deps\n"); */
        work_buf[0] = '\0';
        sprintf(work_buf, "%s", the_repo->base_path);
        printf("work_buf: %s\n", work_buf);
        /* struct sdk_dir *sd = NULL; */
        /* /\* add base dir *\/ */
        /* HASH_FIND_STR(sdk_dirs, work_buf, sd);  /\* already in the hash? *\/ */
        /* if (sd == NULL) { */
        /*     if (verbosity >= 3) */
        /*         printf("adding sdk dir: %s\n", work_buf); */
        /*     sd = malloc(sizeof(struct sdk_dir)); */
        /*     strcpy(sd->dir, work_buf); */
        /*     HASH_ADD_STR(sdk_dirs, dir, sd); */
        /* } */

        if (verbosity == 1)
            printf("Enumerating sdk dirs\n");

        /* dir_seq(coqlib, ""); */
        dir_seq(the_repo, the_repo->base_path, "");

        if (verbosity == 1)
            printf("\n");
    }
}

void handle_include_directive(char *token, char *ctx)
{
    printf("handle_include_directive: %s\n", token);
    token = strtok_r(NULL, " ", &ctx);
    /* tmp_repo = NULL; */
    while (token) {
        printf("\tnext token: %s\n", token);
        token = strtok_r(NULL, " ", &ctx);
    }
}

void handle_q_directive(char *token, char *ctx)
{
    printf("handle_q_directive: %s\n", token);
    token = strtok_r(NULL, " ", &ctx);
    /* tmp_repo = NULL; */
    while (token) {
        printf("\tnext token: %s\n", token);
        token = strtok_r(NULL, " ", &ctx);
    }
}

void handle_r_directive(char *token, char *ctx)
{
    printf("handle_r_directive: %s\n", token);
    token = strtok_r(NULL, " ", &ctx);
    /* tmp_repo = NULL; */
    while (token) {
        printf("\tnext token: %s\n", token);
        token = strtok_r(NULL, " ", &ctx);
    }
}

void handle_assign_directive(char *token, char *line)
{
    printf("handle_assign_directive: %s\n", line);
    /* token = strtok(NULL, " "); */
    /* tmp_repo = NULL; */
    /* while (token) { */
    /*     printf("\tnext token: %s\n", token); */
    /*     token = strtok(NULL, " "); */
    /* } */
}

void handle_other_directive(char *token, char *line)
{
    /* printf("handle_other_directive: %s\n", line); */

    /* token = strtok(NULL, " "); */
    /* /\* tmp_repo = NULL; *\/ */
    /* while (token) { */
    /*     printf("\tnext token: %s\n", token); */
    /*     token = strtok(NULL, " "); */
    /* } */
}

void parse_coqproject_line(struct repo_s *the_repo, char *line)
{
    /* printf("parse_coqproject_line: %s\n", line); */
    char *the_line = strdup(line);
    char *ctx;

    char *token = strtok_r(line, " ", &ctx);
    /* char *token = strchr(line, ' '); */
    /* printf("first token: %s\n", token); */
    int rc;
    while (token) {
        /* tmp_repo = NULL; */
        /* printf("\tnext token: %s\n", token); */
        (strncmp(token, "-I", 2) == 0)
            ? handle_include_directive(token, ctx)
            : (strncmp(token, "-Q", 2) == 0)
            ? handle_q_directive(token, ctx)
            : (strncmp(token, "-R", 2) == 0)
            ? handle_r_directive(token, ctx)
            : is_uppercase(token)
            ? handle_assign_directive(token, the_line)
            : handle_other_directive(token, the_line);
        break;
        /* if (rc) */
        /*     break; */
        /* if (token) */
        /*     token = strtok(NULL, " "); */
    }
    free(the_line);
}

void update_CoqProject(struct repo_s *the_repo)
{
    /* parse and save: _CoqProject, mllib/mlpack files  */
    work_buf[0] = '\0';
    mystrcat(work_buf, the_repo->base_path);
    mystrcat(work_buf, "/_CoqProject");

    FILE *fin;
    fin = fopen(work_buf, "r");
    if (fin == NULL) {
        fprintf(stderr, "update_CoqProject: ");
        perror(work_buf);
        /* exit( EXIT_FAILURE ); */
    }
    memset(work_buf, 0, sizeof work_buf);
    while (fgets(work_buf, sizeof work_buf, fin) != NULL) {
        size_t p = strcspn(work_buf, "\n"); /* remove trailing newline */
        work_buf[p] = '\0';
        parse_coqproject_line(the_repo, work_buf);
    }

    printf("Updating CoqProject data for %s\n", work_buf);
    fclose(fin);
}

/* void update_CoqProject_files() */
/* { */
/*     printf("update_CoqProject_files\n"); */
/*     HASH_ITER(hh, repo_map, the_repo, tmp_repo) { */
/*         if (the_repo->lib_type == COQLIB) { */
/*             update_CoqProject(the_repo); */
/*         } */
/*     } */
/* } */

bool is_coq_lib(char *path)
{
    work_buf[0] = '\0';
    mystrcat(work_buf, path);
    mystrcat(work_buf, "/COQLIB.obazl");
    rc = access(work_buf, R_OK);
    if (rc == 0)
        return true;
    else
        return false;
}

bool has_CoqProject(char *path)
{
    work_buf[0] = '\0';
    mystrcat(work_buf, path);
    mystrcat(work_buf, "/COQLIB.obazl");
    rc = access(work_buf, R_OK);
    if (rc == 0)
        return true;
    else
        return false;
}

bool has_mllib(char *path)
{
    work_buf[0] = '\0';
    /* mystrcat(work_buf, path); */
    /* mystrcat(work_buf, "/_CoqProject"); */
    /* rc = access(work_buf, R_OK); */
    /* if (rc == 0) */
    /*     return true; */
    /* else */
    /*     return false; */
    return false;
}

void update_repo_map(char *repo, char *path) // , enum lib_type_e type)
{
    printf("update_repo_map: %s\n", repo);
    enum lib_type_e type;
    if (is_coq_lib(path))
        type = COQLIB;
    else
        type = OCAMLLIB;
    printf("lib type: %d\n", type);

    the_repo = calloc(sizeof(struct repo_s), 1);
    strncpy(the_repo->name, repo, strlen(repo));
    the_repo->base_path = path;
    the_repo->lib_type = type;
    /* if (type == COQLIB) { */
    /*     the_repo->has_CoqProject = true; */
    /*     update_CoqProject(the_repo); */
    /* /\* } else { *\/ */
    /* } */
    HASH_ADD_STR(repo_map, name, the_repo);

    /* update dir_seq: list of relevant dirs for repo */
    utarray_new(the_repo->dir_seq, &ut_str_icd); /* initialize */

}

void resolve_repos(char *repos) // , enum lib_type_e type)
{
    printf("resolve_repos: %s\n", repos);

    char *ctx;
    char *repo = strtok_r(repos, " ", &ctx);
    /* printf("repo: %s\n", repo); */

    while (repo) {
        tmp_repo = NULL;
        char *d = get_workspace_dir(repo); /* result is callocated */
        printf("REPO: %s -> %s\n", repo, d);

        update_repo_map(repo, d); //, type);

        /* HASH_FIND_STR(repo_map, repo, tmp_repo);  /\* already in the hash? *\/ */
        /* if (tmp_repo) { */
        /*     printf("%s -> %s\n", tmp_repo->name, tmp_repo->base_path); */
        /* } else { */
        /*     fprintf(stderr, "No WS repo found for '%s' listed in .obazlrc\n", repo); */
        /*     exit(EXIT_FAILURE); */
        /* } */
        repo = strtok_r(NULL, " ", &ctx);
    }
}

/* void resolve_ocaml_repos(char *repos) */
/* { */
/*     printf("resolve_ocaml_repos: %s\n", repos); */
/*     resolve_repos(repos, OCAMLLIB); */
/* } */

/* void resolve_coq_repos(char *repos) */
/* { */
/*     printf("resolve_coq_repos: %s\n", repos); */
/*     resolve_repos(repos, COQLIB); */

/*     /\* process _CoqProject files if they exist *\/ */
/*     update_CoqProject_files(); */
/* } */
