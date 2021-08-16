#include <stdlib.h>
#include <string.h>

#include "ini.h"
#include "utarray.h"
#include "uthash.h"

#include "config.h"

#if INTERFACE
struct lib_s {
    char *name;
    char *path;
};

struct configuration_s {
    int obazl_version;
    int libct;
    struct lib_s *ocamllibs[10]; /* is 10 enough? */
    struct lib_s *coqlibs[10]; /* is 10 enough? */
};
#endif

int config_handler(void* data,
                          const char* section, const char* name, const char* value)
{
    printf("config_handler section %s: %s=%s\n", section, name, value);
    struct configuration_s *pconfig = (struct configuration_s*)data;

    #define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0

    if (MATCH("obazl", "version")) {
        // FIXME: verify obazl version
        return 1;
    }

    if (MATCH("obazl", "repos")) {
        resolve_repos((char*)value);
    }

    /* if (MATCH("repos", "coq")) { */
    /*     resolve_coq_repos((char*)value); */
    /* } */

    /* if (MATCH("repos", "ocaml")) { */
    /*     resolve_ocaml_repos((char*)value); */
    /* } */

    /* if ( strncmp(section, "repo:", 5) == 0 ) { */
    /*     /\* printf("REPO section: %s (%s = %s)\n", section, name, value); *\/ */
    /*     char *the_repo = &section[5]; */

    /*     char *repo_dir = get_workspace_dir(the_repo); */
    /*     printf("repo: %s -> %s\n", the_repo, repo_dir); */

    /*     /\* tmp_repo = NULL; *\/ */
    /*     /\* HASH_FIND_STR(repo_map, the_repo, tmp_repo);  /\\* already in the hash? *\\/ *\/ */
    /*     /\* if (tmp_repo) { *\/ */
    /*     /\*     printf("%s -> %s\n", tmp_repo->name, tmp_repo->base_path); *\/ */
    /*     /\* } else { *\/ */
    /*     /\*     fprintf(stderr, "No WS repo found for '%s' listed in .obazlrc\n", the_repo); *\/ */
    /*     /\*     exit(EXIT_FAILURE); *\/ */
    /*     /\* } *\/ */
    /* } */

    if ( strcmp(section, "coqlibs") == 0 ) {
        struct lib_s *cl = calloc(1, sizeof *cl);
        cl->name = strdup(name);
        cl->path = strdup(value);
        pconfig->coqlibs[pconfig->libct] = cl;
        /* printf("loaded lib %d (%p): %s -> %s\n", */
        /*        pconfig->libct, */
        /*        pconfig->coqlibs[pconfig->libct], */
        /*        pconfig->coqlibs[pconfig->libct]->name, */
        /*        pconfig->coqlibs[pconfig->libct]->path); */
        pconfig->libct++;
    }
    return 1;
}

