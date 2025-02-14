#include <libgen.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <unistd.h>

#include "gopt.h"
#include "liblogc.h"
#include "librunfiles.h"

//FIXME: we should get cjson from mustachios?
#include "cJSON.h"
#include "cwalk.h"
/* #include "mustache_cjson.h" */
#include "mustachios.h"
#include "utarray.h"
#include "utlist.h"
#include "uthash.h"
#include "utstring.h"

#include "new.h"

int  debug_new; //DEBUG_LEVEL;
bool trace_new; //TRACE_FLAG;

/* #if LOCAL_INTERFACE */
#define DEBUG_LEVEL debug_new
#define TRACE_FLAG trace_new
/* #endif */

/* extern int  rf_debug; */
/* extern bool rf_trace; */

bool verbose;
bool quiet;

/* static struct runfiles_s *runfiles = NULL; */

char *ws_dir;                  /* BUILD_WORKSPACE_DIRECTORY */
char *build_wd;                /* BUILD_WORKING_DIRECTORY */

enum OPTS {
    OPT_TEMPLATE = 0,
    FLAG_LIST,

    FLAG_DEBUG,
    FLAG_TRACE,
    FLAG_VERBOSE,
    FLAG_VERSION,
    FLAG_QUIET,
    FLAG_HELP,
    LAST
};

static struct option options[] = {
    /* 0 */
    [OPT_TEMPLATE] = {.long_name="template",.short_name='t',
                 .flags=GOPT_ARGUMENT_REQUIRED},
    [FLAG_LIST] = {.long_name="list",.short_name='l',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG] = {.long_name="debug",.short_name='d',
                    .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_TRACE] = {.long_name="trace",.short_name='t',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v',
                      .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_VERSION] = {.long_name="version",
                      .flags=GOPT_ARGUMENT_FORBIDDEN },
    [FLAG_QUIET] = {.long_name="quiet",.short_name='q',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_HELP] = {.long_name="help",.short_name='h',
                   .flags=GOPT_ARGUMENT_FORBIDDEN},
    [LAST] = {.flags = GOPT_LAST}
};

void _print_usage(void) {
    printf("Usage:\t$ bazel run @obazl//new [tlib] [args]\n\n");
    printf("tlib: label of template library. Default: @templates_ocaml\n\n");

    printf("Args:\n");
    printf("\t-l, --list\t\tList templates available in template library.\n");

    printf("\t-t, --template <name>\tName of template to use. Default: default\n");

    printf("\t-d, --debug\t\tEnable debug flags. Repeatable.\n");
    printf("\t-t, --trace\t\tEnable all trace flags (debug only).\n");
    printf("\t-q, --quiet\t\tSuppress all stdout msgs.\n");
    printf("\t-v, --verbose\t\tEnable verbosity. Repeatable.\n");
    printf("\t    --version\t\tPrint version identifier.\n");
}

void _set_options(struct option options[])
{
    if (options[FLAG_HELP].count) {
        _print_usage();
        exit(EXIT_SUCCESS);
    }

    if (options[FLAG_VERBOSE].count) {
        verbose = true;
        verbosity = options[FLAG_VERBOSE].count;
    }
    if (options[FLAG_QUIET].count) {
        quiet = true;
    }

    if (options[FLAG_DEBUG].count) {
        debug_new = options[FLAG_DEBUG].count;
    }
    if (options[FLAG_TRACE].count) {
        trace_new = true;
    }
}

bool prefix_matches(const char *pfx, const char *str)
{
    return strncmp(pfx, str, strlen(pfx)) == 0;
}

/* we expect this to always be run by bazel, as an external tool */
char *_templates[] = {
    /* dest: BUILD_WORKSPACE_DIRECTORY */
    "new/repo/templates/.bazelignore",
    "new/repo/templates/.bazeliskrc",
    "new/repo/templates/.bazelrc",
    "new/repo/templates/.bazelversion",
    "new/repo/templates/.config/.gitignore",
    "new/repo/templates/.config/miblrc",
    "new/repo/templates/.config/user.bazelrc",
    "new/repo/templates/WORKSPACE.bazel",
    "new/repo/templates/WORKSPACE.bzl",
    "new/repo/templates/WORKSPACE.opam.bzl",

    /* dest: XDG_DATA_HOME/obazl */
    /* "new/repo/templates/xdg/data/queries/obazlinfo.qry.bzl", */

    /* dest: XDG_BIN_HOME ($HOME/.local/bin) */
    /* "new/repo/templates/xdg/bin/obazlinfo", */

    "" /* <<== do not remove terminating null string */
};
char **template;
int pfx_len = 29; // strlen("external/obazl/new/templates") + 1

UT_string *abs_dest;
UT_string *abs_dir;
char *dest;
char *safe_dest;
char *path;

#if LOCAL_INTERFACE
struct subcat_s {
    char *name;
    struct subcat_s *next;
};
struct categories {
    char name[128];
    struct subcat_s *subcat;
    /* UT_array *subcats; */
    UT_hash_handle hh;
};
#endif
struct subcat_s *subcat_p, *subcat_tmp, *subcat_srch;
struct categories *cats, *tmp, *cat = NULL;
UT_string *seg0;
UT_string *seg1;


static int subcat_cmp(struct subcat_s *a, struct subcat_s *b)
{
    return strcmp(a->name, b->name);
}

void _list_templates(char *template)
{
    TRACE_ENTRY;
    LOG_DEBUG(0, "template file: %s", template);

    /* utarray_new(categories,&ut_str_icd); */

    char f[FILENAME_MAX];

    cwk_path_normalize(template, f, sizeof(f));
    LOG_DEBUG(0, "normalized: %s", f);

    struct cwk_segment segment;
    if(!cwk_path_get_first_segment(f, &segment)) {
        printf("Path doesn't have any segments.");
    }
    utstring_renew(seg0);
    utstring_printf(seg0, "%.*s", (int)segment.size, segment.begin);
    LOG_DEBUG(0, "Seg 0: %s", utstring_body(seg0));

    cwk_path_get_next_segment(&segment);
    utstring_renew(seg1);
    utstring_printf(seg1, "%.*s", (int)segment.size, segment.begin);
    LOG_DEBUG(0, "\tSeg 1: %s", utstring_body(seg1));

    HASH_FIND_STR(cats, utstring_body(seg0), cat);
    if (cat)  {
        LOG_DEBUG(0, "found cat %s", cat->name);
        subcat_tmp = NULL;
        subcat_srch = (struct subcat_s *)malloc(sizeof *subcat_srch);
        subcat_srch->name = strdup(utstring_body(seg1));
        LL_SEARCH(cat->subcat, subcat_tmp,
                  subcat_srch, subcat_cmp);
        if (subcat_tmp) {
            LOG_DEBUG(0, "  found subcat %s", subcat_tmp->name);
        } else {
            LOG_DEBUG(0, "  NOT found subcat %s; adding", utstring_body(seg1));
            subcat_tmp = (struct subcat_s *)malloc(sizeof *subcat_tmp);
            subcat_tmp->name = strdup(utstring_body(seg1));
            LL_PREPEND(cat->subcat, subcat_tmp);
        }
    } else {
        LOG_DEBUG(0, "NOT found cat %s; adding, with subcat %s",
                  utstring_body(seg0), utstring_body(seg1));
        tmp = (struct categories *)malloc(sizeof *tmp);
        strcpy(tmp->name, utstring_body(seg0));
        tmp->subcat = NULL;
        subcat_p = (struct subcat_s *)malloc(sizeof *subcat_p);
        subcat_p->name = strdup(utstring_body(seg1));
        LL_PREPEND(tmp->subcat, subcat_p);
        HASH_ADD_STR(cats, name, tmp);
    }

    TRACE_EXIT;
    return;
}

void _handle_file(char *template)
{
    TRACE_ENTRY;
    LOG_DEBUG(0, "template file: %s", template);

    char *d = "{\"name\": \"myproj\"}";

    /* if ext == ".mustache" or ".template": */
    cJSON *data = cJSON_Parse(d);
    /* if (data == 0) { */
    /*     log_error("cjson parse error: %s", d); */
    /*     TEST_FAIL_MESSAGE("cJSON_Parse error"); */
    /*     cJSON_free(data); */
    /* } else { */
    /*     const char *result = mustache_json_render(template, */
    /*                                               0, */
    /*                                               data, */
    /*                                               0); */
    /* } */
    cJSON_free(data);
    TRACE_EXIT;
    return;
}

/*
  Repo mapping: user may have passed e.g.
      --@obazl//template=@templates_foo//proj/tplg
  But we now way of knowing that. What we do know is
  that the default template is templates_ocaml/project/mwe,
  so the repo mapping contains just:
      templates_ocaml+,templates_ocaml,templates_ocaml+
      tools_obazl+,templates_ocaml,templates_ocaml+
      tools_obazl+,tools_obazl,tools_obazl+

   So we first look for templates_ocaml+, and if we
   find it, the template is by rule the 2nd grandchild,
   e.g. templates_ocaml+/project/mwe
   If we don't find it, the user must have passed some other
   templates repo, so we eliminate tools_obazl+ and
   use whatever is left.
 */
char *_get_templates_root(void)
{
    TRACE_ENTRY;
    // read repo_mapping
    char *repo_map = rf_repo_map();
    LOG_DEBUG(0, "repo_mapping: %s", repo_map);

    /* we do not know how the mappings will be ordered
       so we cannot just go top to bottom. We need an array. */
    UT_array *mappings;
    char **p;
    utarray_new(mappings,&ut_str_icd);

#   define BUFLEN 512
    char linebuf[BUFLEN];
    char *s = linebuf;
    FILE *file;

    file = fopen(repo_map, "r" );
    if (file == NULL) {
        perror("can't open: ");
        exit(EXIT_FAILURE);
    }
    while (fgets(linebuf, BUFLEN, file) != NULL) {
        if (strncmp(linebuf, "tools_obazl+", 12) == 0)
            continue;
        linebuf[strlen(linebuf) - 1] = '\0';
        utarray_push_back(mappings, &s);
    }
    fclose(file);

    /* there should be only one mapping other
       than the tools_obazl entries. */

    char *troot;
    p = NULL;
    while ( (p=(char**)utarray_next(mappings,p))) {
        LOG_DEBUG(0, "mapping: %s",*p);
        /* format:
           templates_ocaml+,templates_ocaml,templates_ocaml+ */
        troot = strnstr(*p, ",", 256);
        *troot = '\0';
        troot = strdup(*p);
    }
    utarray_free(mappings);
    LOG_DEBUG(0, "troot: %s", troot);
    TRACE_EXIT;
    return troot;
}

int main(int argc, char *argv[])
{
    argc = gopt(argv, options);
    (void)argc;

    gopt_errors (argv[0], options);

    _set_options(options);

    if (options[FLAG_VERSION].count) {
        fprintf(stdout, "@obazel//new version: %s\n", "x.y.z");
        exit(EXIT_SUCCESS);
    }

    rf_init(argv[0]);

    ws_dir = getenv("BUILD_WORKSPACE_DIRECTORY");
    build_wd = getenv("BUILD_WORKING_DIRECTORY");
    utstring_new(abs_dest);
    utstring_new(abs_dir);

    /* config_xdg_dirs(); */

    optind = 1;
    /* new_workspace(argv[0]); */

    char *rfroot = rf_root();
    LOG_DEBUG(0, "rf_root: %s", rfroot);
    char *templates_root = _get_templates_root();

    UT_string *fts_root;
    utstring_new(fts_root);
    utstring_printf(fts_root, "%s/%s", rfroot, templates_root);

    if (options[FLAG_LIST].count) {
        utstring_new(seg0);
        utstring_new(seg1);
        rf_fts(utstring_body(fts_root), _list_templates);
        utstring_free(seg0);
        utstring_free(seg1);
        fprintf(stdout, "rootdir %s\n", utstring_body(fts_root));
        FILE *docstr_file;
        UT_string *docstr;
        utstring_new(docstr);
        HASH_ITER(hh, cats, cat, tmp) {
            LL_FOREACH_SAFE(cat->subcat, subcat_p, subcat_tmp) {
                fprintf(stdout, "%s/%s\n", cat->name, subcat_p->name);
                utstring_renew(docstr);
                utstring_printf(docstr, "%s/%s/%s/DOCSTRING",
                                utstring_body(fts_root),
                                cat->name, subcat_p->name);
                int rc = access(utstring_body(docstr), R_OK);
                if (rc == 0) {
                    LOG_DEBUG(0, "found docstring: %s",
                              utstring_body(docstr));
#define BUFSZ 512
                    char buf[BUFSZ];
                    docstr_file = fopen(utstring_body(docstr), "r");
                    if (docstr_file == NULL) {
                        fprintf(stderr, "fopen failure %s",
                                utstring_body(docstr));
                    }
                    while (fgets(buf, BUFSZ, docstr_file)) {
                        fprintf(stdout, "\t%s\n", buf);
                    }
                    fclose(docstr_file);

                } else {
                    LOG_DEBUG(0, "NOT found docstring: %s",
                              utstring_body(docstr));
                }

                LL_DELETE(cat->subcat , subcat_p);
                free(subcat_p);
            }
            /* LL_FOREACH(cat->subcat, subcat_tmp) { */
            /* } */
            HASH_DEL(cats, cat);
            free(cat);
        }
        utstring_free(docstr);
    } else {
        rf_fts(utstring_body(fts_root), _handle_file);
    }

    utstring_free(fts_root);

    LOG_DEBUG(0, "workspace exit...", "");
    return 0;
}
