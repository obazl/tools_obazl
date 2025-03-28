#include <ctype.h>
#include <libgen.h>
#include <pwd.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "gopt.h"
#include "liblogc.h"
#include "librunfiles.h"

#include "cwalk.h"
#include "mustache_cjson.h"
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

/* json data from cmd line */
char *default_name = "myproj";
char *default_path = ".";
char *json_data;
cJSON *json_str;

enum OPTS {
    OPT_TEMPLATE = 0,
    OPT_DATA,                   /* json string? */
    OPT_JSON,
    OPT_NAME,
    OPT_PATH,
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
    [OPT_DATA] = {.long_name="data",
                 .flags=GOPT_ARGUMENT_REQUIRED},
    [OPT_JSON] = {.long_name="json", .short_name='j',
                 .flags=GOPT_ARGUMENT_REQUIRED},
    [OPT_NAME] = {.long_name="name", .short_name='n',
                  .flags=GOPT_ARGUMENT_REQUIRED},
    [OPT_PATH] = {.long_name="path", .short_name='p',
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
    printf("\nUsage:\t$ bazel run @obazl//new [--@obazl//tlib=@<tlib>] -- [opts]\n\n");
    printf("@<tlib>: label of template library repo. Must be listed as a 'bazel_dep' in MODULE.bazel. Default: @templates_ocaml\n\n");

    printf("Options:\n");
    printf("\t-l, --list\t\tList templates available in template library.\n");

    printf("\t-t, --template <name>\tName of template to use. Default: default\n");

    printf("\t-n, --name <name>\tName to use for pkg etc. Default: 'myproj'\n");

    printf("\t-d, --debug\t\tEnable debug flags. Repeatable.\n");
    printf("\t-t, --trace\t\tEnable all trace flags (debug only).\n");
    printf("\t-q, --quiet\t\tSuppress all stdout msgs.\n");
    printf("\t-v, --verbose\t\tEnable verbosity. Repeatable.\n");
    printf("\t    --version\t\tPrint version identifier.\n\n");

    printf("Examples:\n");
    printf("\t$ bazel run @obazl//new -- --list\n");
    printf("\t$ bazel run @obazl//new -- -t proj/hello -n howdy\n");
    printf("\n");
    printf("With 'bazel_dep(name=\"templates_dune\", version=\"0.1.0.dev\")' in MODULE.bazel:\n");
    printf("\t$ bazel run @obazl//new --@obazl//tlib=@templates_dune -- --list\n\n");
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

    if (options[OPT_NAME].count) {
        default_name = strdup(options[OPT_NAME].argument);
    }
    if (options[OPT_PATH].count) {
        default_path = strdup(options[OPT_PATH].argument);
    }
}

bool prefix_matches(const char *pfx, const char *str)
{
    return strncmp(pfx, str, strlen(pfx)) == 0;
}

/* we expect this to always be run by bazel, as an external tool */
/* char *_templates[] = { */
/*     /\* dest: BUILD_WORKSPACE_DIRECTORY *\/ */
/*     "new/repo/templates/.bazelignore", */
/*     "new/repo/templates/.bazeliskrc", */
/*     "new/repo/templates/.bazelrc", */
/*     "new/repo/templates/.bazelversion", */
/*     "new/repo/templates/.config/.gitignore", */
/*     "new/repo/templates/.config/miblrc", */
/*     "new/repo/templates/.config/user.bazelrc", */
/*     "new/repo/templates/WORKSPACE.bazel", */
/*     "new/repo/templates/WORKSPACE.bzl", */
/*     "new/repo/templates/WORKSPACE.opam.bzl", */

/*     /\* dest: XDG_DATA_HOME/obazl *\/ */
/*     /\* "new/repo/templates/xdg/data/queries/obazlinfo.qry.bzl", *\/ */

/*     /\* dest: XDG_BIN_HOME ($HOME/.local/bin) *\/ */
/*     /\* "new/repo/templates/xdg/bin/obazlinfo", *\/ */

/*     "" /\* <<== do not remove terminating null string *\/ */
/* }; */
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

void _template_lister(char *template)
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

void _list_templates(UT_string *fts_root, char *templates_root)
{
        utstring_new(seg0);
        utstring_new(seg1);
        rf_fts(utstring_body(fts_root), _template_lister);
        utstring_free(seg0);
        utstring_free(seg1);

        // drop the trailing '+' from the repo name:
        int len = strlen(templates_root);
        fprintf(stdout, "Templates available from library @%.*s\n",
                len-1, templates_root);

        FILE *docstr_file;
        UT_string *docstr;
        utstring_new(docstr);
        HASH_ITER(hh, cats, cat, tmp) {
            LL_FOREACH_SAFE(cat->subcat, subcat_p, subcat_tmp) {
                fprintf(stdout, "\t%s/%s\n", cat->name, subcat_p->name);
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
                        fprintf(stdout, "\t\t%s", buf);
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
}

UT_string *ofile;

void _handle_file(char *template)
{
    TRACE_ENTRY;
    static char buffer[FILENAME_MAX];
    memset(buffer, 0, sizeof(buffer));

    char tfile_src[FILENAME_MAX];
    memset(tfile_src, 0, sizeof(tfile_src));
    cwk_path_normalize(template, tfile_src, sizeof(tfile_src));
    LOG_DEBUG(0, "template:    %s", realpath(tfile_src, NULL));

    /* next, transform filename */
    int flags = Mustach_With_AllExtensions;
    /* if (options[FLAG_STRICT].count) { */
    /*     flags |= Mustach_With_ErrorUndefined; */
    /* } */
    errno = 0;
    const char *tfile = mustache_json_render(tfile_src, 0, json_str, flags);
    LOG_DEBUG(0, "transformed tfile: %s", tfile);

    if (strncmp(tfile, "DOCSTRING", 9) == 0) return;

    const char *extension;
    size_t length;
    if (cwk_path_has_extension(tfile)) {
        cwk_path_get_extension(tfile, &extension, &length);
        /* printf("The extension is: '%.*s'\n", (int)length, extension); */

        if (strncmp(extension, ".mustache", 9) == 0) {

            /* **** load template **** */
            size_t file_sz = 0;
            char *template_str = mustachios_readfile(tfile_src, &file_sz);
            LOG_DEBUG(0, "template file_sz: %d", file_sz);

            // remove extension
            cwk_path_change_extension(tfile, "", buffer, sizeof(buffer));
            buffer[strlen(buffer) - 1] = '\0';
            utstring_renew(ofile);
            utstring_printf(ofile, "%s/%s/%s",
                            ws_dir, default_name, strdup(buffer));
            LOG_DEBUG(0, "output file: %s", utstring_body(ofile));
            /* LOG_DEBUG(0, "template_str: %s", template_str); */
            /* LOG_DEBUG(0, "END TEMPLATE_STR", ""); */
            /* **** render **** */
            errno = 0;
            const char *result = mustache_json_render(template_str, 0, json_str, flags);
            free(template_str);


            cwk_path_get_dirname(utstring_body(ofile), &length);
            char *dname = strndup(utstring_body(ofile), length);
            LOG_DEBUG(0, "The dirname is: '%s'", dname);
            mkdir_r(dname);
            free(dname);

            /* LOG_DEBUG(0, "%s", result); */

            FILE *outstream = fopen(utstring_body(ofile), "w");
            if (outstream == NULL) {
                LOG_ERROR(0, "fopen failure %s", utstring_body(ofile));
            }

            /* **** write outfile **** */
            int buflen = strlen(result);
            int ct = fwrite(result, 1, buflen, outstream);
            if (ct != buflen) {
                log_error("failed fwrite");
            }
            fclose(outstream);
        } else {
            utstring_renew(ofile);
            utstring_printf(ofile, "%s/%s/%s",
                            ws_dir, default_name, tfile);
            LOG_DEBUG(0, "output file: %s", utstring_body(ofile));
            cwk_path_get_dirname(utstring_body(ofile), &length);
            char *dname = strndup(utstring_body(ofile), length);
            LOG_DEBUG(0, "The dirname is: '%s'", dname);
            mkdir_r(dname);
            copyfile((char*)tfile_src, utstring_body(ofile));
            free(dname);
        }
    } else {
        utstring_renew(ofile);
        utstring_printf(ofile, "%s/%s/%s",
                        ws_dir, default_name, tfile);
        LOG_DEBUG(0, "output file: %s", utstring_body(ofile));
        cwk_path_get_dirname(utstring_body(ofile), &length);
        char *dname = strndup(utstring_body(ofile), length);
        LOG_DEBUG(0, "The dirname is: '%s'", dname);
        mkdir_r(dname);
        copyfile((char*)tfile_src, utstring_body(ofile));
        free(dname);
    }
    TRACE_EXIT;
    return;
}

/*
  Repo mapping: user may have passed e.g.
      --@obazl//template=@templates_foo//proj/tplg
  But we have no way of knowing that. What we do know is
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

void _augment_data(void)
{
    const cJSON *nm = NULL;
    nm = cJSON_GetObjectItemCaseSensitive(json_str, "name");
    if (cJSON_IsString(nm)
        && (nm->valuestring != NULL)) {
        LOG_DEBUG(0, "name: '%s'", nm->valuestring);
        default_name = strdup(nm->valuestring);
    } else {
        // default name == myproj
        cJSON *_name = cJSON_CreateString(default_name);
        if (_name == NULL) {
            LOG_ERROR(0, "cJSON_CreateString fail: _name %s", _name);
            exit(EXIT_FAILURE);
        }
        cJSON_AddItemToObject(json_str, "name", _name);
    }
    /* add capitalized name */
    char *uc_name = strdup(default_name);
    uc_name[0] = toupper(uc_name[0]);
    cJSON *ucname = cJSON_CreateString(uc_name);
    if (ucname == NULL) {
        LOG_ERROR(0, "cJSON_CreateString fail: ucname %s", uc_name);
        exit(EXIT_FAILURE);
    }
    cJSON_AddItemToObject(json_str, "_Name", ucname);

    const char *homedir = getenv("HOME");
    if (homedir == NULL) {
        struct passwd *pw = getpwuid(getuid());
        homedir = pw->pw_dir;
    }
    cJSON *home = cJSON_CreateString(homedir);
    if (ucname == NULL) {
        LOG_ERROR(0, "cJSON_CreateString fail: home %s", homedir);
        exit(EXIT_FAILURE);
    }
    cJSON_AddItemToObject(json_str, "_HOME", home);

    const char *xdghome = getenv("XDG_DATA_HOME");
    if (xdghome == NULL) {
        xdghome = homedir;
    }
    cJSON *xdg = cJSON_CreateString(xdghome);
    if (ucname == NULL) {
        LOG_ERROR(0, "cJSON_CreateString fail: xdg %s", xdghome);
        exit(EXIT_FAILURE);
    }
    cJSON_AddItemToObject(json_str, "_XDG", xdg);
}

int main(int argc, char *argv[])
{
    int argc_raw = argc;
    argc = gopt(argv, options);
    (void)argc;

    gopt_errors(argv[0], options);

    _set_options(options);

    if (options[FLAG_VERSION].count) {
        fprintf(stdout, "@obazel//new version: %s\n", "x.y.z");
        exit(EXIT_SUCCESS);
    }

    rf_init(argv[0]);

    LOG_DEBUG(0, "cwd: %s", getcwd(NULL, 0));

    ws_dir = getenv("BUILD_WORKSPACE_DIRECTORY");
    LOG_DEBUG(0, "ws dir: %s", ws_dir);
    /* FIXME: outdir set by --path */

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

    if (argc_raw < 2) {
        _list_templates(fts_root, templates_root);
        exit(EXIT_SUCCESS);
    }

    utstring_new(ofile);

    if (options[OPT_JSON].count) {
        LOG_DEBUG(0, "JSON: %s", options[OPT_JSON].argument);
    }


    if (options[FLAG_LIST].count) {
        _list_templates(fts_root, templates_root);
    }
    else if (options[OPT_TEMPLATE].count) {
        utstring_printf(fts_root, "/%s", options[OPT_TEMPLATE].argument);
        LOG_DEBUG(0, "FTS root: %s", utstring_body(fts_root));

        /* first get data for mustachios */
        UT_string *jsondat;
        utstring_new(jsondat);
        /* char *json_str = mustachios_readfile(data_infile, &file_sz); */
        if (options[OPT_DATA].count) {
            utstring_printf(jsondat, "%s",
                            options[OPT_DATA].argument);
            /* log_debug("json file_sz: %d", file_sz); */
            /* utstring_printf(jsondat, "{\"name\": \"%s\"}", name); */
            json_str = cjsonx_read_string(utstring_body(jsondat));
            if (json_str == 0) {
                LOG_ERROR(0, "cjson parse error: %s", utstring_body(jsondat));
                exit(EXIT_FAILURE);
            }
        } else {
            // no options[OPT_DATA].count
            json_str = cJSON_CreateObject();
            cJSON *_name = cJSON_CreateString(default_name);
            if (_name == NULL) {
                LOG_ERROR(0, "cJSON_CreateString fail: _name %s", _name);
                exit(EXIT_FAILURE);
            }
            cJSON_AddItemToObject(json_str, "name", _name);
        }
        _augment_data();
        rf_fts(utstring_body(fts_root), _handle_file);
        /* utstring_free(jsondat); */
        /* cJSON_free(json_str); */
    }

    utstring_free(fts_root);
    utstring_free(ofile);

    LOG_DEBUG(0, "workspace exit...", "");
    return 0;
}
