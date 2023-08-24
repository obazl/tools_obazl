//FIXME: support -j (--jsoo-enable) flag

#include <errno.h>
#include <fcntl.h>
#include <libgen.h>

#if INTERFACE
#ifdef __linux__
#include <linux/limits.h>
#else
#include <limits.h>
#endif
#endif
#include <sys/stat.h>
#include <unistd.h>

#include "gopt.h"
#include "log.h"
#include "semver.h"
#include "utarray.h"
#include "utstring.h"

#include "libs7.h"
#include "findlibc.h"
#include "opamc.h"
#include "xdgc.h"

#include "coswitch.h"

static UT_string *meta_path;

static char *opam_switch;

#if defined(DEVBUILD)
extern bool mibl_debug;
extern int  debug_level;
extern bool debug_findlib;
extern bool mibl_trace;
#endif

bool verbose;
int  verbosity;
/* extern */ bool enable_jsoo;

char *pkg_path = NULL;

enum OPTS {
    OPT_PKG = 0,
    OPT_SWITCH,
    FLAG_JSOO,
    FLAG_CLEAN,
    FLAG_DEBUG,
    FLAG_SHOW_CONFIG,
    FLAG_TRACE,
    FLAG_VERBOSE,
    FLAG_QUIET,
    FLAG_HELP,
    LAST
};

void _print_usage(void) {
    printf("Usage:\t$ bazel run @obazl//convert [flags, options]\n");

    printf("Flags\n");
    printf("\t-j, --jsoo\t\t\tImport Js_of_ocaml resources.\n");
    printf("\t-c, --clean\t\t\tClean coswitch and reset to uninitialized state.\n");

    printf("\t-d, --debug\t\t\tEnable all debugging flags.\n");
    /* printf("\t-t, --trace\t\t\tEnable all trace flags.\n"); */
    printf("\t-v, --verbose\t\t\tEnable verbosity. Repeatable.\n");
}

static struct option options[] = {
    /* 0 */
    [OPT_PKG] = {.long_name="pkg",.short_name='p',
                 .flags=GOPT_ARGUMENT_REQUIRED},
    [OPT_SWITCH] = {.long_name="switch",.short_name='s',
                 .flags=GOPT_ARGUMENT_REQUIRED},
    [FLAG_JSOO] = {.long_name="jsoo", .short_name='j',
                   .flags=GOPT_ARGUMENT_REQUIRED},
    [FLAG_CLEAN] = {.long_name="clean",.short_name='c',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_DEBUG] = {.long_name="debug",.short_name='d',
                    .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_SHOW_CONFIG] = {.long_name="show-config",
                          .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_TRACE] = {.long_name="trace",.short_name='t',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_VERBOSE] = {.long_name="verbose",.short_name='v',
                      .flags=GOPT_ARGUMENT_FORBIDDEN | GOPT_REPEATABLE},
    [FLAG_QUIET] = {.long_name="quiet",.short_name='q',
                    .flags=GOPT_ARGUMENT_FORBIDDEN},
    [FLAG_HELP] = {.long_name="help",.short_name='h',
                   .flags=GOPT_ARGUMENT_FORBIDDEN},
    [LAST] = {.flags = GOPT_LAST}
};

void _set_options(struct option options[])
{
    if (options[FLAG_HELP].count) {
        _print_usage();
        exit(EXIT_SUCCESS);
    }

    if (options[FLAG_VERBOSE].count) {
        /* printf("verbose ct: %d\n", options[FLAG_VERBOSE].count); */
        verbose = true;
        verbosity = options[FLAG_VERBOSE].count;
    }

    if (options[OPT_SWITCH].count) {
        opam_switch = options[OPT_SWITCH].argument;
    } else {
        if (verbose)
            log_info("using current switch: %s", opam_switch_name());
        opam_switch = opam_switch_name();
    }

    if (options[FLAG_DEBUG].count) {
#if defined(TRACING)
        mibl_debug = true;
        mibl_debug_level = options[FLAG_DEBUG].count;
#endif
    }

    if (options[FLAG_TRACE].count) {
#if defined(TRACING)
        mibl_trace = true;
#endif
    }

    if (options[FLAG_JSOO].count) {
        enable_jsoo = true;
    }

    /* if (options[OPT_PKG].count) { */
    /*     utarray_push_back(opam_include_pkgs, &optarg); */
    /* } */

    /* case 'x': */
    /*     printf("EXCL %s\n", optarg); */
    /*     utarray_push_back(opam_exclude_pkgs, &optarg); */
    /*     break; */
}

/* FIXME: put this in @cc_config//utils, for sharing? */
void _mkdir_r(const char *dir) {
    char tmp[256];
    char *p = NULL;
    size_t len;

    snprintf(tmp, sizeof(tmp),"%s",dir);
    len = strlen(tmp);
    if (tmp[len - 1] == '/')
        tmp[len - 1] = 0;
    for (p = tmp + 1; *p; p++)
        if (*p == '/') {
            *p = 0;
            mkdir(tmp, S_IRWXU);
            *p = '/';
        }
    mkdir(tmp, S_IRWXU);
}

/* **************** **************** */
void pkg_handler(char *site_lib, char *pkg_dir,
                 void *_modules_dir)
{
    UT_string *modules_dir = (UT_string*)_modules_dir;
    if (verbosity > 1) {
        log_debug("pkg_handler: %s", pkg_dir);
        log_debug("site-lib: %s", site_lib);
        log_debug("modules: %s", utstring_body(modules_dir));
    }

    UT_string *tmp;
    utstring_new(tmp);
    utstring_printf(tmp, "%s/%s",
                    utstring_body(modules_dir),
                    pkg_dir);
    _mkdir_r(utstring_body(tmp));

    utstring_renew(meta_path);
    utstring_printf(meta_path, "%s/%s/META",
                    site_lib,
                    pkg_dir);
                    /* utstring_body(opam_switch_lib), pkg_name); */
    if (verbosity > 1)
        log_info("meta_path: %s", utstring_body(meta_path));

    errno = 0;
    if ( access(utstring_body(meta_path), F_OK) != 0 ) {
        log_warn("%s: %s",
                 strerror(errno), utstring_body(meta_path));
        return;
    } else {
        /* exists */
        log_info("accessible: %s", utstring_body(meta_path));
    }

    errno = 0;
    struct obzl_meta_package *pkg
        = obzl_meta_parse_file(utstring_body(meta_path));

    if (pkg == NULL) {
        if (errno == -1) {
/* #if defined(TRACING) */
            if (verbose)
                log_warn("Empty META file: %s", utstring_body(meta_path));
/* #endif */
            /* check dune-package for installed executables */
            /* chdir(old_cwd); */
            return;
        }
        else if (errno == -2) {
            log_warn("META file contains only whitespace: %s", utstring_body(meta_path));
            return;
        } else {
            log_error("Error parsing %s", utstring_body(meta_path));
            return;
        }
        /* emitted_bootstrapper = false; */
    } else {
        /* #if defined(DEVBUILD) */
        if (verbose)
            log_info("PARSED %s", utstring_body(meta_path));
        /* if (mibl_debug_findlib) */
        /* DUMP_PKG(0, pkg); */
        /* #endif */
        // write module stuff in site-lib
        // write registry stuff
    }

    obzl_meta_value version;
    char *pname = "version";
    struct obzl_meta_property *version_prop = obzl_meta_package_property(pkg, pname);
    if (version_prop) {
#if defined(DEVBUILD)
        dump_property(0, version_prop);
#endif
        version = obzl_meta_property_value(version_prop);
        log_debug("version: %s", (char*)version);

        if (!semver_is_valid(version)) {
            log_warn("BAD VERSION STRING: %s", (char*)version);
            /* return; */
        }
    } else {
        log_warn("No version property found");
        return; //FIXME ??? e.g. dune META has no version
    }

    /** emit workspace, module files **/
    // WORKSPACE.bazel
    UT_string *bazel_file;
    utstring_new(bazel_file);
    utstring_printf(bazel_file, "%s/%s/WORKSPACE.bazel",
                    /* utstring_body(opam_switch_lib), */
                    site_lib,
                    pkg->name);
    /* we know META file exists, so pkg path exists */
    emit_workspace_file(bazel_file, pkg->name);

    // MODULE.bazel
    utstring_renew(bazel_file);
    utstring_printf(bazel_file, "%s/%s/MODULE.bazel",
                    /* utstring_body(opam_switch_lib), */
                    site_lib,
                    pkg->name);
    emit_module_file(bazel_file, pkg);

    // emit registry files
    utstring_renew(bazel_file);
    utstring_printf(bazel_file,
                    "%s/%s",
                    utstring_body(modules_dir),
                    pkg->name);
    log_info("Emitting registry record to: %s",
             utstring_body(bazel_file));

    // modules/$MODULE/metadata.json
    UT_string *metadata_json_file;
    utstring_new(metadata_json_file);
    utstring_printf(metadata_json_file,
                    "%s/metadata.json",
                    utstring_body(bazel_file));
    if (verbose)
        log_info("metadata.json: %s",
                 utstring_body(metadata_json_file));

    //FIXME: from opam file: maintainer(s), homepage

    char *metadata_json_template = ""
        "{\n"
        "    \"homepage\": \"\",\n"
        "    \"maintainers\": [],\n"
        "    \"versions\": [\"%s\"],\n"
        "    \"yanked_versions\": {}\n"
        "}\n";
    // optional?  "repository": ["github:obazl/semverc"]

    UT_string *metadata_json;
    utstring_new(metadata_json);
    utstring_printf(metadata_json,
                    metadata_json_template,
                    (char*)version);
    if (verbose)
        log_info("metadata_json:\n%s",
                 utstring_body(metadata_json));

    FILE *metadata_json_fd
        = fopen(utstring_body(metadata_json_file), "w");
    fprintf(metadata_json_fd,
            "%s", utstring_body(metadata_json));
    fclose (metadata_json_fd);

    utstring_free(metadata_json_file);
    utstring_free(metadata_json);

    // modules/$MODULE/$VERSION/[MODULE.bazel, source.json]
    UT_string *reg_file;
    utstring_new(reg_file);
    utstring_printf(reg_file,
                    "%s/%s",
                    utstring_body(bazel_file),
                    (char*)version);
    mkdir_r(utstring_body(reg_file));

    utstring_printf(reg_file, "/%s", "MODULE.bazel");
    log_info("reg MODULE.bazel: %s",
             utstring_body(reg_file));

    emit_module_file(reg_file, pkg);

    utstring_renew(reg_file);
    utstring_printf(reg_file,
                    "%s/%s/source.json",
                    utstring_body(bazel_file),
                    (char*)version);
    log_info("reg source.json : %s",
             utstring_body(reg_file));

    char *source_json_template = ""
        "{\n"
        "    \"type\": \"local_path\",\n"
        "    \"path\": \"%s\"\n"
        "}\n";

    UT_string *source_json;
    utstring_new(source_json);
    utstring_printf(source_json,
                    source_json_template,
                    pkg->name);
    if (verbose)
        log_info("source_json:\n%s",
                 utstring_body(source_json));

    FILE *source_json_fd
        = fopen(utstring_body(reg_file), "w");
    fprintf(source_json_fd,
            "%s", utstring_body(source_json));
    fclose (source_json_fd);

    utstring_free(source_json);

    utstring_free(reg_file);
    utstring_free(bazel_file);
}

UT_string *_config_bzlmod_registry(char *opam_switch)
{
    char *s = xdg_data_home();
    if (verbose)
        log_info("xdg data home: %s", s);
    UT_string *obazl_registry_home;
    utstring_new(obazl_registry_home);
    utstring_printf(obazl_registry_home,
                    "%s/obazl/registry/%s",
                    s, opam_switch);
    free(s);
    if (verbose)
        log_info("registry: %s",
                 utstring_body(obazl_registry_home));
    _mkdir_r(utstring_body(obazl_registry_home));

    // write: bazel_registry.json
    // also MODULE.bazel, WORKSPACE???

    char *module_base_path = opam_switch_site_lib(opam_switch);
    if (verbose)
        log_info("module_base_path: %s", module_base_path);
    // alternative: mbp = XDG/share/obazl/opam/<switch>
    // or we could put coswitch stuff directly in the registry

    char *bazel_registry_template = ""
        "{\n"
        "    \"mirrors\": [],\n"
        "    \"module_base_path\": \"%s\"\n"
        "}\n";
    UT_string *bazel_registry_json;
    utstring_new(bazel_registry_json);
    utstring_printf(bazel_registry_json,
                    bazel_registry_template,
                    module_base_path);
    if (verbose)
        log_info("bazel_registry_json:\n%s",
                 utstring_body(bazel_registry_json));

    UT_string *obazl_registry_json_file;
    utstring_new(obazl_registry_json_file);
    utstring_printf(obazl_registry_json_file,
                    "%s/bazel_registry.json",
                    utstring_body(obazl_registry_home));
    if (verbose)
        log_info("bazel_registry.json:\n%s",
                 utstring_body(obazl_registry_json_file));
    FILE *bazel_registry_json_fd
        = fopen(utstring_body(obazl_registry_json_file), "w");
    fprintf(bazel_registry_json_fd,
            "%s", utstring_body(bazel_registry_json));
    fclose (bazel_registry_json_fd);

    utstring_printf(obazl_registry_home,
                    "/%s", "modules");
    if (verbose)
        log_info("modules dir: %s",
                 utstring_body(obazl_registry_home));
    _mkdir_r(utstring_body(obazl_registry_home));
    return obazl_registry_home;
}

int main(int argc, char *argv[])
{
    argc = gopt(argv, options);
    (void)argc;

    gopt_errors (argv[0], options);

    _set_options(options);

    /* utstring_new(mibl_runfiles_root); */
    /* utstring_printf(mibl_runfiles_root, "%s", getcwd(NULL, 0)); */

    /* s7_scheme *s7 = mibl_s7_init(); */
    /* s7_scheme *s7 = libs7_init(); */

    /* mibl_s7_init2(NULL, // options[OPT_MAIN].argument, */
    /*              NULL); // options[OPT_WS].argument); */

    /* mibl_configure(); */
    /* config_mibl.c reads miblrc, may call s7 */
    /* utstring_new(setter); */

    UT_array *opam_include_pkgs;
    utarray_new(opam_include_pkgs,&ut_str_icd);

    UT_array *opam_exclude_pkgs;
    utarray_new(opam_exclude_pkgs,&ut_str_icd);

    // globals in config_bazel.c:
    // bzl_mode
    // mibl_runfiles_root, runtime_data_dir
    // obazl_ini_path
    // rootws, ews_root
    // traversal_root
    // build_ws_dir, build_wd, launch_dir
    // may call config_xdg_dirs()
    /* bazel_configure(NULL);      /\* run by mibl_s7_init??? *\/ */

    /* if (options[FLAG_SHOW_CONFIG].count) { */
    /*     show_bazel_config(); */
    /*     show_mibl_config(); */
    /*     show_s7_config(); */
    /*     exit(0); */
    /* } */

    /* chdir(rootws);            /\* always run from base ws root *\/ */
    char *ws = getenv("BUILD_WORKSPACE_DIRECTORY");
    if (ws) {
        /* we launched from bazel workspace, cd to launch dir */
        chdir(ws);
    }

    //LEGACY: opam_configure() sets globals in mibl
    //TODO: use libopam
    // opam_configure must be run from root ws to account for local switches
    // sets global opam_switch_* vars
    /* if (opam_switch) */
    /*     opam_configure(opam_switch); */
    /* else */
    /*     opam_configure(""); */

    /* if (options[FLAG_CLEAN].count) { */
    /*     clean_coswitch(); */
    /*     if (options[FLAG_QUIET].count < 1) */
    /*         printf(GRN "INFO: " CRESET */
    /*                "Cleaned and reset coswitch." */
    /*                " To reinitialize run 'bazel run @obazl//coswitch'\n"); */
    /* } else { */
    /*     convert_findlib_pkgs(opam_include_pkgs, opam_exclude_pkgs); */
    /* } */

    //FIXME: get site-lib from libopam
    if (verbosity > 0) {
        log_info("switch: %s", opam_switch);
        log_info("opam root: %s", opam_root());
        log_info("opam switch lib: %s", opam_switch_site_lib(opam_switch));
    }

    UT_string *modules = _config_bzlmod_registry(opam_switch);

    /* char *_opam_root = opam_root(); */
    /* char *homedir = getenv("HOME"); */
    /* UT_string *findlib_site_lib; */
    /* utstring_new(findlib_site_lib); */
    /* utstring_printf(findlib_site_lib, */
    /*                 "%s/%s/lib", */
    /*                 _opam_root, opam_switch); */
    /*                 /\* "%s/.opam/%s/lib", *\/ */
    /*                 /\* homedir, *\/ */
    /*                 /\* opam_switch); *\/ */
    /* log_info("site-lib: %s", utstring_body(findlib_site_lib)); */

    if (verbose)
        log_info("switch prefix: %s", opam_switch_prefix(opam_switch));

    char *findlib_site_lib = opam_switch_site_lib(opam_switch);
    if (verbose)
        log_info("switch site-lib: %s", findlib_site_lib);

    utstring_new(meta_path);

    //FIXME: add extras arg to pass extra info to pkg_handler
    findlib_map(opam_include_pkgs,
                opam_exclude_pkgs,
                findlib_site_lib,
                pkg_handler,
                (void*)modules);

    /* FIXME: free opam_include_pkgs, opam_exclude_pkgs */
    free(findlib_site_lib);
    utstring_free(meta_path);

/* #if defined(TRACING) */
    log_debug("exiting new:coswitch");
/* #endif */
    /* dump_nodes(result); */
}
