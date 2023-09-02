//FIXME: support -j (--jsoo-enable) flag

#include <ctype.h>
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
#include "libs7.h"
#include "log.h"
#include "findlibc.h"
#include "opamc.h"
#include "semver.h"
#include "utarray.h"
#include "uthash.h"
#include "utstring.h"
#include "xdgc.h"

#include "coswitch.h"

static UT_string *meta_path;

static char *switch_name;

#if defined(DEVBUILD)
bool coswitch_debug;
int  coswitch_debug_level;
bool debug_findlib;
bool coswitch_trace;
#endif

int level = 0;
int spfactor = 2;
char *sp = " ";

s7_scheme *s7;

UT_string *imports_path;
UT_string *pkg_parent;

struct paths_s {
    UT_string *registry;
    UT_string *coswitch_lib;
    struct obzl_meta_package *pkgs;
};

UT_string *coswitch_runfiles_root;

char *default_version = "0.0.0";
int   default_compat  = 0;

bool verbose;
int  verbosity;

int log_writes = 2; // threshhold for logging all writes
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
        switch_name = options[OPT_SWITCH].argument;
    } else {
        if (verbose)
            log_info("using current switch: %s", opam_switch_name());
        switch_name = opam_switch_name();
    }

    if (options[FLAG_DEBUG].count) {
#if defined(TRACING)
        coswitch_debug = true;
        coswitch_debug_level = options[FLAG_DEBUG].count;
#endif
    }

    if (options[FLAG_TRACE].count) {
#if defined(TRACING)
        coswitch_trace = true;
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
/* **************** **************** */
// coswitch_dst: either <switch>/lib
//     or XDG_DATA_HOME/obazl/opam/<switch>/lib
//     or <project>/_opam/lib
void pkg_handler(char *site_lib,
                 char *pkg_dir,
                 void *_paths)
{
    struct paths_s *paths = (struct paths_s*)_paths;
    UT_string *registry = (UT_string*)paths->registry;
    UT_string *coswitch_lib = (UT_string*)paths->coswitch_lib;
    struct obzl_meta_package *pkgs
        = (struct obzl_meta_package*)paths->pkgs;

    if (verbosity > 1) {
        log_debug("pkg_handler: %s", pkg_dir);
        log_debug("site-lib: %s", site_lib);
        log_debug("registry: %s", utstring_body(registry));
        log_debug("coswitch: %s", utstring_body(coswitch_lib));
        log_debug("pkgs ct: %d", HASH_COUNT(pkgs));
    }

    utstring_renew(meta_path);
    utstring_printf(meta_path, "%s/%s/META",
                    site_lib,
                    pkg_dir);
                    /* utstring_body(opam_switch_lib), pkg_name); */
    if (verbosity > 1)
        log_info("meta_path: %s", utstring_body(meta_path));

    errno = 0;
    if ( access(utstring_body(meta_path), F_OK) != 0 ) {
        // no META happens for e.g. <switch>/lib/stublibs
        /* log_warn("%s: %s", */
        /*          strerror(errno), utstring_body(meta_path)); */
        return;
    /* } else { */
    /*     /\* exists *\/ */
    /*     log_info("accessible: %s", utstring_body(meta_path)); */
    }

    errno = 0;
    // pkg must be freed...
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
        /* if (coswitch_debug_findlib) */
        /* DUMP_PKG(0, pkg); */
        /* #endif */
        // write module stuff in site-lib
        // write registry stuff
    }

    /* struct obzl_meta_packages_s *pkg */
    /*     = malloc(sizeof(obzl_meta_packages_s)); */
    /* char *n =  "TEST"; */
    /* pkg->name = n; */

    /* if (HASH_COUNT(pkgs) == 0) { */
    /*     log_debug("adding initial pkg: %s", pkg->name); */
    /*     pkgs = pkg; */
    /* } else { */
    /*     log_debug("adding next pkg", pkg->name); */
    /*     HASH_ADD_PTR(pkgs, name, pkg); */
    /* } */

    /* log_debug("pkg->name: %s", pkg->name); */
    /* log_debug("pkg->module_name: %s", pkg->module_name); */

    char *pkg_name = strdup(pkg->module_name);
    /* log_debug("pkg_name: %s", pkg_name); */

    /* char *p = pkg_name; */
    /* for (p = pkg_name; *p; ++p) *p = tolower(*p); */

    /* log_debug("adding next pkg: %s, (%p)", pkg->name, pkg); */
    /* log_debug("  keyptr: %p", pkg->name); */
    /* log_debug("  path: %s (%p)", pkg->path, pkg->path); */

    HASH_ADD_KEYPTR(hh, paths->pkgs,
                    pkg_name, strlen(pkg_name),
                    pkg);

    /* log_debug("HASH CT: %d", HASH_COUNT(paths->pkgs)); */
    /* struct obzl_meta_package *p; */
    /* HASH_FIND_STR(paths->pkgs, pkg->name, p); */
    /* if (p) */
    /*     log_debug("found: %s", p->name); */
    /* else */
    /*     log_debug("NOT found: %s", pkg->name); */

    /* log_debug("iterating"); */
    /* struct obzl_meta_package *s; */
    /* for (s = paths->pkgs; s != NULL; s = s->hh.next) { */
    /*     log_debug("\tpkg ptr %p", s); */
    /*     log_debug("\tpkg name %s", s->name); */
    /*     log_debug("\tpkg path %s (%p)", s->path, s->path); */
    /* } */


    /* log_debug("coswitch_lib: %s", utstring_body(coswitch_lib)); */

    /** emit workspace, module files for opam pkg **/
    // WORKSPACE.bazel
    UT_string *ws_root;
    utstring_new(ws_root);
    utstring_printf(ws_root, "%s/%s",
                    /* utstring_body(opam_switch_lib), */
                    /* site_lib, */
                    utstring_body(coswitch_lib),
                    pkg_name);
                    /* (char*)version); */
    mkdir_r(utstring_body(ws_root));

    UT_string *bazel_file;
    utstring_new(bazel_file);
    utstring_printf(bazel_file, "%s/WORKSPACE.bazel",
                    utstring_body(ws_root));
    emit_workspace_file(bazel_file, pkg_name);

    // MODULE.bazel emitted later, after all pkgs parsed

    // then emit the BUILD.bazel files for the opam pkg
    utstring_new(imports_path);
    utstring_printf(imports_path, "%s", pkg_name);
                    /* obzl_meta_package_name(pkg)); */

    utstring_new(pkg_parent);

    // for now:
    UT_string *switch_lib;
    utstring_new(switch_lib);
    utstring_printf(switch_lib, "%s", site_lib);

    emit_build_bazel(switch_lib, // site_lib,
                     utstring_body(coswitch_lib),
                     /* utstring_body(ws_root), */
                     0,         /* indent level */
                     pkg_name,
                     pkg_parent, /* needed for handling subpkgs */
                     NULL, // "buildfiles",        /* _pkg_prefix */
                     utstring_body(imports_path),
                     /* "",      /\* pkg-path *\/ */
                     pkg);
                     /* opam_pending_deps, */
                     /* opam_completed_deps); */

    // this will emit on BUILD.bazel file per pkg & subpkg
    // and put them in <switch>/lib/<repo>/lib/<subpkg> dirs
    // e.g. <switch>/lib/ppxlib/lib/ast for ppxlib.ast

    /* ******************************** */
    // finally, the registry record
    // emit registry files
    /* emit_registry_record(registry, */
    /*                      meta_path, */
    /*                      pkg_dir, */
    /*                      pkg, */
    /*                      default_version // (char*)version */
    /*                      ); */

    //FIXME: read dunfile is choking on dune-package files,
    // which all contain
    //(sections (lib .) (libexec .) (doc ../../doc/<pkg>))
    /* emit_pkg_bindir(site_lib, utstring_body(coswitch_lib), */
    /*                 pkg->name); */
}

UT_string *_config_bzlmod_registry(char *switch_name,
                                   char *coswitch_lib)
{
    char *s = xdg_data_home();
    if (verbose)
        log_info("xdg data home: %s", s);
    UT_string *obazl_registry_home;
    utstring_new(obazl_registry_home);
    utstring_printf(obazl_registry_home,
                    "%s/obazl/registry/%s",
                    s, switch_name);
    free(s);
    if (verbose)
        log_info("registry home: %s",
                 utstring_body(obazl_registry_home));
    mkdir_r(utstring_body(obazl_registry_home));

    // write: bazel_registry.json
    // also MODULE.bazel, WORKSPACE???

    /* char *module_base_path = opam_switch_lib(switch_name); */
    char *module_base_path = coswitch_lib;
    if (verbose)
        log_info("module_base_path: %s",
                 module_base_path);
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

    /* utstring_printf(obazl_registry_home, */
    /*                 "/%s", "modules"); */
    if (verbose)
        log_info("modules dir: %s",
                 utstring_body(obazl_registry_home));
    mkdir_r(utstring_body(obazl_registry_home));
    return obazl_registry_home;
}

/**
   Method:
   1. Iterate over switch, converting META to BUILD.bazel
      and collecting toplevel packages in pkgs utarray
   2. Iterate over pkgs array, emitting
      - MODULE.bazel to coswitch
      - registry record
 */
int main(int argc, char *argv[])
{
    argc = gopt(argv, options);
    (void)argc;

    gopt_errors (argv[0], options);

    _set_options(options);

    utstring_new(coswitch_runfiles_root);
    utstring_printf(coswitch_runfiles_root, "%s", getcwd(NULL, 0));

    /* s7_scheme *s7 = coswitch_s7_init(); */
    s7 = libs7_init();

    /* coswitch_s7_init2(NULL, // options[OPT_MAIN].argument, */
    /*              NULL); // options[OPT_WS].argument); */

    /* coswitch_configure(); */
    /* config_mibl.c reads miblrc, may call s7 */
    /* utstring_new(setter); */

    UT_array *opam_include_pkgs;
    utarray_new(opam_include_pkgs,&ut_str_icd);

    UT_array *opam_exclude_pkgs;
    utarray_new(opam_exclude_pkgs,&ut_str_icd);

    // globals in config_bazel.c:
    // bzl_mode
    // coswitch_runfiles_root, runtime_data_dir
    // obazl_ini_path
    // rootws, ews_root
    // traversal_root
    // build_ws_dir, build_wd, launch_dir
    // may call config_xdg_dirs()
    /* bazel_configure(NULL);      /\* run by coswitch_s7_init??? *\/ */

    /* if (options[FLAG_SHOW_CONFIG].count) { */
    /*     show_bazel_config(); */
    /*     show_coswitch_config(); */
    /*     show_s7_config(); */
    /*     exit(0); */
    /* } */

    /* chdir(rootws);            /\* always run from base ws root *\/ */
    /* char *ws = getenv("BUILD_WORKSPACE_DIRECTORY"); */
    /* if (ws) { */
    /*     /\* we launched from bazel workspace, cd to launch dir *\/ */
    /*     chdir(ws); */
    /* } */

    //FIXME: get site-lib from libopam
    if (verbosity > 0) {
        log_info("switch: %s", switch_name);
        log_info("opam root: %s", opam_root());
        log_info("opam switch lib: %s", opam_switch_lib(switch_name));
    }

    /* struct obzl_meta_package_s *pkgs = NULL; */
    /* struct obzl_meta_packages_s *pkg */
    /*     = malloc(sizeof(obzl_meta_packages_s)); */
    /* char *n =  "TEST"; */
    /* pkg->name = n; */
    /* HASH_ADD_INT(pkgs, name, pkg); */
    /* log_debug("pkgs ct: %d", HASH_COUNT(pkgs)); */
    /* log_debug("pkgs ptr: %p", pkgs); */

    if (verbose)
        log_info("switch prefix: %s", opam_switch_prefix(switch_name));

    char *findlib_site_lib = opam_switch_lib(switch_name);
    if (verbose)
        log_info("switch site-lib: %s", findlib_site_lib);

    UT_string *coswitch_lib;
    utstring_new(coswitch_lib);
    utstring_printf(coswitch_lib,
                    "%s/obazl/opam/%s/lib",
                    xdg_data_home(),
                    switch_name);
                    /* "%s/modules", */
                    /* utstring_body(registry)); */

    UT_string *registry = _config_bzlmod_registry(switch_name,
                                                  utstring_body(coswitch_lib));

    char *switch_pfx = opam_switch_prefix(switch_name);
    if (verbose)
        log_info("switch prefix: %s", switch_pfx);

    utstring_new(meta_path);

    /* struct obzl_meta_package *pkgs = NULL; */
    struct paths_s paths = {
        .registry = registry,
        .coswitch_lib = coswitch_lib
        /* .pkgs = pkgs */
    };

    //FIXME: add extras arg to pass extra info to pkg_handler
    findlib_map(opam_include_pkgs,
                opam_exclude_pkgs,
                findlib_site_lib,
                pkg_handler,
                (void*)&paths);

    /* log_debug("FINAL HASH CT: %d", HASH_COUNT(paths.pkgs)); */
    struct obzl_meta_package *pkg;
    char *pkg_name, *p;
    for (pkg = paths.pkgs; pkg != NULL; pkg = pkg->hh.next) {
        pkg_name = strdup(pkg->name);
        for (p = pkg_name; *p; ++p) *p = tolower(*p);
        if (verbosity > 0)
            log_debug(BLU " PKG %s" CRESET, pkg_name);
        semver_t *version = findlib_pkg_version(pkg);
        if (verbosity > 2) {
            log_debug("    version %d.%d.%d",
                      version->major, version->minor,
                      version->patch);
            log_debug("    compat: %d", version->major);
            log_debug("    path %s", pkg->path);
            log_debug("    path ptr %p", pkg->path);
            log_debug("    dir %s", pkg->directory);
            log_debug("    meta %s", pkg->metafile);
            log_debug("    entries ptr %p", pkg->entries);
        }
        /* ******************************** */
        emit_registry_record(registry, pkg, paths.pkgs);
                             /* s->metafile, */
                             /* s->directory, */
                             /* s, */
                             /* default_version // (char*)version */
                             /* ); */

        UT_string *mfile;
        utstring_new(mfile);
        utstring_printf(mfile, "%s/%s",
                        utstring_body(coswitch_lib),
                        /* version->major, version->minor, */
                        /* version->patch, */
                        pkg_name);
        free(version);
        mkdir_r(utstring_body(mfile));
        utstring_printf(mfile, "/MODULE.bazel");
                        /* utstring_body(mfile)); */
        emit_module_file(mfile, pkg, paths.pkgs);
        utstring_free(mfile);
        free(pkg_name);
    }

    /* FIXME: free opam_include_pkgs, opam_exclude_pkgs */

    emit_ocaml_workspace(registry,
                         paths.pkgs,
                         switch_name,
                         switch_pfx,
                         utstring_body(coswitch_lib));

    free(findlib_site_lib);
    utstring_free(meta_path);

#if defined(TRACING)
    log_debug("exiting new:coswitch");
#endif
    /* dump_nodes(result); */
}
