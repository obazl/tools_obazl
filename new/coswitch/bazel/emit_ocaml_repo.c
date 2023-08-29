#include <errno.h>
#include <dirent.h>
#ifdef __linux__
#include <linux/limits.h>
#else
#include <limits.h>
#endif
/* #if EXPORT_INTERFACE */
#include <stdio.h>
/* #endif */
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "log.h"
#include "findlibc.h"
#include "opamc.h"
#include "utarray.h"
#include "utstring.h"

/* #include "cjson/cJSON.h" */
/* #include "mustach-cjson.h" */
/* #include "mustach.h" */

#include "emit_ocaml_repo.h"

extern int verbosity;

LOCAL const char *ws_name = "mibl";

/* extern UT_string *opam_switch_id; */
/* extern UT_string *opam_switch_prefix; */
extern UT_string *opam_ocaml_version;
/* extern UT_string *opam_switch_bin; */
/* extern UT_string *opam_switch_lib; */
extern UT_string *mibl_runfiles_root;

#if defined(TRACING)
bool mibl_debug_symlinks = false;
#endif

EXPORT void _mkdir_r(const char *dir) {
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

//FIXME: mv to emit_registry.c???
EXPORT void emit_registry_record(UT_string *registry,
                                 UT_string *meta_path,
                                 char *pkg_name,
                                 struct obzl_meta_package *pkg,
                                 char *version
                                 )
{
    log_debug(UBLU "emit_registry_record" CRESET);
    UT_string *tmp;
    utstring_new(tmp);
    utstring_printf(tmp, "%s/modules/%s",
                    utstring_body(registry),
                    pkg_name);
    _mkdir_r(utstring_body(tmp));

    UT_string *reg_dir;
    utstring_new(reg_dir);
    utstring_printf(reg_dir,
                    "%s/modules/%s",
                    utstring_body(registry),
                    pkg_name);
                    /* pkg->name); */
                    /* default_version); */
                    /* (char*)version); */
    mkdir_r(utstring_body(reg_dir));
    log_debug("regdir: %s", utstring_body(reg_dir));

    // modules/$MODULE/metadata.json
    /* UT_string *metadata_json_file; */
    UT_string *bazel_file;
    utstring_new(bazel_file);
    utstring_printf(bazel_file,
                    "%s/metadata.json",
                    utstring_body(reg_dir));
    /* if (verbose) */
        log_info("metadata.json: %s",
                 utstring_body(bazel_file));

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
                    version);
    /* if (verbose) */
        log_info("metadata_json:\n%s",
                 utstring_body(metadata_json));

    FILE *metadata_json_fd
        = fopen(utstring_body(bazel_file), "w");
    fprintf(metadata_json_fd,
            "%s", utstring_body(metadata_json));
    fclose (metadata_json_fd);

    utstring_free(metadata_json);

    // modules/$MODULE/$VERSION/[MODULE.bazel, source.json]
    utstring_printf(reg_dir, "/%s", default_version);
    mkdir_r(utstring_body(reg_dir));
    log_debug("regdir: %s", utstring_body(reg_dir));

    UT_string *reg_file;
    utstring_new(reg_file);
    utstring_printf(reg_file,
                    "%s/MODULE.bazel",
                    utstring_body(reg_dir));
    log_info("reg MODULE.bazel: %s",
             utstring_body(reg_file));

    if (strncmp("ocaml", pkg_name, 6) == 0) {
        // no META pkg
        FILE *ostream;
        ostream = fopen(utstring_body(reg_file), "w");
        if (ostream == NULL) {
            log_error("%s", strerror(errno));
            perror(utstring_body(reg_file));
            exit(EXIT_FAILURE);
        }

        fprintf(ostream, "## generated file - DO NOT EDIT\n\n");

        fprintf(ostream, "module(\n");
        fprintf(ostream, "    name = \"%s\", version = \"%s\",\n",
                pkg_name, version
                );
        fprintf(ostream, "    compatibility_level = \"0\",\n",
                default_compat);
        /* semversion.major); */
        fprintf(ostream, ")\n");
        fprintf(ostream, "\n");
        fclose(ostream);
    } else {
        emit_module_file(reg_file, pkg);
    }

    if (meta_path) {
        utstring_new(reg_file);
        utstring_printf(reg_file,
                        "%s/META",
                        utstring_body(reg_dir));
        log_info("reg META: %s",
             utstring_body(reg_file));

        _copy_buildfile(utstring_body(meta_path),
                        reg_file);
    }

    utstring_renew(reg_file);
    utstring_printf(reg_file,
                    "%s/source.json",
                    utstring_body(reg_dir));
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
                    pkg_name);
                    /* pkg->name); */
    if (verbose) {
        log_info("source_json:\n%s",
                 utstring_body(source_json));
        log_info("regfile: %s", utstring_body(reg_file));
    }
    FILE *source_json_fd
        = fopen(utstring_body(reg_file), "w");
    fprintf(source_json_fd,
            "%s", utstring_body(source_json));
    fclose (source_json_fd);

    utstring_free(source_json);

    utstring_free(reg_file);
    utstring_free(bazel_file);
}

void _emit_toplevel(UT_string *templates,
                    char *template,
                    UT_string *src_file,
                    UT_string *dst_dir,
                    UT_string *dst_file,
                    char *coswitch_lib,
                    char *pkg
                    )
{
    log_debug("_emit_toplevel");
    // create <switch>/lib/str for ocaml >= 5.0.0
    // previous versions already have it
    // step 1: write MODULE.bazel, WORKSPACE.bazel
    // step 2: write lib/str/lib/str/BUILD.bazel
    // step 3: write registry record
    char *content_template = ""
        "## generated file - DO NOT EDIT\n"
        "\n"
        "module(\n"
        "    name = \"%s\", version = \"0.0.0\",\n"
        "    compatibility_level = \"0\"\n"
        ")\n";
    UT_string *content;
    utstring_new(content);
    utstring_printf(content, content_template, pkg);

    utstring_renew(dst_dir);
    utstring_printf(dst_dir,
                    "%s/%s",
                    coswitch_lib,
                    pkg);
    mkdir_r(utstring_body(dst_dir));
    utstring_renew(dst_file);
    utstring_printf(dst_file, "%s/MODULE.bazel",
                    utstring_body(dst_dir));
    // write content to dst_file
    log_info("writing %s", utstring_body(dst_file));
    FILE *ostream = fopen(utstring_body(dst_file), "w");
    if (ostream == NULL) {
        perror(utstring_body(dst_file));
        log_error("fopen: %s: %s", strerror(errno),
                  utstring_body(dst_file));
        exit(EXIT_FAILURE);
    }
    fprintf(ostream, "%s", utstring_body(content));
    fclose(ostream);

    utstring_renew(dst_file);
    utstring_printf(dst_file, "%s/WORKSPACE.bazel",
                    utstring_body(dst_dir));
    // write content to ws file
    log_info("writing %s", utstring_body(dst_file));
    ostream = fopen(utstring_body(dst_file), "w");
    if (ostream == NULL) {
        perror(utstring_body(dst_file));
        log_error("fopen: %s: %s", strerror(errno),
                  utstring_body(dst_file));
        exit(EXIT_FAILURE);
    }
    fprintf(ostream, "## generated file - DO NOT EDIT\n");
    fclose(ostream);

    utstring_renew(dst_dir);
    utstring_printf(dst_dir,
                    "%s/%s/lib/%s",
                    coswitch_lib,
                    pkg, pkg);
    mkdir_r(utstring_body(dst_dir));
    utstring_renew(dst_file);
    utstring_printf(dst_file, "%s/BUILD.bazel",
                    utstring_body(dst_dir));

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    template);

    log_debug("cp src: %s, dst: %s",
              utstring_body(src_file), utstring_body(dst_file));
    _copy_buildfile(utstring_body(src_file), dst_file);
}

EXPORT void _copy_buildfile(char *src_file, UT_string *to_file)
{
    log_debug("_copy_buildfile src: %s, dst: %s",
              src_file, utstring_body(to_file));
    UT_string *src;
    utstring_new(src);

    int rc = access(src_file, F_OK);
    if (rc != 0) {
        perror(utstring_body(src));
        log_error("not found: %s", utstring_body(src));
        /* fprintf(stderr, "not found: %s\n", utstring_body(src)); */
        exit(EXIT_FAILURE);
        return;
    }

    /* if (mibl_debug) { */
    /*     log_debug("copying %s to %s\n", */
    /*               utstring_body(src), */
    /*               utstring_body(to_file)); */
    /* } */
    errno = 0;
    rc = copyfile(src_file,
                  utstring_body(to_file));
    if (rc != 0) {
        log_error("copyfile: %s", strerror(errno));
        fprintf(stderr, "ERROR copyfile: %s", strerror(errno));
        log_error("Exiting");
        fprintf(stderr, "Exiting\n");
        exit(EXIT_FAILURE);
    }
}

FILE *_open_buildfile(UT_string *ocaml_file)
{
    FILE *ostream = fopen(utstring_body(ocaml_file), "w");
    /* ostream =       fopen(utstring_body(ocaml_file), "w"); */
    if (ostream == NULL) {
        log_error("fopen: %s: %s", strerror(errno),
                  utstring_body(ocaml_file));
        fprintf(stderr, "fopen: %s: %s", strerror(errno),
                utstring_body(ocaml_file));
        fprintf(stderr, "exiting\n");
        /* perror(utstring_body(ocaml_file)); */
        exit(EXIT_FAILURE);
    }
    return ostream;
}

/* void emit_ocaml_stdlib_pkg(char *switch_name) */
/* { */
/*     if (mibl_debug) */
/*         log_debug("emit_ocaml_stdlib_pkg"); */

/*     UT_string *ocaml_file; */
/*     utstring_new(ocaml_file); */
/*     utstring_concat(ocaml_file, coswitch_pfx); */
/*     utstring_printf(ocaml_file, "/ocaml/lib/stdlib"); */
/*     mkdir_r(utstring_body(ocaml_file)); */

/*     _symlink_ocaml_stdlib(utstring_body(ocaml_file)); */

/*     utstring_printf(ocaml_file, "/BUILD.bazel"); */

/*     _copy_buildfile("ocaml_stdlib.BUILD", ocaml_file); */
/*     utstring_free(ocaml_file); */
/* } */

/* void _symlink_ocaml_stdlib(char *tgtdir) */
/* { */
/*     if (mibl_debug) */
/*         log_debug("_symlink_ocaml_stdlib to %s\n", tgtdir); */

/*     UT_string *opamdir; */
/*     utstring_new(opamdir); */
/*     utstring_printf(opamdir, "%s/ocaml", utstring_body(opam_switch_lib)); */

/*     UT_string *src; */
/*     utstring_new(src); */
/*     UT_string *dst; */
/*     utstring_new(dst); */
/*     int rc; */

/*     DIR *d = opendir(utstring_body(opamdir)); */
/*     if (d == NULL) { */
/*         fprintf(stderr, "Unable to opendir for symlinking stdlib: %s\n", */
/*                 utstring_body(opamdir)); */
/*         /\* exit(EXIT_FAILURE); *\/ */
/*         return; */
/*     } */

/*     struct dirent *direntry; */
/*     while ((direntry = readdir(d)) != NULL) { */
/*         if(direntry->d_type==DT_REG){ */
/*             if (strncmp("stdlib", direntry->d_name, 6) != 0) */
/*                 continue; */

/*             utstring_renew(src); */
/*             utstring_printf(src, "%s/%s", */
/*                             utstring_body(opamdir), direntry->d_name); */
/*             utstring_renew(dst); */
/*             utstring_printf(dst, "%s/%s", */
/*                             tgtdir, direntry->d_name); */
/*             /\* printf("symlinking %s to %s\n", *\/ */
/*             /\*        utstring_body(src), *\/ */
/*             /\*        utstring_body(dst)); *\/ */
/*             rc = symlink(utstring_body(src), */
/*                          utstring_body(dst)); */
/*             if (rc != 0) { */
/*                 if (errno != EEXIST) { */
/*                     perror(utstring_body(src)); */
/*                     fprintf(stderr, "exiting\n"); */
/*                     exit(EXIT_FAILURE); */
/*                 } */
/*             } */
/*         } */
/*     } */
/*     closedir(d); */
/* } */

void emit_ocaml_runtime_pkg(char *runfiles,
                            char *switch_lib,
                            char *coswitch_lib)  // dest
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_trace("emit_ocaml_runtime_pkg");
#endif

    UT_string *dst_dir;
    utstring_new(dst_dir);
    /* utstring_printf(dst_dir, coswitch_lib); // pfx); */
    utstring_printf(dst_dir, "%s/ocaml/runtime", coswitch_lib);
    mkdir_r(utstring_body(dst_dir));

    UT_string *templates;
    utstring_new(templates);
    utstring_printf(templates, "%s/%s",
                    runfiles, "/new/coswitch/templates");

    UT_string *src_file;
    utstring_new(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "ocaml_runtime.BUILD");

    UT_string *dst_file;
    utstring_new(dst_file);
    utstring_printf(dst_file,
                    "%s/BUILD.bazel",
                    utstring_body(dst_dir));

    _copy_buildfile(utstring_body(src_file), dst_file);

    /* _symlink_ocaml_runtime(utstring_body(dst_file)); */
    _symlink_ocaml_runtime(switch_lib, utstring_body(dst_dir));

    utstring_free(src_file);
    utstring_free(dst_dir);
    utstring_free(templates);
}

void _symlink_ocaml_runtime(char *switch_lib, char *tgtdir)
{
#if defined(TRACING)
    /* if (mibl_debug_symlinks) */
        log_trace("_symlink_ocaml_runtime src: %s, dst %s",
                  switch_lib, tgtdir);
#endif
    UT_string *opamdir;
    utstring_new(opamdir);
    utstring_printf(opamdir, "%s/ocaml",
                    switch_lib);
                    /* utstring_body(opam_switch_lib)); */
    log_info("OPAMDIR: %s", utstring_body(opamdir));

    UT_string *src;
    utstring_new(src);
    UT_string *dst;
    utstring_new(dst);
    int rc;

    DIR *d = opendir(utstring_body(opamdir));
    if (d == NULL) {
        perror(utstring_body(opamdir));
        log_error(RED "Unable to opendir for symlinking stdlib" CRESET);
        exit(EXIT_FAILURE);
        return;
    }

    struct dirent *direntry;
    while ((direntry = readdir(d)) != NULL) {
        /* log_debug("DIRENT"); */
        if(direntry->d_type==DT_REG){
            if (strncmp("stdlib", direntry->d_name, 6) != 0)
                if (strncmp("std_exit", direntry->d_name, 8) != 0)
                    continue;

            utstring_renew(src);
            utstring_printf(src, "%s/%s",
                            utstring_body(opamdir), direntry->d_name);
            utstring_renew(dst);
            utstring_printf(dst, "%s/%s",
                            tgtdir,
                            direntry->d_name);
            /* log_debug("symlinking %s to %s\n", */
            /*           utstring_body(src), */
            /*           utstring_body(dst)); */
            rc = symlink(utstring_body(src),
                         utstring_body(dst));
            symlink_ct++;
            if (rc != 0) {
                if (errno != EEXIST) {
                    perror(utstring_body(src));
                    fprintf(stderr, "exiting\n");
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    closedir(d);
}

/* ************************************** */
void emit_ocaml_stublibs(char *switch_pfx,
                         char *coswitch_lib)
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_trace("emit_ocaml_stublibs: %s/ocaml/stublibs", coswitch_lib);
#endif

    UT_string *dst_file;
    utstring_new(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/stublibs",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");

    FILE *ostream = fopen(utstring_body(dst_file), "w");
    ostream = fopen(utstring_body(dst_file), "w");
    if (ostream == NULL) {
        perror(utstring_body(dst_file));
        log_error("fopen: %s: %s", strerror(errno),
                  utstring_body(dst_file));
        exit(EXIT_FAILURE);
    }
    fprintf(ostream, "# generated file - DO NOT EDIT\n");
    /* fprintf(ostream, "exports_files(glob([\"**\"]))\n"); */
    fprintf(ostream, "filegroup(\n");
    fprintf(ostream, "    name = \"stublibs\",\n");
    fprintf(ostream, "    srcs = glob([\"**\"]),\n");
    fprintf(ostream, ")\n");
    fclose(ostream);

    utstring_free(dst_file);

    _emit_ocaml_stublibs_symlinks(switch_pfx,
                                  coswitch_lib,
                                  "ocaml/stublibs");
}

void _emit_ocaml_stublibs_symlinks(char *switch_pfx,
                                   char *coswitch_lib,
                                   char *tgtdir)
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_trace("_emit_ocaml_stublibs_symlinks: %s", tgtdir);
#endif
     // _dir ==? "/ocaml/stublibs");

    UT_string *src_dir;
    utstring_new(src_dir);
    utstring_printf(src_dir,
                    "%s/lib/%s",
                    switch_pfx,
                    tgtdir);

    UT_string *dst_dir;
    utstring_new(dst_dir);
    utstring_printf(dst_dir,
                    "%s/%s",
                    coswitch_lib,
                    tgtdir);
    mkdir_r(utstring_body(dst_dir));

/* #if defined(TRACING) */
/*     log_debug("src_dir: %s\n", utstring_body(src_dir)); */
/*     log_debug("dst_dir: %s\n", utstring_body(dst_dir)); */
/* #endif */

    UT_string *src_file;
    utstring_new(src_file);
    UT_string *dst_file;
    utstring_new(dst_file);
    int rc;

/* #if defined(TRACING) */
/*     log_debug("opening src_dir for read: %s\n", */
/*               utstring_body(src_dir)); */
/* #endif */
    DIR *srcd = opendir(utstring_body(src_dir));
    /* DIR *srcd = opendir(utstring_body(opamdir)); */
    if (srcd == NULL) {
        fprintf(stderr, "Unable to opendir for symlinking stublibs: %s\n",
                utstring_body(src_dir));
        /* exit(EXIT_FAILURE); */
        /* this can happen if a related pkg is not installed */
        /* example, see topkg and topkg-care */
        return;
    }

    struct dirent *direntry;
    while ((direntry = readdir(srcd)) != NULL) {
        //Condition to check regular file.
#if defined(TRACING)
        if (mibl_debug)
            log_debug("stublib: %s, type %d",
                      direntry->d_name, direntry->d_type);
#endif
        if( (direntry->d_type==DT_REG)
            || (direntry->d_type==DT_LNK) ){

            /* do not symlink workspace file */
            if (strncmp(direntry->d_name, "WORKSPACE", 9) == 0) {
                continue;
            }
            utstring_renew(src_file);
            utstring_printf(src_file, "%s/%s",
                            utstring_body(src_dir),
                            direntry->d_name);
            utstring_renew(dst_file);
            utstring_printf(dst_file, "%s/%s",
                            utstring_body(dst_dir),
                            direntry->d_name);

#if defined(TRACING)
            if (mibl_debug) {
                log_debug("stublibs: symlinking %s to %s\n",
                          utstring_body(src_file),
                          utstring_body(dst_file));
            }
#endif

            rc = symlink(utstring_body(src_file),
                         utstring_body(dst_file));
            symlink_ct++;
            if (rc != 0) {
                switch (errno) {
                case EEXIST:
                    goto ignore;
                case ENOENT:
                    log_error("symlink ENOENT: %s", strerror(errno));
                    log_error("a component of '%s' does not name an existing file",  utstring_body(dst_dir));
                    fprintf(stderr, "symlink ENOENT: %s\n", strerror(errno));
                    fprintf(stderr, "A component of '%s' does not name an existing file.\n",  utstring_body(dst_dir));
                    break;
                default:
                    log_error("symlink err: %s", strerror(errno));
                    fprintf(stderr, "symlink err: %s", strerror(errno));
                }
                log_error("Exiting");
                fprintf(stderr, "Error, exiting\n");
                exit(EXIT_FAILURE);
            ignore:
                ;
            }
        }
    }
    closedir(srcd);
}

/* **************************************************************** */
void emit_lib_stublibs(char *switch_stublibs, char *coswitch_lib)
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_trace("emit_lib_stublibs");
#endif
    UT_string *dst_file;
    utstring_new(dst_file);
    /* utstring_concat(dst_file, coswitch_pfx); */
    utstring_printf(dst_file, "%s/stublibs", coswitch_lib);
    mkdir_r(utstring_body(dst_file));

    utstring_printf(dst_file, "/WORKSPACE.bazel");

    FILE *ostream = fopen(utstring_body(dst_file), "w");
    ostream = fopen(utstring_body(dst_file), "w");
    if (ostream == NULL) {
        log_error("fopen: %s: %s", strerror(errno),
                  utstring_body(dst_file));
        fprintf(stderr, "fopen: %s: %s", strerror(errno),
                utstring_body(dst_file));
        fprintf(stderr, "exiting\n");
        /* perror(utstring_body(dst_file)); */
        exit(EXIT_FAILURE);
    }
    fprintf(ostream, "workspace( name = \"stublibs\" )"
            "    # generated file - DO NOT EDIT\n");
    fclose(ostream);

    /* now BUILD.bazel */
    utstring_renew(dst_file);
    /* utstring_concat(dst_file, coswitch_pfx); */
    utstring_printf(dst_file, "%s/stublibs/lib/stublibs",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");

    ostream = fopen(utstring_body(dst_file), "w");
    if (ostream == NULL) {
        log_error("fopen: %s: %s", strerror(errno),
                  utstring_body(dst_file));
        fprintf(stderr, "fopen: %s: %s", strerror(errno),
                utstring_body(dst_file));
        fprintf(stderr, "exiting\n");
        /* perror(utstring_body(dst_file)); */
        exit(EXIT_FAILURE);
    }
    fprintf(ostream, "# generated file - DO NOT EDIT\n\n");
    /* fprintf(ostream, "exports_files(glob([\"**\"]))\n"); */
    fprintf(ostream, "filegroup(\n");
    fprintf(ostream, "    name = \"stublibs\",\n");
    fprintf(ostream, "    srcs = glob([\"**\"]),\n");
    fprintf(ostream, "    visibility = [\"//visibility:public\"]\n");
    fprintf(ostream, ")\n");
    fclose(ostream);
    utstring_free(dst_file);
    /* **************************************************************** */
    _emit_lib_stublibs_symlinks(switch_stublibs, coswitch_lib);
}

void _emit_lib_stublibs_symlinks(char *switch_stublibs,
                                 char *coswitch_lib)
{
#if defined(TRACING)
    /* if (mibl_debug_symlinks) */
        log_trace("_emit_lib_stublibs_symlinks: %s", coswitch_lib);
#endif

    UT_string *dst_dir;
    utstring_new(dst_dir);
    /* utstring_printf(dst_dir, coswitch_pfx); */
    utstring_printf(dst_dir, "%s/stublibs/lib/stublibs",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_dir));

    UT_string *src_dir; // relative to opam_switch_lib
    utstring_new(src_dir);
    utstring_printf(src_dir, "%s", switch_stublibs);
                    /* "%s%s", */
                    /* utstring_body(opam_switch_lib), */
                    /* "/stublibs"); // _dir); */

    /* log_debug("src_dir: %s\n", utstring_body(src_dir)); */
    /* log_debug("dst_dir: %s\n", utstring_body(dst_dir)); */

    UT_string *src;
    utstring_new(src);
    UT_string *dst;
    utstring_new(dst);
    int rc;

/* #if defined(TRACING) */
/*     log_debug("opening src_dir for read: %s", */
/*               utstring_body(src_dir)); */
/* #endif */
    DIR *srcd = opendir(utstring_body(src_dir));
    /* DIR *srcd = opendir(utstring_body(opamdir)); */
    if (srcd == NULL) {
        fprintf(stderr, "Unable to opendir for symlinking stublibs: %s\n",
                utstring_body(src_dir));
        /* exit(EXIT_FAILURE); */
        /* this can happen if a related pkg is not installed */
        /* example, see topkg and topkg-care */
        return;
    }

    struct dirent *direntry;
    while ((direntry = readdir(srcd)) != NULL) {
        //Condition to check regular file.
#if defined(TRACING)
        if (mibl_debug_symlinks) {
            log_debug("stublib: %s, type %d",
                      direntry->d_name, direntry->d_type);
        }
#endif
        if (strncmp(direntry->d_name, "WORKSPACE", 9) == 0) {
            continue;
        }

        if( (direntry->d_type==DT_REG)
            || (direntry->d_type==DT_LNK) ){

            utstring_renew(src);
            utstring_printf(src, "%s/%s",
                            utstring_body(src_dir),
                            direntry->d_name);
            utstring_renew(dst);
            utstring_printf(dst, "%s/%s",
                            utstring_body(dst_dir), direntry->d_name);

#if defined(TRACING)
            if (mibl_debug_symlinks) {
                log_debug("stublibs: symlinking %s to %s",
                          utstring_body(src),
                          utstring_body(dst));
            }
#endif

            rc = symlink(utstring_body(src),
                         utstring_body(dst));
            symlink_ct++;
            if (rc != 0) {
                switch (errno) {
                case EEXIST:
                    goto ignore;
                case ENOENT:
                    log_error("symlink ENOENT: %s", strerror(errno));
                    log_error("a component of '%s' does not name an existing file",  utstring_body(dst_dir));
                    fprintf(stderr, "symlink ENOENT: %s\n", strerror(errno));
                    fprintf(stderr, "A component of '%s' does not name an existing file.\n",  utstring_body(dst_dir));
                    break;
                default:
                    log_error("symlink err: %s", strerror(errno));
                    fprintf(stderr, "symlink err: %s", strerror(errno));
                }
                log_error("Exiting");
                fprintf(stderr, "Exiting\n");
                exit(EXIT_FAILURE);
            ignore:
                ;
            }
        }
    }
    closedir(srcd);
}

/* *********************************************** */
void emit_ocaml_platform_buildfiles(char *runfiles,
                                    char *coswitch_lib)
{
#if defined(TRACING)
    /* if (mibl_trace) */
    log_trace("emit_ocaml_platform_buildfiles: %s",
              coswitch_lib);
#endif
    UT_string *templates;
    utstring_new(templates);
    utstring_printf(templates, "%s/%s",
                    runfiles, "/new/coswitch/templates");

    UT_string *src_file;
    utstring_new(src_file);
    UT_string *dst_file;
    utstring_new(dst_file);

    /* log_debug("CWD: %s", getcwd(NULL,0)); */

    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "platforms/BUILD.bazel");
    utstring_printf(dst_file,
                    "%s/ocaml/platforms",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "platforms/build/BUILD.bazel");

    utstring_renew(dst_file);
    utstring_printf(dst_file, "%s/ocaml/platforms/build", coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "platforms/target/BUILD.bazel");

    utstring_renew(dst_file);
    utstring_printf(dst_file, "%s/ocaml/platforms/target", coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_free(templates);
    utstring_free(src_file);
    utstring_free(dst_file);
}

void emit_ocaml_toolchain_buildfiles(char *runfiles,
                                     char *coswitch_lib)
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_trace("emit_ocaml_toolchain_buildfiles: %s", coswitch_lib);
#endif
    UT_string *templates;
    utstring_new(templates);
    utstring_printf(templates, "%s/%s",
                    runfiles, "/new/coswitch/templates");

    UT_string *src_file;
    utstring_new(src_file);
    UT_string *dst_file;
    utstring_new(dst_file);

    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "toolchain/selectors/local.BUILD");
    utstring_printf(dst_file,
                    "%s/ocaml/toolchain/selectors/local",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "toolchain/selectors/macos/x86_64.BUILD");
    utstring_renew(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/toolchain/selectors/macos/x86_64",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "toolchain/selectors/macos/arm.BUILD");
    utstring_new(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/toolchain/selectors/macos/arm",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "toolchain/selectors/linux/x86_64.BUILD");
    utstring_new(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/toolchain/selectors/linux/x86_64",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "toolchain/selectors/linux/arm.BUILD");
    utstring_new(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/toolchain/selectors/linux/arm",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    /* toolchain options */
    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "toolchain/profiles/profiles.BUILD");
    utstring_new(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/toolchain/profiles",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    /* toolchain adapters */
    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "toolchain/adapters/local.BUILD");
    utstring_new(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/toolchain/adapters/local",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);
    //TODO: mustache support
    /* _process_mustache("toolchain/adapters/local.BUILD.mustache", dst_file); */

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "toolchain/adapters/linux/x86_64.BUILD");
    utstring_new(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/toolchain/adapters/linux/x86_64",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "toolchain/adapters/linux/arm.BUILD");
    utstring_new(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/toolchain/adapters/linux/arm",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "toolchain/adapters/macos/x86_64.BUILD");
    utstring_new(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/toolchain/adapters/macos/x86_64",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "toolchain/adapters/macos/arm.BUILD");
    utstring_new(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/toolchain/adapters/macos/arm",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_free(templates);
    utstring_free(src_file);
    utstring_free(dst_file);
}

void emit_ocaml_bin_dir(char *switch_pfx, char *coswitch_lib)
{
#if defined(TRACING)
    /* if (mibl_trace) */
    log_trace("emit_ocaml_bin_dir: %s", coswitch_lib);
#endif
    UT_string *dst_file;
    utstring_new(dst_file);
    utstring_printf(dst_file, "%s/ocaml/bin", coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    log_trace("dst_file: %s", utstring_body(dst_file));

    FILE *ostream;
    ostream = fopen(utstring_body(dst_file), "w");
    if (ostream == NULL) {
        log_error("fopen: %s: %s", strerror(errno),
                  utstring_body(dst_file));
        fprintf(stderr, "fopen: %s: %s", strerror(errno),
                utstring_body(dst_file));
        fprintf(stderr, "exiting\n");
        /* perror(utstring_body(dst_file)); */
        exit(EXIT_FAILURE);
    }
    fprintf(ostream, "# generated file - DO NOT EDIT\n");
    fprintf(ostream, "exports_files(glob([\"**\"]))\n");
    fclose(ostream);
    utstring_free(dst_file);

    /* **************************************************************** */
    _emit_ocaml_bin_symlinks(switch_pfx, coswitch_lib);
}

void _emit_ocaml_bin_symlinks(char *opam_switch_pfx,
                              char *coswitch_lib // dest
                              )
{
#if defined(TRACING)
    /* if (mibl_debug_symlinks) */
    log_debug("_emit_ocaml_bin_symlinks");
    log_debug("lib: %s", coswitch_lib);
#endif

    UT_string *dst_dir;
    utstring_new(dst_dir);
    /* utstring_concat(dst_dir, coswitch_lib); // pfx); */
    utstring_printf(dst_dir, "%s/ocaml/bin", coswitch_lib);

    /* UT_string *src_dir; // relative to opam_switch_lib */
    /* utstring_new(src_dir); */
    /* utstring_printf(src_dir, "bin"); */

    mkdir_r(utstring_body(dst_dir));

#if defined(TRACING)
    /* if (mibl_debug) { */
    log_debug("opam pfx: %s", opam_switch_pfx);
        log_debug("dst_dir: %s", utstring_body(dst_dir));
    /* } */
#endif

    /* UT_string *opamdir; */
    /* utstring_new(opamdir); */
    /* utstring_printf(opamdir, "%s/bin", */
    /*                 utstring_body(opam_switch_pfx) */
    /*                 /\* utstring_body(src_dir) *\/ */
    /*                 ); */

    UT_string *src;
    utstring_new(src);
    UT_string *dst;
    utstring_new(dst);
    int rc;

    UT_string *opam_switch_bin;
    utstring_new(opam_switch_bin);
    utstring_printf(opam_switch_bin, "%s/bin", opam_switch_pfx);
#if defined(TRACING)
    /* if (mibl_debug) */
        log_debug("opening src_dir for read: %s",
                  utstring_body(opam_switch_bin));
#endif
    DIR *srcd = opendir(utstring_body(opam_switch_bin));
    /* DIR *srcd = opendir(utstring_body(opamdir)); */
    if (dst == NULL) {
        fprintf(stderr, "Unable to opendir for symlinking toolchain: %s\n",
                utstring_body(opam_switch_bin));
        /* exit(EXIT_FAILURE); */
        /* this can happen if a related pkg is not installed */
        /* example, see topkg and topkg-care */
        return;
    }

    struct dirent *direntry;
    while ((direntry = readdir(srcd)) != NULL) {
        //Condition to check regular file.
        if( (direntry->d_type==DT_REG)
            || (direntry->d_type==DT_LNK) ){

            utstring_renew(src);
            utstring_printf(src, "%s/%s",
                            utstring_body(opam_switch_bin),
                            direntry->d_name);
            utstring_renew(dst);
            utstring_printf(dst, "%s/%s",
                            utstring_body(dst_dir), direntry->d_name);

            /* if (debug) */
            /*     log_debug("symlinking %s to %s", */
            /*               utstring_body(src), */
            /*               utstring_body(dst)); */

            rc = symlink(utstring_body(src),
                         utstring_body(dst));
            symlink_ct++;
            if (rc != 0) {
                switch (errno) {
                case EEXIST:
                    goto ignore;
                case ENOENT:
                    log_error("symlink ENOENT: %s", strerror(errno));
                    log_error("a component of '%s' does not name an existing file",  utstring_body(dst_dir));
                    fprintf(stderr, "symlink ENOENT: %s\n", strerror(errno));
                    fprintf(stderr, "A component of '%s' does not name an existing file.\n",  utstring_body(dst_dir));
                    break;
                default:
                    log_error("symlink err: %s", strerror(errno));
                    fprintf(stderr, "symlink err: %s", strerror(errno));
                }
                log_error("Exiting");
                fprintf(stderr, "Exiting\n");
                exit(EXIT_FAILURE);
            ignore:
                ;
            }
        }
    }
    closedir(srcd);
    utstring_free(opam_switch_bin);
    log_debug("symlinking finished");
}

/* **************************************************************** */
/* obsolete but keep it around in case we decide to use it later */
void _symlink_buildfile(char *buildfile, UT_string *to_file)
{
#if defined(TRACING)
    if (mibl_debug_symlinks) log_trace("_symlink_buildfile");
#endif
    UT_string *src;
    utstring_new(src);
    utstring_printf(src,
                    "%s/external/%s/coswitch/templates/%s",
                    utstring_body(mibl_runfiles_root),
                    ws_name,
                    buildfile);
    int rc = access(utstring_body(src), F_OK);
    if (rc != 0) {
        log_error("not found: %s", utstring_body(src));
        fprintf(stderr, "not found: %s\n", utstring_body(src));
        return;
    }

#if defined(TRACING)
    if (mibl_debug) {
        log_debug("c_libs: symlinking %s to %s\n",
                  utstring_body(src),
                  utstring_body(to_file));
    }
#endif
    errno = 0;
    rc = symlink(utstring_body(src),
                 utstring_body(to_file));
    symlink_ct++;
    if (rc != 0) {
        switch (errno) {
        case EEXIST:
            goto ignore;
        case ENOENT:
            log_error("symlink ENOENT: %s", strerror(errno));
            /* log_error("a component of '%s' does not name an existing file",  utstring_body(dst_dir)); */
            fprintf(stderr, "symlink ENOENT: %s\n", strerror(errno));
            /* fprintf(stderr, "A component of '%s' does not name an existing file.\n",  utstring_body(dst_dir)); */
            break;
        default:
            log_error("symlink err: %s", strerror(errno));
            fprintf(stderr, "symlink err: %s", strerror(errno));
        }
        log_error("Exiting");
        fprintf(stderr, "Exiting\n");
        exit(EXIT_FAILURE);
    ignore:
        ;
    }
}

/* **************************************************************** */
void emit_ocaml_bigarray_pkg(char *runfiles,
                             char *switch_lib,
                             char *coswitch_lib)
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_debug("emit_ocaml_bigarray_pkg");
#endif

    UT_string *bigarray_dir;
    utstring_new(bigarray_dir);
    utstring_printf(bigarray_dir,
                    "%s/bigarray",
                    switch_lib);
    int rc = access(utstring_body(bigarray_dir), F_OK);
    if (rc != 0) {
#if defined(TRACING)
        /* if (mibl_trace) */
        log_warn(YEL "NOT FOUND: %s",
                 utstring_body(bigarray_dir));
#endif
        utstring_free(bigarray_dir);
        // v >= 5.0.0 does not include any bigarray archive
        return;
#if defined(TRACING)
    } else {
        log_warn(YEL "FOUND: %s", utstring_body(bigarray_dir));
#endif
    }

    UT_string *templates;
    utstring_new(templates);
    utstring_printf(templates, "%s/%s",
                    runfiles, "/new/coswitch/templates");

    UT_string *src_file;
    UT_string *dst_dir;
    UT_string *dst_file;
    utstring_new(src_file);
    utstring_new(dst_dir);
    utstring_new(dst_file);

    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "ocaml_bigarray.BUILD");
    utstring_printf(dst_dir,
                    "%s/ocaml/bigarray",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_dir));
    utstring_printf(dst_file, "%s/BUILD.bazel",
                    utstring_body(dst_dir));
    _copy_buildfile(utstring_body(src_file), dst_file);

    // if we found <switch>/lib/bigarray,
    // then the files will be in <switch>/lib/ocaml
    // (not <switch>/lib/ocaml/bigarray)
    utstring_renew(bigarray_dir);
    utstring_printf(bigarray_dir,
                    "%s/ocaml",
                    switch_lib);
    _symlink_ocaml_bigarray(bigarray_dir, utstring_body(dst_dir))
;

    utstring_free(src_file);
    utstring_free(dst_file);
    utstring_free(templates);
    utstring_free(bigarray_dir);
}

void _symlink_ocaml_bigarray(UT_string *bigarray_dir,
                             char *tgtdir)
{
#if defined(TRACING)
    /* if (mibl_debug_symlinks) */
    log_debug("_symlink_ocaml_bigarray");
    log_debug("src: %s, dst: %s",
              utstring_body(bigarray_dir), tgtdir);
#endif

    /* UT_string *opamdir; */
    /* utstring_new(opamdir); */
    /* utstring_printf(opamdir, "%s/ocaml", switch_lib); */
    /*                 /\* utstring_body(opam_switch_lib)); *\/ */

    UT_string *src;
    utstring_new(src);
    UT_string *dst;
    utstring_new(dst);
    int rc;

    DIR *d = opendir(utstring_body(bigarray_dir));
    if (d == NULL) {
        perror(utstring_body(bigarray_dir));
        log_error(RED "Unable to opendir for symlinking bigarray" CRESET " %s\n",
                utstring_body(bigarray_dir));
        /* exit(EXIT_FAILURE); */
        /* this can happen if a related pkg is not installed */
        /* example, see topkg and topkg-care */
        return;
    }

    struct dirent *direntry;
    while ((direntry = readdir(d)) != NULL) {
        if(direntry->d_type==DT_REG){
            /* link files starting with "bigarray" */
            if (strncmp("bigarray", direntry->d_name, 8) != 0)
                continue;

            utstring_renew(src);
            utstring_printf(src, "%s/%s",
                            utstring_body(bigarray_dir),
                            direntry->d_name);
            utstring_renew(dst);
            utstring_printf(dst, "%s/%s",
                            tgtdir, direntry->d_name);
            /* log_debug("symlinking %s to %s", */
            /*        utstring_body(src), */
            /*        utstring_body(dst)); */
            errno = 0;
            rc = symlink(utstring_body(src),
                         utstring_body(dst));
            symlink_ct++;
            if (rc != 0) {
                if (errno != EEXIST) {
                    /* perror(utstring_body(src)); */
                    log_error("ERROR: %s", strerror(errno));
                    log_error("src: %s, dst: %s",
                              utstring_body(src),
                              utstring_body(dst));
                    log_error("exiting");
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    closedir(d);
}

/*
  for <switch>/lib/compiler-libs.
  all targets aliased to <switch>/lib/ocaml/compiler-libs
 */
void emit_compiler_libs_pkg(char *runfiles, char *coswitch_lib)
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_debug("emit_compiler_libs_pkg");
#endif
    UT_string *templates;
    utstring_new(templates);
    utstring_printf(templates, "%s/%s",
                    runfiles, "/new/coswitch/templates");

    UT_string *src_file;
    utstring_new(src_file);
    UT_string *dst_file;
    utstring_new(dst_file);

    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "compiler_libs/common.BUILD");
    utstring_printf(dst_file,
                    "%s/compiler-libs/lib/compiler-libs",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "compiler_libs/common.BUILD");
    utstring_renew(dst_file);
    utstring_printf(dst_file,
                    "%s/compiler-libs/lib/common",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "compiler_libs/bytecomp.BUILD");
    utstring_renew(dst_file);
    utstring_printf(dst_file, "%s/compiler-libs/lib/bytecomp",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "compiler_libs/optcomp.BUILD");
    utstring_renew(dst_file);
    utstring_printf(dst_file, "%s/compiler-libs/lib/optcomp",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "compiler_libs/toplevel.BUILD");
    utstring_renew(dst_file);
    utstring_printf(dst_file, "%s/compiler-libs/lib/toplevel",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_free(src_file);
    utstring_free(dst_file);
    utstring_free(templates);
}

/* **************************************************************** */
/* for <switch>/lib/ocaml/compiler-libs */
void emit_ocaml_compiler_libs_pkg(char *runfiles,
                                  char *switch_lib,
                                  char *coswitch_lib)
                                  /* char *tgtdir) */
/* (char *runfiles, */
/*  char *coswitch_lib) */
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_debug("emit_ocaml_compiler_libs_pkg");
#endif
    UT_string *templates;
    utstring_new(templates);
    utstring_printf(templates, "%s/%s",
                    runfiles, "/new/coswitch/templates");

    UT_string *src_file;
    UT_string *dst_file;
    utstring_new(src_file);
    utstring_new(dst_file);

    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "ocaml_compiler-libs.BUILD");
    utstring_printf(dst_file,
                    "%s/ocaml/compiler-libs",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    /* log_debug("cp src: %s, dst: %s", */
    /*           utstring_body(src_file), */
    /*           utstring_body(dst_file)); */
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "compiler_libs/common.BUILD");
    utstring_renew(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/compiler-libs/common",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "compiler_libs/bytecomp.BUILD");
    utstring_renew(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/compiler-libs/bytecomp",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "compiler_libs/optcomp.BUILD");
    utstring_renew(dst_file);
    utstring_printf(dst_file,
                    "%s/ocaml/compiler-libs/optcomp",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "compiler_libs/toplevel.BUILD");
    utstring_renew(dst_file);
    utstring_printf(dst_file, "%s/ocaml/compiler-libs/toplevel",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_file));
    utstring_printf(dst_file, "/BUILD.bazel");
    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_renew(dst_file);
    utstring_printf(dst_file, "%s/ocaml/compiler-libs",
                    coswitch_lib);
    _symlink_ocaml_compiler_libs(switch_lib,
                                 /* coswitch_lib); */
                                 utstring_body(dst_file));

    utstring_free(src_file);
    utstring_free(dst_file);
    utstring_free(templates);
}

void _symlink_ocaml_compiler_libs(char *switch_lib,
                                  char *coswitch_lib)
                                  /* char *tgtdir) */
{
#if defined(TRACING)
    /* if (mibl_debug_symlinks) */
    log_debug("_symlink_ocaml_compiler_libs");
    /* log_debug("src: %s, dst: %s, tgt: %s", */
    /*           switch_lib, coswitch_lib); //, tgtdir); */
#endif
    UT_string *opamdir;
    utstring_new(opamdir);
    utstring_printf(opamdir, "%s/ocaml/compiler-libs",
                    switch_lib);
                    /* utstring_body(opam_switch_lib)); */
    /* log_debug("OPAM DIR src: %s", utstring_body(opamdir)); */
    UT_string *src;
    utstring_new(src);
    UT_string *dst;
    utstring_new(dst);
    int rc;

    DIR *d = opendir(utstring_body(opamdir));
    if (d == NULL) {
        fprintf(stderr, "Unable to opendir for symlinking compiler-libs: %s\n",
                utstring_body(opamdir));
        /* exit(EXIT_FAILURE); */
        /* this can happen if a related pkg is not installed */
        /* example, see topkg and topkg-care */
        return;
    }

    struct dirent *direntry;
    while ((direntry = readdir(d)) != NULL) {
        if(direntry->d_type==DT_REG){

            utstring_renew(src);
            utstring_printf(src, "%s/%s",
                            utstring_body(opamdir), direntry->d_name);

            utstring_renew(dst);
            utstring_printf(dst, "%s/%s",
                            //tgtdir,
                            coswitch_lib,
                            direntry->d_name);
            /* log_debug("symlinking %s to %s", */
            /*           utstring_body(src), */
            /*           utstring_body(dst)); */
            rc = symlink(utstring_body(src),
                         utstring_body(dst));
            symlink_ct++;
            if (rc != 0) {
                if (errno != EEXIST) {
                    perror(utstring_body(src));
                    log_error("exiting");
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    closedir(d);
}

/* ***************************************** */
void emit_ocaml_dynlink_pkg(char *runfiles,
                            char *switch_lib,
                            char *coswitch_lib)
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_trace("emit_ocaml_dynlink_pkg");
#endif
    bool toplevel = false;

    UT_string *dynlink_dir;
    utstring_new(dynlink_dir);
    utstring_printf(dynlink_dir,
                    "%s/dynlink",
                    switch_lib);
    int rc = access(utstring_body(dynlink_dir), F_OK);
    if (rc != 0) {
#if defined(TRACING)
        /* if (mibl_trace) */
        log_warn(YEL "NOT FOUND: %s",
                 utstring_body(dynlink_dir));
#endif
        utstring_new(dynlink_dir);
        utstring_printf(dynlink_dir,
                        "%s/ocaml/dynlink",
                        switch_lib);
        int rc = access(utstring_body(dynlink_dir), F_OK);
        if (rc != 0) {
#if defined(TRACING)
            /* if (mibl_trace) */
            log_warn(YEL "NOT FOUND: %s",
                     utstring_body(dynlink_dir));
#endif
            utstring_free(dynlink_dir);
            return;
#if defined(TRACING)
        } else {
            log_warn(YEL "FOUND: %s", utstring_body(dynlink_dir));
            toplevel = true;

#endif
        }
#if defined(TRACING)
    } else {
        log_warn(YEL "FOUND: %s", utstring_body(dynlink_dir));
#endif
    }

    UT_string *templates;
    utstring_new(templates);
    utstring_printf(templates, "%s/%s",
                    runfiles, "/new/coswitch/templates");

    UT_string *src_file;
    UT_string *dst_dir;
    UT_string *dst_file;
    utstring_new(src_file);
    utstring_new(dst_dir);
    utstring_new(dst_file);

    // 5.0.0+ has <switch>/lib/ocaml/dynlink
    // pre-5.0.0: no <switch>/lib/ocaml/dynlink
    // always emit <coswitch>/lib/ocaml/dynlink
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "ocaml_dynlink.BUILD");
    utstring_printf(dst_dir,
                    "%s/ocaml/dynlink",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_dir));
    utstring_printf(dst_file, "%s/BUILD.bazel",
                    utstring_body(dst_dir));
    _copy_buildfile(utstring_body(src_file), dst_file);

    if (toplevel) {
        // not found: <switch>/lib/dynlink
        // means >= 5.0.0
        // add <coswitch>/lib/dynlink for (bazel) compatibility
        _symlink_ocaml_dynlink(dynlink_dir,
                               utstring_body(dst_dir));
        _emit_toplevel(templates, "ocaml_dynlink_alias.BUILD",
                       src_file,
                       dst_dir, dst_file,
                       coswitch_lib, "dynlink");
    } else {
        // if we found <switch>/lib/dynlink (versions < 5.0.0)
        // then we need to populate <coswitch>/lib/ocaml/dynlink
        // (its already there for >= 5.0.0)
        utstring_new(dynlink_dir);
        utstring_printf(dynlink_dir,
                        "%s/ocaml",
                        switch_lib);
        _symlink_ocaml_dynlink(dynlink_dir,
                               utstring_body(dst_dir));
    }

    utstring_free(src_file);
    utstring_free(dst_file);
    utstring_free(templates);
    utstring_free(dynlink_dir);
}

void _symlink_ocaml_dynlink(UT_string *dynlink_dir, char *tgtdir)
{
#if defined(TRACING)
    /* if (mibl_debug_symlinks) */
        log_debug("_symlink_ocaml_dynlink");
        log_debug("src: %s, dst: %s",
                  utstring_body(dynlink_dir), tgtdir);
#endif
    UT_string *src;
    utstring_new(src);
    UT_string *dst;
    utstring_new(dst);
    int rc;

    DIR *d = opendir(utstring_body(dynlink_dir));
    if (d == NULL) {
        fprintf(stderr, "Unable to opendir for symlinking dynlink: %s\n",
                utstring_body(dynlink_dir));
        /* exit(EXIT_FAILURE); */
        /* this can happen if a related pkg is not installed */
        /* example, see topkg and topkg-care */
        return;
    } else {
        log_debug("opened dir %s", utstring_body(dynlink_dir));
    }

    struct dirent *direntry;
    while ((direntry = readdir(d)) != NULL) {
        if(direntry->d_type==DT_REG){
            /* link files starting with "dynlink" */
            if (strncmp("dynlink", direntry->d_name, 7) != 0)
                continue;

            utstring_renew(src);
            utstring_printf(src, "%s/%s",
                            utstring_body(dynlink_dir),
                            direntry->d_name);
            utstring_renew(dst);
            utstring_printf(dst, "%s/%s",
                            tgtdir, direntry->d_name);
            /* log_debug("symlinking %s to %s", */
            /*           utstring_body(src), */
            /*           utstring_body(dst)); */
            errno = 0;
            rc = symlink(utstring_body(src),
                         utstring_body(dst));
            symlink_ct++;
            if (rc != 0) {
                if (errno != EEXIST) {
                    log_error("ERROR: %s", strerror(errno));
                    log_error("src: %s, dst: %s",
                              utstring_body(src),
                              utstring_body(dst));
                    log_error("exiting");
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    closedir(d);
}

/* ***************************************** */
void emit_ocaml_num_pkg(char *runfiles,
                        char *switch_lib,
                        char *coswitch_lib)
{ /* only if opam 'nums' pseudo-pkg was installed */
#if defined(TRACING)
    /* if (mibl_trace) */
        log_debug("emit_ocaml_num_pkg");
#endif

    UT_string *num_dir;
    utstring_new(num_dir);
    utstring_printf(num_dir,
                    "%s/num",
                    switch_lib);
    int rc = access(utstring_body(num_dir), F_OK);
    if (rc != 0) {
#if defined(TRACING)
        /* if (mibl_trace) */
        log_warn(YEL "NOT FOUND: %s",
                 utstring_body(num_dir));
#endif
        return;
#if defined(TRACING)
    } else {
        log_warn(YEL "FOUND: %s", utstring_body(num_dir));
#endif
    }

    UT_string *dst_file;
/*     utstring_new(dst_file); */
/*     utstring_printf(dst_file, "%s/num/META", switch_lib); */
/*     rc = access(utstring_body(dst_file), F_OK); */
/*     if (rc != 0) { */
/* #if defined(TRACING) */
/*         if (mibl_trace) log_trace(YEL "num pkg not installed" CRESET); */
/* #endif */
/*         return; */
/*     } */

    utstring_new(dst_file);
    /* utstring_concat(dst_file, coswitch_pfx); */
    utstring_printf(dst_file, "%s/ocaml/num/core", coswitch_lib);
    mkdir_r(utstring_body(dst_file));

    log_debug("dst_file: %s", utstring_body(dst_file));
    _symlink_ocaml_num(switch_lib, utstring_body(dst_file));

    UT_string *templates;
    utstring_new(templates);
    utstring_printf(templates, "%s/%s",
                    runfiles, "/new/coswitch/templates");

    UT_string *src_file;
    utstring_new(src_file);
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "ocaml_num.BUILD");
    utstring_printf(dst_file, "/BUILD.bazel");

    _copy_buildfile(utstring_body(src_file), dst_file);

    utstring_free(src_file);
    utstring_free(dst_file);
    utstring_free(templates);
}

void _symlink_ocaml_num(char *switch_lib, char *tgtdir)
{
#if defined(TRACING)
    /* if (mibl_debug_symlinks) */
        log_debug("_symlink_ocaml_num to %s", tgtdir);
#endif

    UT_string *opamdir;
    utstring_new(opamdir);
    utstring_printf(opamdir, "%s/ocaml", switch_lib);
    /* utstring_body(opam_switch_lib)); */

    UT_string *src;
    utstring_new(src);
    UT_string *dst;
    utstring_new(dst);
    int rc;

    DIR *d = opendir(utstring_body(opamdir));
    if (d == NULL) {
        perror(utstring_body(opamdir));
        log_error("Unable to opendir for symlinking num: %s\n",
                  utstring_body(opamdir));
        /* exit(EXIT_FAILURE); */
        /* this can happen if a related pkg is not installed */
        /* example, see topkg and topkg-care */
        return;
    }
#if defined(TRACING)
    if (mibl_debug)
        log_debug("reading num dir %s", utstring_body(opamdir));
#endif

    struct dirent *direntry;
    while ((direntry = readdir(d)) != NULL) {
        if(direntry->d_type==DT_REG){
            /* if (strcasestr(direntry->d_name, "num") == NULL) */
            if (strncmp("nums.", direntry->d_name, 5) != 0)
                if (strncmp("libnums.", direntry->d_name, 8) != 0)
                    continue;

            utstring_renew(src);
            utstring_printf(src, "%s/%s",
                            utstring_body(opamdir), direntry->d_name);
            utstring_renew(dst);
            utstring_printf(dst, "%s/%s",
                            tgtdir, direntry->d_name);
#if defined(TRACING)
            /* if (mibl_debug_symlinks) */
                /* log_debug("symlinking %s to %s\n", */
                /*           utstring_body(src), */
                /*           utstring_body(dst)); */
#endif
            rc = symlink(utstring_body(src),
                         utstring_body(dst));
            symlink_ct++;
            if (rc != 0) {
                if (errno != EEXIST) {
                    perror(utstring_body(src));
                    fprintf(stderr, "exiting\n");
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    closedir(d);
}

/* ************************************* */
void emit_ocaml_str_pkg(char *runfiles,
                        char *switch_lib,
                        char *coswitch_lib)
{
#if defined(TRACING)
    if (mibl_trace) log_debug("emit_ocaml_str_pkg");
#endif

    // toplevel_str true means ocaml version >= 5.0.0
    // which means switch lacks lib/str
    // but we add it to coswitch so @str//lib/str will
    // work with >= 5.0.0
    bool toplevel = false;

    UT_string *str_dir;
    utstring_new(str_dir);
    utstring_printf(str_dir,
                    "%s/str",
                    switch_lib);
    int rc = access(utstring_body(str_dir), F_OK);
    if (rc != 0) {
#if defined(TRACING)
        /* if (mibl_trace) */
        log_warn(YEL "NOT FOUND: %s",
                 utstring_body(str_dir));
#endif
        utstring_new(str_dir);
        utstring_printf(str_dir,
                        "%s/ocaml/str",
                        switch_lib);
        int rc = access(utstring_body(str_dir), F_OK);
        if (rc != 0) {
#if defined(TRACING)
            /* if (mibl_trace) */
            log_warn(YEL "NOT FOUND: %s",
                     utstring_body(str_dir));
#endif
            utstring_free(str_dir);
            return;
#if defined(TRACING)
        } else {
            log_warn(YEL "FOUND: %s", utstring_body(str_dir));
#endif
            toplevel = true;
        }
#if defined(TRACING)
    } else {
        log_warn(YEL "FOUND: %s", utstring_body(str_dir));
#endif
    }

    UT_string *templates;
    utstring_new(templates);
    utstring_printf(templates, "%s/%s",
                    runfiles, "/new/coswitch/templates");

    UT_string *src_file;
    UT_string *dst_dir;
    UT_string *dst_file;
    utstring_new(src_file);
    utstring_new(dst_dir);
    utstring_new(dst_file);

    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "ocaml_str.BUILD");
    utstring_printf(dst_dir,
                    "%s/ocaml/str",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_dir));
    utstring_printf(dst_file, "%s/BUILD.bazel",
                    utstring_body(dst_dir));
    _copy_buildfile(utstring_body(src_file), dst_file);

    if (toplevel) {
        _symlink_ocaml_str(str_dir, utstring_body(dst_dir));
        _emit_toplevel(templates,
                       "ocaml_str_alias.BUILD",
                       src_file,
                       dst_dir, dst_file,
                       coswitch_lib,
                       "str");
    } else {
        // if we found <switch>/lib/str (versions < 5.0.0)
        // then we need to populate <coswitch>/lib/ocaml/str
        // (its already there for >= 5.0.0)
        utstring_new(str_dir);
        utstring_printf(str_dir,
                        "%s/ocaml",
                        switch_lib);
        _symlink_ocaml_str(str_dir,
                           utstring_body(dst_dir));
    }

    utstring_free(src_file);
    utstring_free(dst_file);
    utstring_free(templates);
    utstring_free(str_dir);
}

void _symlink_ocaml_str(UT_string *str_dir, char *tgtdir)
{
#if defined(TRACING)
    /* if (mibl_debug) */
    log_debug("_symlink_ocaml_str");
    log_debug("src: %s, dst %s",
              utstring_body(str_dir),
              tgtdir);
#endif

    /* UT_string *opamdir; */
    /* utstring_new(opamdir); */
    /* utstring_printf(opamdir, "%s/ocaml", switch_lib); */
    /*                 /\* utstring_body(opam_switch_lib)); *\/ */

    UT_string *src;
    utstring_new(src);
    UT_string *dst;
    utstring_new(dst);
    int rc;

    DIR *d = opendir(utstring_body(str_dir));
    if (d == NULL) {
        fprintf(stderr, "Unable to opendir for symlinking str: %s\n",
                utstring_body(str_dir));
        /* exit(EXIT_FAILURE); */
        /* this can happen if a related pkg is not installed */
        /* example, see topkg and topkg-care */
        return;
    } else {
        log_debug("opened dir %s", utstring_body(str_dir));
    }

    struct dirent *direntry;
    while ((direntry = readdir(d)) != NULL) {
        if(direntry->d_type==DT_REG){
            /* link files starting with "str." */
            if (strncmp("str.", direntry->d_name, 4) != 0)
                continue;

            utstring_renew(src);
            utstring_printf(src, "%s/%s",
                            utstring_body(str_dir),
                            direntry->d_name);
            utstring_renew(dst);
            utstring_printf(dst, "%s/%s",
                            tgtdir, direntry->d_name);
            /* printf("symlinking %s to %s\n", */
            /*        utstring_body(src), */
            /*        utstring_body(dst)); */
            rc = symlink(utstring_body(src),
                         utstring_body(dst));
            symlink_ct++;
            if (rc != 0) {
                if (errno != EEXIST) {
                    log_error("ERROR: %s", strerror(errno));
                    log_error("src: %s, dst: %s",
                              utstring_body(src),
                              utstring_body(dst));
                    log_error("exiting");
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    closedir(d);
}

/* ***************************************** */
/**
   pre-5.0.0: <switch>/threads and <switch>/lib/ocaml/threads
   5.0.0+:    <switch>/lib/ocaml/threads only
 */
void emit_ocaml_threads_pkg(char *runfiles,
                            char *switch_lib,
                            char *coswitch_lib)
{
#if defined(TRACING)
    /* if (mibl_trace) */
    log_trace("emit_ocaml_threads_pkg");
#endif
    bool toplevel = false;

    UT_string *threads_dir;
    utstring_new(threads_dir);
    utstring_printf(threads_dir,
                    "%s/threads",
                    switch_lib);
    int rc = access(utstring_body(threads_dir), F_OK);
    if (rc != 0) {
#if defined(TRACING)
        /* if (mibl_trace) */
        log_warn(YEL "NOT FOUND: %s",
                 utstring_body(threads_dir));
#endif
        utstring_new(threads_dir);
        utstring_printf(threads_dir,
                        "%s/ocaml/threads",
                        switch_lib);
        int rc = access(utstring_body(threads_dir), F_OK);
        if (rc != 0) {
#if defined(TRACING)
            /* if (mibl_trace) */
            log_warn(YEL "NOT FOUND: %s",
                     utstring_body(threads_dir));
#endif
            utstring_free(threads_dir);
            return;
#if defined(TRACING)
        } else {
            log_warn(YEL "FOUND: %s", utstring_body(threads_dir));
            toplevel = true;

#endif
        }
#if defined(TRACING)
    } else {
        log_warn(YEL "FOUND: %s", utstring_body(threads_dir));
#endif
    }

    // 5.0.0+ has <switch>/lib/ocaml/threads
    // pre-5.0.0: no <switch>/lib/ocaml/threads
    // always emit <coswitch>/lib/ocaml/threads
    UT_string *templates;
    utstring_new(templates);
    utstring_printf(templates, "%s/%s",
                    runfiles, "/new/coswitch/templates");

    UT_string *src_file;
    UT_string *dst_dir;
    UT_string *dst_file;
    utstring_new(src_file);
    utstring_new(dst_dir);
    utstring_new(dst_file);

    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "ocaml_threads.BUILD");
    utstring_printf(dst_dir,
                    "%s/ocaml/threads",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_dir));
    utstring_printf(dst_file, "%s/BUILD.bazel",
                    utstring_body(dst_dir));
    _copy_buildfile(utstring_body(src_file), dst_file);

    if (toplevel) {
        // not found: <switch>/lib/threads
        // means >= 5.0.0
        // add <coswitch>/lib/threads for (bazel) compatibility
        _emit_toplevel(templates,
                       "ocaml_threads_alias.BUILD",
                       src_file,
                       dst_dir, dst_file,
                       coswitch_lib,
                       "threads");
    }
    // all versions have <switch>/lib/ocaml/threads
    // so we always symlink to <coswitch>/lib/ocaml/threads
    utstring_new(threads_dir);
    utstring_printf(threads_dir,
                    "%s/ocaml",
                    switch_lib);
    utstring_renew(dst_dir);
    utstring_printf(dst_dir,
                    "%s/ocaml/threads",
                    coswitch_lib);

    // symlink <switch>/lib/ocaml/threads
    _symlink_ocaml_threads(threads_dir, utstring_body(dst_dir));
    // symlink <switch>/lib/ocaml/libthreads.s, libthreadsnat.a

    utstring_free(src_file);
    utstring_free(dst_file);
    utstring_free(templates);
    utstring_free(threads_dir);
}

void _symlink_ocaml_threads(UT_string *threads_dir, char *tgtdir)
{
#if defined(TRACING)
    /* if (mibl_debug_symlinks) */
        log_debug("_symlink_ocaml_threads");
        log_debug("src: %s, dst: %s",
                  utstring_body(threads_dir), tgtdir);
#endif
    UT_string *src;
    utstring_new(src);
    UT_string *dst;
    utstring_new(dst);
    int rc;

    utstring_printf(src, "%s/libthreads.a",
                    utstring_body(threads_dir));
    //FIXME: verify access
    utstring_printf(dst, "%s/libthreads.a",
                    tgtdir);
    log_debug("SYMLINKING %s to %s\n",
              utstring_body(src),
              utstring_body(dst));
    rc = symlink(utstring_body(src),
                 utstring_body(dst));
    symlink_ct++;
    if (rc != 0) {
        if (errno != EEXIST) {
            log_error("ERROR: %s", strerror(errno));
            log_error("src: %s, dst: %s",
                      utstring_body(src),
                      utstring_body(dst));
            log_error("exiting");
            exit(EXIT_FAILURE);
        }
    }

    utstring_renew(src);
    utstring_printf(src, "%s/libthreadsnat.a",
                    utstring_body(threads_dir));
    //FIXME: verify access
    utstring_renew(dst);
    utstring_printf(dst, "%s/libthreadsnat.a",
                    tgtdir);
    log_debug("SYMLINKING %s to %s\n",
              utstring_body(src),
              utstring_body(dst));
    rc = symlink(utstring_body(src),
                 utstring_body(dst));
    symlink_ct++;
    if (rc != 0) {
        if (errno != EEXIST) {
            log_error("ERROR: %s", strerror(errno));
            log_error("src: %s, dst: %s",
                      utstring_body(src),
                      utstring_body(dst));
            log_error("exiting");
            exit(EXIT_FAILURE);
        }
    }

    utstring_printf(threads_dir, "/%s", "threads");
    DIR *d = opendir(utstring_body(threads_dir));
    if (d == NULL) {
        fprintf(stderr, "Unable to opendir for symlinking threads: %s\n",
                utstring_body(threads_dir));
        log_error("Unable to opendir for symlinking threads: %s\n", utstring_body(threads_dir));
        /* exit(EXIT_FAILURE); */
        /* this can happen if a related pkg is not installed */
        /* example, see topkg and topkg-care */
        return;
    }

    struct dirent *direntry;
    while ((direntry = readdir(d)) != NULL) {
        if(direntry->d_type==DT_REG){
            utstring_renew(src);
            utstring_printf(src, "%s/%s",
                            utstring_body(threads_dir),
                            direntry->d_name);
            utstring_renew(dst);
            utstring_printf(dst, "%s/%s",
                            tgtdir, direntry->d_name);
            /* printf("symlinking %s to %s\n", */
            /*        utstring_body(src), */
            /*        utstring_body(dst)); */
            rc = symlink(utstring_body(src),
                         utstring_body(dst));
            symlink_ct++;
            if (rc != 0) {
                if (errno != EEXIST) {
                    log_error("ERROR: %s", strerror(errno));
                    log_error("src: %s, dst: %s",
                              utstring_body(src),
                              utstring_body(dst));
                    log_error("exiting");
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    closedir(d);
}

/* ***************************************** */
//FIXMEFIXME
void emit_ocaml_ocamldoc_pkg(char *runfiles,
                             char *switch_lib,
                             char *coswitch_lib)
{
#if defined(TRACING)
    if (mibl_debug) log_debug("emit_ocaml_ocamldoc_pkg");
#endif

    UT_string *ocamldoc_dir;
    utstring_new(ocamldoc_dir);
    utstring_printf(ocamldoc_dir,
                    "%s/ocamldoc",
                    switch_lib);
    int rc = access(utstring_body(ocamldoc_dir), F_OK);
    if (rc != 0) {
#if defined(TRACING)
        /* if (mibl_trace) */
        log_warn(YEL "NOT FOUND: %s",
                 utstring_body(ocamldoc_dir));
#endif
        utstring_new(ocamldoc_dir);
        utstring_printf(ocamldoc_dir,
                        "%s/ocaml/ocamldoc",
                        switch_lib);
        int rc = access(utstring_body(ocamldoc_dir), F_OK);
        if (rc != 0) {
#if defined(TRACING)
            /* if (mibl_trace) */
            log_warn(YEL "NOT FOUND: %s",
                     utstring_body(ocamldoc_dir));
#endif
            utstring_free(ocamldoc_dir);
            return;
#if defined(TRACING)
        } else {
            log_warn(YEL "FOUND: %s", utstring_body(ocamldoc_dir));
#endif
        }
#if defined(TRACING)
    } else {
        log_warn(YEL "FOUND: %s", utstring_body(ocamldoc_dir));
#endif
    }

    UT_string *templates;
    utstring_new(templates);
    utstring_printf(templates, "%s/%s",
                    runfiles, "/new/coswitch/templates");

    UT_string *src_file;
    UT_string *dst_dir;
    UT_string *dst_file;
    utstring_new(src_file);
    utstring_new(dst_dir);
    utstring_new(dst_file);

    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "ocaml_ocamldoc.BUILD");
    utstring_printf(dst_dir,
                    "%s/ocaml/ocamldoc",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_dir));
    utstring_printf(dst_file, "%s/BUILD.bazel",
                    utstring_body(dst_dir));
    _copy_buildfile(utstring_body(src_file), dst_file);

    _symlink_ocaml_ocamldoc(ocamldoc_dir, utstring_body(dst_dir));

    utstring_free(src_file);
    utstring_free(dst_file);
    utstring_free(templates);
    utstring_free(ocamldoc_dir);
}

void _symlink_ocaml_ocamldoc(UT_string *ocamldoc_dir,
                             char *tgtdir)
{
#if defined(TRACING)
    /* if (mibl_debug_symlinks) */
        log_debug("_symlink_ocaml_ocamldoc to %s\n", tgtdir);
#endif

    /* UT_string *opamdir; */
    /* utstring_new(opamdir); */
    /* utstring_printf(opamdir, "%s/ocaml/ocamldoc", switch_lib); */
    /*                 /\* utstring_body(opam_switch_lib)); *\/ */

    UT_string *src;
    utstring_new(src);
    UT_string *dst;
    utstring_new(dst);
    int rc;

    DIR *d = opendir(utstring_body(ocamldoc_dir));
    if (d == NULL) {
        fprintf(stderr, "Unable to opendir for symlinking ocamldoc: %s\n",
                utstring_body(ocamldoc_dir));
        /* exit(EXIT_FAILURE); */
        /* this can happen if a related pkg is not installed */
        /* example, see topkg and topkg-care */
        return;
    } else {
        log_debug("opened dir %s", utstring_body(ocamldoc_dir));
    }

    struct dirent *direntry;
    while ((direntry = readdir(d)) != NULL) {
        if(direntry->d_type==DT_REG){
            /* if (strcasestr(direntry->d_name, "ocamldoc") == NULL) */
            /* if (strncmp("odoc", direntry->d_name, 4) != 0) */
            /*     continue; */

            utstring_renew(src);
            utstring_printf(src, "%s/%s",
                            utstring_body(ocamldoc_dir),
                            direntry->d_name);
            utstring_renew(dst);
            utstring_printf(dst, "%s/%s",
                            tgtdir, direntry->d_name);
            /* log_debug("ocamldoc symlinking %s to %s", */
            /*           utstring_body(src), */
            /*           utstring_body(dst)); */
            rc = symlink(utstring_body(src),
                         utstring_body(dst));
            symlink_ct++;
            if (rc != 0) {
                if (errno != EEXIST) {
                    log_error("ERROR: %s", strerror(errno));
                    log_error("src: %s, dst: %s",
                              utstring_body(src),
                              utstring_body(dst));
                    log_error("exiting");
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    closedir(d);
}

void emit_ocaml_unix_pkg(char *runfiles,
                         char *switch_lib,
                         char *coswitch_lib)
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_trace("emit_ocaml_unix_pkg");
#endif
    bool toplevel = false;

    UT_string *unix_dir;
    utstring_new(unix_dir);
    utstring_printf(unix_dir,
                    "%s/unix",
                    switch_lib);
    int rc = access(utstring_body(unix_dir), F_OK);
    if (rc != 0) {
#if defined(TRACING)
        /* if (mibl_trace) */
        log_warn(YEL "NOT FOUND: %s",
                 utstring_body(unix_dir));
#endif
        utstring_new(unix_dir);
        utstring_printf(unix_dir,
                        "%s/ocaml/unix",
                        switch_lib);
        int rc = access(utstring_body(unix_dir), F_OK);
        if (rc != 0) {
#if defined(TRACING)
            /* if (mibl_trace) */
            log_warn(YEL "NOT FOUND: %s",
                     utstring_body(unix_dir));
#endif
            utstring_free(unix_dir);
            return;
#if defined(TRACING)
        } else {
            log_warn(YEL "FOUND: %s", utstring_body(unix_dir));
            toplevel = true;

#endif
        }
#if defined(TRACING)
    } else {
        log_warn(YEL "FOUND: %s", utstring_body(unix_dir));
#endif
    }

    UT_string *templates;
    utstring_new(templates);
    utstring_printf(templates, "%s/%s",
                    runfiles, "/new/coswitch/templates");

    UT_string *src_file;
    UT_string *dst_dir;
    UT_string *dst_file;
    utstring_new(src_file);
    utstring_new(dst_dir);
    utstring_new(dst_file);

    // 5.0.0+ has <switch>/lib/ocaml/unix
    // pre-5.0.0: no <switch>/lib/ocaml/unix
    // always emit <coswitch>/lib/ocaml/unix
    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "ocaml_unix.BUILD");
    utstring_printf(dst_dir,
                    "%s/ocaml/unix",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_dir));
    utstring_printf(dst_file, "%s/BUILD.bazel",
                    utstring_body(dst_dir));
    _copy_buildfile(utstring_body(src_file), dst_file);

    if (toplevel) {
        // found <switch>/lib/ocaml/unix,
        // not found: <switch>/lib/unix, means >= 5.0.0
        // link the former,
        // add <coswitch>/lib/unix alias for (bazel) compat
        _symlink_ocaml_unix(unix_dir,
                            utstring_body(dst_dir));
        // also link <switch>/lib/ocaml/libunix.a
        utstring_renew(src_file);
        utstring_printf(src_file, "%s/ocaml/libunix.a",
                        switch_lib);
        //FIXME: verify access
        utstring_renew(dst_file);
        utstring_printf(dst_file, "%s/libunix.a",
                        utstring_body(dst_dir));
        log_debug("SYMLINKING %s to %s\n",
                  utstring_body(src_file),
                  utstring_body(dst_file));
        rc = symlink(utstring_body(src_file),
                     utstring_body(dst_file));
        symlink_ct++;
        if (rc != 0) {
            if (errno != EEXIST) {
                log_error("ERROR: %s", strerror(errno));
                log_error("src: %s, dst: %s",
                          utstring_body(src_file),
                          utstring_body(dst_file));
                log_error("exiting");
                exit(EXIT_FAILURE);
            }
        }

        _emit_toplevel(templates,
                       "ocaml_unix_alias.BUILD",
                       src_file,
                       dst_dir, dst_file,
                       coswitch_lib,
                       "unix");
    } else {
        // if we found <switch>/lib/unix (versions < 5.0.0)
        // then we need to populate <coswitch>/lib/ocaml/unix
        // (its already there for >= 5.0.0)
        utstring_new(unix_dir);
        utstring_printf(unix_dir,
                        "%s/ocaml",
                        switch_lib);
        _symlink_ocaml_unix(unix_dir, utstring_body(dst_dir));
    }

    utstring_free(src_file);
    utstring_free(dst_file);
    utstring_free(templates);
    utstring_free(unix_dir);
}

void _symlink_ocaml_unix(UT_string *unix_dir, char *tgtdir)
{
#if defined(TRACING)
    if (mibl_debug_symlinks)
        log_debug("_symlink_ocaml_unix to %s\n", tgtdir);
#endif
    UT_string *src;
    utstring_new(src);
    UT_string *dst;
    utstring_new(dst);
    int rc;

    DIR *d = opendir(utstring_body(unix_dir));
    if (d == NULL) {
        fprintf(stderr, "Unable to opendir for symlinking unix: %s\n",
                utstring_body(unix_dir));
        /* exit(EXIT_FAILURE); */
        /* this can happen if a related pkg is not installed */
        /* example, see topkg and topkg-care */
        return;
    }

    struct dirent *direntry;
    while ((direntry = readdir(d)) != NULL) {
        if(direntry->d_type==DT_REG){
            /* link files starting with "unix." */
            if (strncmp("unix.", direntry->d_name, 5) != 0)
                if (strncmp("unixLabels.", direntry->d_name, 11) != 0)
                    if (strncmp("libunix", direntry->d_name, 7) != 0)
                continue;

            utstring_renew(src);
            utstring_printf(src, "%s/%s",
                            utstring_body(unix_dir),
                            direntry->d_name);
            utstring_renew(dst);
            utstring_printf(dst, "%s/%s",
                            tgtdir, direntry->d_name);
            /* log_debug("symlinking %s to %s", */
            /*        utstring_body(src), */
            /*        utstring_body(dst)); */
            rc = symlink(utstring_body(src),
                         utstring_body(dst));
            symlink_ct++;
            if (rc != 0) {
                if (errno != EEXIST) {
                    log_error("ERROR: %s", strerror(errno));
                    log_error("src: %s, dst: %s",
                              utstring_body(src),
                              utstring_body(dst));
                    log_error("exiting");
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    closedir(d);
}

/* ***************************************** */
void emit_ocaml_c_api_pkg(char *runfiles,
                          char *switch_lib,
                          char *coswitch_lib)
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_trace(BLU "emit_ocaml_c_api_pkg" CRESET);
#endif

    UT_string *templates;
    utstring_new(templates);
    utstring_printf(templates, "%s/%s",
                    runfiles, "/new/coswitch/templates");

    UT_string *src_file;
    UT_string *dst_dir;
    UT_string *dst_file;
    utstring_new(src_file);
    utstring_new(dst_dir);
    utstring_new(dst_file);

    utstring_printf(src_file, "%s/%s",
                    utstring_body(templates),
                    "ocaml_c_api.BUILD");
    utstring_printf(dst_dir,
                    "%s/ocaml/c",
                    coswitch_lib);
    mkdir_r(utstring_body(dst_dir));

    utstring_printf(dst_file,
                    "%s/BUILD.bazel",
                    utstring_body(dst_dir));
    _copy_buildfile(utstring_body(src_file), dst_file);

    _symlink_ocaml_c_hdrs(switch_lib, utstring_body(dst_dir));
    _symlink_ocaml_c_libs(switch_lib, utstring_body(dst_dir));

    utstring_free(templates);
    utstring_free(src_file);
    utstring_free(dst_dir);
    utstring_free(dst_file);
}

void _symlink_ocaml_c_hdrs(char *switch_lib, char *tgtdir)
{
#if defined(TRACING)
    if (mibl_debug_symlinks)
        log_debug("_symlink_ocaml_c_hdrs to %s\n", tgtdir);
#endif

    UT_string *opamdir;
    utstring_new(opamdir);
    utstring_printf(opamdir, "%s/ocaml/caml", switch_lib);

    UT_string *obazldir;
    utstring_new(obazldir);
    utstring_printf(obazldir, "%s/caml", tgtdir);
    mkdir_r(utstring_body(obazldir));

    UT_string *src;
    utstring_new(src);
    UT_string *dst;
    utstring_new(dst);
    int rc;

    DIR *d = opendir(utstring_body(opamdir));
    if (d == NULL) {
        fprintf(stderr, "Unable to opendir for symlinking c sdk: %s\n",
                utstring_body(opamdir));
        /* exit(EXIT_FAILURE); */
        /* this can happen if a related pkg is not installed */
        /* example, see topkg and topkg-care */
        return;
    }

    struct dirent *direntry;
    while ((direntry = readdir(d)) != NULL) {
        if(direntry->d_type==DT_REG){
            /* if (strncmp("lib", direntry->d_name, 3) != 0) */
            /*     continue;       /\* no match *\/ */
            //FIXME: check for .h?

            utstring_renew(src);
            utstring_printf(src, "%s/%s",
                            utstring_body(opamdir),
                            direntry->d_name);
            utstring_renew(dst);
            utstring_printf(dst, "%s/%s",
                            utstring_body(obazldir),
                            direntry->d_name);
#if defined(TRACING)
            /* if (mibl_debug_symlinks) { */
            /*     log_debug("c_hdrs: symlinking %s to %s\n", */
            /*               utstring_body(src), */
            /*               utstring_body(dst)); */
            /* } */
#endif
            errno = 0;
            rc = symlink(utstring_body(src),
                         utstring_body(dst));
            symlink_ct++;
            if (rc != 0) {
                if (errno != EEXIST) {
                    log_error("ERROR: %s", strerror(errno));
                    log_error("src: %s, dst: %s",
                              utstring_body(src),
                              utstring_body(dst));
                    log_error("exiting");
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    closedir(d);
}

void _symlink_ocaml_c_libs(char *switch_lib, char *tgtdir)
{
#if defined(TRACING)
    if (mibl_debug_symlinks)
        log_debug("_symlink_ocaml_c_libs to %s\n", tgtdir);
#endif

    UT_string *opamdir;
    utstring_new(opamdir);
    utstring_printf(opamdir, "%s/ocaml", switch_lib);
                    /* utstring_body(opam_switch_lib)); */

    UT_string *obazldir;
    utstring_new(obazldir);
    utstring_printf(obazldir, "%s/lib", tgtdir);
    mkdir_r(utstring_body(obazldir));

    UT_string *src;
    utstring_new(src);
    UT_string *dst;
    utstring_new(dst);
    int rc;

    DIR *d = opendir(utstring_body(opamdir));
    if (d == NULL) {
        fprintf(stderr, "Unable to opendir for symlinking c_libs: %s\n",
                utstring_body(opamdir));
        /* exit(EXIT_FAILURE); */
        /* this can happen if a related pkg is not installed */
        /* example, see topkg and topkg-care */
        return;
    }

    struct dirent *direntry;
    while ((direntry = readdir(d)) != NULL) {
        if(direntry->d_type==DT_REG){
            if (strncmp("lib", direntry->d_name, 3) != 0)
                continue;       /* no match */

            utstring_renew(src);
            utstring_printf(src, "%s/%s",
                            utstring_body(opamdir), direntry->d_name);
            utstring_renew(dst);
            utstring_printf(dst, "%s/%s",
                            utstring_body(obazldir), /* tgtdir, */
                            direntry->d_name);
#if defined(TRACING)
            /* if (mibl_debug_symlinks) { */
            /*     log_debug("c_libs: symlinking %s to %s\n", */
            /*               utstring_body(src), */
            /*               utstring_body(dst)); */
            /* } */
#endif
            errno = 0;
            rc = symlink(utstring_body(src),
                         utstring_body(dst));
            symlink_ct++;
            if (rc != 0) {
                if (errno != EEXIST) {
                    fprintf(stdout, "c_libs symlink errno: %d: %s\n",
                            errno, strerror(errno));
                    fprintf(stdout, "c_libs src: %s, dst: %s\n",
                            utstring_body(src),
                            utstring_body(dst));
                    fprintf(stderr, "exiting\n");
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    closedir(d);
}

/* **************************************************************** */
/* void emit_ocaml_bootstrap(char *opam_switch, FILE *bootstrap_FILE) */
/* { */
/*     (void)opam_switch; */
/* #if defined(TRACING) */
/*     if (mibl_trace) log_trace("emit_ocaml_bootstrap"); */
/* #endif */

/*     fprintf(bootstrap_FILE, "    native.local_repository(\n"); */
/*     fprintf(bootstrap_FILE, "        name       = \"ocaml\",\n"); */

/*     fprintf(bootstrap_FILE, "        path       = "); */
/*     fprintf(bootstrap_FILE, "\"%s/%s\",\n", */
/*             utstring_body(coswitch_pfx), */
/*             "ocaml"); */
/*     fprintf(bootstrap_FILE, "    )\n\n"); */

/*     /\* we also always have lib/stublibs, which is not an OPAM pkg *\/ */
/*     fprintf(bootstrap_FILE, "    native.local_repository(\n"); */
/*     fprintf(bootstrap_FILE, "        name       = \"stublibs\",\n"); */

/*     fprintf(bootstrap_FILE, "        path       = "); */
/*     fprintf(bootstrap_FILE, "\"%s/%s\",\n", */
/*             utstring_body(coswitch_pfx), */
/*             "lib/stublibs"); */
/*     fprintf(bootstrap_FILE, "    )\n\n"); */
/* } */

/*
  src: an opam <switch>/lib dir
  dst: same as src
      or XDG_DATA_HOME/obazl/opam/<switch>/
      or project-local _opam/
  bootstrap_FILE: remains open so opam pkgs can write to it
 */
EXPORT void emit_ocaml_workspace(UT_string *registry,
                                 char *switch_name,
                                 char *switch_pfx,
                                 char *coswitch_lib) //dst
{
#if defined(TRACING)
    /* if (mibl_trace) */
        log_trace(BLU "EMIT_ocaml_workspace:" CRESET
                  " switch_pfx:%s, dst: %s",
                  switch_pfx, coswitch_lib);
#endif

    char *switch_lib = opam_switch_lib(switch_name);

    // emit registry record for @ocaml
    log_debug("registry: %s", utstring_body(registry));

    emit_registry_record(registry,
                         NULL, // meta_path
                         "ocaml", // pkg_name
                         NULL, // pkg
                         "0.0.0" // default_version
                         );

    /* UT_string *switch_lib; */
    /* utstring_new(switch_lib); */
    /* utstring_printf(switch_lib, "%s/lib", switch_pfx); */

    /* printf("emit_ocaml_repo, coswitch_pfx: %s\n", */
    /*        utstring_body(coswitch_pfx)); */

    /* If we're writing bazel stuff directly into an opam switch, we
       do not need to write a BOOTSTRAP.bzl file here. We can always
       emit one for a list of pkgs since we know where the workspaces
       are. */
    /* emit_ocaml_bootstrap(opam_switch_lib, bootstrap_FILE); */

    UT_string *dst_file;
    utstring_new(dst_file);
    /* utstring_concat(dst_file, coswitch_lib); // pfx); */
    utstring_printf(dst_file, "%s/ocaml", coswitch_lib);
    mkdir_r(utstring_body(dst_file));

    utstring_printf(dst_file, "/WORKSPACE.bazel");

    if (verbose && verbosity > 1)
        log_info("Writing ws file: %s", utstring_body(dst_file));

    FILE *ostream = fopen(utstring_body(dst_file), "w");
    if (ostream == NULL) {
        log_error("fopen: %s: %s", strerror(errno),
                  utstring_body(dst_file));
        fprintf(stderr, "fopen: %s: %s", strerror(errno),
                utstring_body(dst_file));
        fprintf(stderr, "exiting\n");
        /* perror(utstring_body(dst_file)); */
        exit(EXIT_FAILURE);
    }
    fprintf(ostream, "workspace( name = \"ocaml\" )"
            "    # generated file - DO NOT EDIT\n");

    fclose(ostream);

    /* now drop WORKSPACE.bazel from path */
    utstring_renew(dst_file);
    /* utstring_concat(dst_file, coswitch_lib); // pfx); */
    utstring_printf(dst_file, "%s/ocaml", coswitch_lib);

    char *runfiles;
    if (strlen(BAZEL_CURRENT_REPOSITORY) == 0) {
        runfiles = realpath(".", NULL);
    } else {
        runfiles = realpath("external/" BAZEL_CURRENT_REPOSITORY, NULL);
    }
    log_debug("RUNFILES: %s", runfiles);

    emit_ocaml_bin_dir(switch_pfx, coswitch_lib);

    emit_ocaml_platform_buildfiles(runfiles, coswitch_lib);

    emit_ocaml_toolchain_buildfiles(runfiles, coswitch_lib);

    /* FIXME: decide on 'stdlib' or 'runtime' */
    /* X emit_ocaml_stdlib_pkg(opam_switch_lib); */
    emit_ocaml_runtime_pkg(runfiles,
                           switch_lib,
                           coswitch_lib);

    emit_ocaml_stublibs(switch_pfx, coswitch_lib);

    char *switch_stublibs = opam_switch_stublibs(switch_name);
    emit_lib_stublibs(switch_stublibs, coswitch_lib);

    emit_compiler_libs_pkg(runfiles, coswitch_lib);
    emit_ocaml_compiler_libs_pkg(runfiles,
                                 switch_lib,
                                 coswitch_lib);

    emit_ocaml_bigarray_pkg(runfiles, switch_lib, coswitch_lib);

    emit_ocaml_dynlink_pkg(runfiles, switch_lib, coswitch_lib);

    emit_ocaml_num_pkg(runfiles, switch_lib, coswitch_lib);

    //FIXMEFIXME:
    emit_ocaml_ocamldoc_pkg(runfiles, switch_lib, coswitch_lib);

    emit_ocaml_str_pkg(runfiles, switch_lib, coswitch_lib);

    //NB: vmthreads removed in v. 4.08.0?
    emit_ocaml_threads_pkg(runfiles, switch_lib, coswitch_lib);

    emit_ocaml_unix_pkg(runfiles, switch_lib, coswitch_lib);

    emit_ocaml_c_api_pkg(runfiles, switch_lib, coswitch_lib);
}
