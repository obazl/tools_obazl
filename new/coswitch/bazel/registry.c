#include <errno.h>

#include "log.h"
#include "findlibc.h"
#include "utstring.h"

#include "registry.h"

const char *platforms_version = "0.0.7";
const char *rules_ocaml_version = "1.0.0";

#if INTERFACE
#define INFOFD stdout
#endif

void _emit_reg_rec(UT_string *reg_file, char *pkg_name)
{
    TRACE_ENTRY;
    FILE *ostream;
    ostream = fopen(utstring_body(reg_file), "w");
    if (ostream == NULL) {
        log_error("%s", strerror(errno));
        perror(utstring_body(reg_file));
        exit(EXIT_FAILURE);
    }

    fprintf(ostream, "## generated file - DO NOT EDIT\n\n");

    /* always set ocaml and stublibs version to 0.0.0
       that way using another switch does not require
       a change to the ocaml bazel_dep
    */
    fprintf(ostream, "module(\n");
    fprintf(ostream, "    name = \"%s\", version = \"%s\",\n",
            pkg_name,
            default_version
            );
    fprintf(ostream, "    compatibility_level = %d,\n",
            default_compat);
    fprintf(ostream, ")\n");
    fprintf(ostream, "\n");
    if (strncmp("ocaml", pkg_name, 6) == 0) {
        fprintf(ostream,
                "bazel_dep(name = \"platforms\", version = \"%s\")\n", platforms_version);
        fprintf(ostream,
                "bazel_dep(name = \"rules_ocaml\", version = \"%s\")\n", rules_ocaml_version);

        fprintf(ostream,
                "bazel_dep(name = \"stublibs\", version = \"%s\")\n",
                default_version);
    }

    fprintf(ostream, "\n");
    /* } */
    fclose(ostream);
    if (verbosity > log_writes)
        fprintf(INFOFD, GRN "INFO" CRESET " wrote %s\n", utstring_body(reg_file));
}

/**
   emit:
       <reg>/modules/<module>/metadata.json
       <reg>/modules/<module>/<version>/MODULE.bazel
       <reg>/modules/<module>/<version>/source.json
       <reg>/modules/<module>/<version>/patches ???
 */
EXPORT void emit_registry_record(UT_string *registry,
                                 char *compiler_version,
                                 struct obzl_meta_package *pkg,
                                 struct obzl_meta_package *pkgs)
{
    TRACE_ENTRY;
    char *pkg_name;
    char version[256];
    semver_t *semv;
    if (pkg) {
        pkg_name = pkg->module_name;
        semv = findlib_pkg_version(pkg);
        sprintf(version, "%d.%d.%d",
                semv->major, semv->minor, semv->patch);
        _emit_registry_record(registry,
                              compiler_version,
                              pkg, pkgs,
                              pkg_name,
                              version); //, semv->major);
    } else {
        _emit_registry_record(registry,
                              compiler_version,
                              pkg, pkgs,
                              "ocaml", "0.0.0");
        _emit_registry_record(registry,
                              compiler_version,
                              pkg, pkgs,
                              "stublibs", "0.0.0");
        _emit_registry_record(registry,
                              compiler_version,
                              pkg, pkgs,
                              "compiler-libs", "0.0.0");
    }
    TRACE_EXIT;
}

EXPORT void _emit_registry_record(UT_string *registry,
                                  char *compiler_version,
                                  struct obzl_meta_package *pkg,
                                  struct obzl_meta_package *pkgs,
                                  char *pkg_name,
                                  char *version)
{
    TRACE_ENTRY;
    UT_string *tmp;
    utstring_new(tmp);
    utstring_printf(tmp, "%s/modules/%s",
                    utstring_body(registry),
                    pkg_name);
    mkdir_r(utstring_body(tmp));

    UT_string *reg_dir;
    utstring_new(reg_dir);
    utstring_printf(reg_dir,
                    "%s/modules/%s",
                    utstring_body(registry),
                    pkg_name);
                    /* pkg_name); */
                    /* default_version); */
                    /* (char*)version); */
    mkdir_r(utstring_body(reg_dir));
    /* log_debug("regdir: %s", utstring_body(reg_dir)); */

    // modules/$MODULE/metadata.json
    /* UT_string *metadata_json_file; */
    UT_string *bazel_file;
    utstring_new(bazel_file);
    utstring_printf(bazel_file,
                    "%s/metadata.json",
                    utstring_body(reg_dir));
    /* if (verbose) */
        /* log_info("metadata.json: %s", */
        /*          utstring_body(bazel_file)); */

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
        /* log_info("metadata_json:\n%s", */
        /*          utstring_body(metadata_json)); */

    FILE *metadata_json_fd
        = fopen(utstring_body(bazel_file), "w");
    fprintf(metadata_json_fd,
            "%s", utstring_body(metadata_json));
    fclose (metadata_json_fd);
    if (verbosity > log_writes) {
        fprintf(INFOFD, GRN "INFO" CRESET " wrote %s\n", utstring_body(bazel_file));
        /* fflush(NULL); */
    }

    utstring_free(metadata_json);

    // modules/$MODULE/$VERSION/[MODULE.bazel, source.json]
    utstring_printf(reg_dir, "/%s", default_version); //version);
    mkdir_r(utstring_body(reg_dir));
    /* log_debug("regdir: %s", utstring_body(reg_dir)); */

    UT_string *reg_file;
    utstring_new(reg_file);
    utstring_printf(reg_file,
                    "%s/MODULE.bazel",
                    utstring_body(reg_dir));
    /* log_info("reg MODULE.bazel: %s", */
    /*          utstring_body(reg_file)); */

    /* if ( (strncmp("ocaml", pkg_name, 6) == 0) */
    /*      || (strncmp("stublibs", pkg_name, 8) == 0)) { */
    if (pkg == NULL) {
        // emit lib/ocaml and lib/stublibs
        _emit_reg_rec(reg_file, pkg_name);
    } else {
        emit_module_file(reg_file, compiler_version, pkg, pkgs);
    }
    // JUST FOR DEBUGGING:
#if defined(DEVBUILD)
    if (pkg) {
        if (pkg->metafile) {
            utstring_new(reg_file);
            utstring_printf(reg_file,
                            "%s/META",
                            utstring_body(reg_dir));
            /* log_info("reg META: %s", */
            /*          utstring_body(reg_file)); */

            copy_buildfile(pkg->metafile, reg_file);
        }
    }
#endif

    utstring_renew(reg_file);
    utstring_printf(reg_file,
                    "%s/source.json",
                    utstring_body(reg_dir));
    /* log_info("reg source.json : %s", */
    /*          utstring_body(reg_file)); */

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
                    /* pkg_name); */
    /* if (verbose) { */
    /*     log_info("source_json:\n%s", */
    /*              utstring_body(source_json)); */
    /*     log_info("regfile: %s", utstring_body(reg_file)); */
    /* } */
    FILE *source_json_fd
        = fopen(utstring_body(reg_file), "w");
    fprintf(source_json_fd,
            "%s", utstring_body(source_json));
    fclose (source_json_fd);
    if (verbosity > log_writes) {
        fprintf(INFOFD, GRN "INFO" CRESET " wrote %s\n", utstring_body(reg_file));
    }

    utstring_free(source_json);

    utstring_free(reg_file);
    utstring_free(bazel_file);
    TRACE_EXIT;
}
