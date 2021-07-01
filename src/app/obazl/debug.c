/* FIXME: remove dependency on utarray.h */
#if INTERFACE
#include "utarray.h"
#endif

#include "log.h"

#include "debug.h"

static int indent = 2;
static int delta = 2;
static char *sp = " ";

//FIXME: rename dump_meta_pkg
void dump_pkg(int indent, struct obazl_meta_package *_pkg)
{
    /* log_set_quiet(false); */
    log_debug("%*sname:      %s", indent, sp, obazl_meta_package_name(_pkg));
    log_debug("%*sdirectory: %s", indent, sp, obazl_meta_package_dir(_pkg));
    log_debug("%*smetafile:  %s", indent, sp, obazl_meta_package_src(_pkg));

    /* UT_array *entries = obazl_meta_package_entries(_pkg); */
    obazl_meta_entries *entries = obazl_meta_package_entries(_pkg);
    log_debug("%*sentries:", indent, sp);
    /* void *e = NULL; */
    /* for(e  = utarray_front((entries->list)); */
    /*     e != NULL; */
    /*     e  = utarray_next((entries->list), e)) { */

    /* for(e  = obazl_meta_entries_front(entries); */
    /*     e != NULL; */
    /*     e  = obazl_meta_entries_next(entries, e)) { */

    obazl_meta_entry *e = NULL;
    for (int i = 0; i < obazl_meta_entries_count(entries); i++) {
        e = obazl_meta_entries_nth(entries, i);

        if (obazl_meta_entry_type(e) == OMP_PROPERTY) {
            log_debug("%*sproperty:", delta+indent, sp);

            obazl_meta_property *prop = obazl_meta_entry_property(e);
            /* void *prop = obazl_meta_entry_property(e); */
            log_debug("%*sname: %s", delta+delta+indent, sp,
                      obazl_meta_property_name(prop));
            // or:  obazl_meta_entry_name(prop));

            log_debug("%*ssettings:", 2*delta+indent, sp);

            obazl_meta_settings *settings = obazl_meta_property_settings(prop);
            obazl_meta_setting *setting = NULL;
            for (int i = 0; i < obazl_meta_settings_count(settings); i++) {
                setting = obazl_meta_settings_nth(settings, i);

            /* for(setting  = utarray_front(settings->list); */
            /*     setting != NULL; */
            /*     setting  = utarray_next(settings->list, setting)) { */

                log_debug("%*ssetting:", 3*delta+indent, sp);

                obazl_meta_flags *flags = obazl_meta_setting_flags(setting);
                if (flags == NULL) {
                    log_debug("%*sflags: none", 4*delta+indent, sp);
                } else {
                    log_debug("%*sflags:", 4*delta+indent, sp); // , utarray_len(flags));
                    obazl_meta_flag *flag = NULL;
                    for (int i = 0; i < obazl_meta_flags_count(flags); i++) {
                        flag = obazl_meta_flags_nth(flags, i);

                    /* for(flag  = utarray_front(flags->list); */
                    /*     flag != NULL; */
                    /*     flag  = utarray_next(flags->list, flag)) { */

                        log_debug("%*s%s%s", 5*delta+indent, sp,
                                  (obazl_meta_flag_polarity(flag) == 1) ? "+" : "-",
                                  obazl_meta_flag_name(flag)
                                  );
                    }
                }

                log_debug("%*sopcode: %d", 4*delta+indent, sp, obazl_meta_setting_opcode(setting));

                obazl_meta_values *vals = obazl_meta_setting_values(setting);
                if (obazl_meta_values_count(vals) == 0) {
                    log_debug("%*svalues: none", 2*delta+indent, sp);
                } else {
                    log_debug("%*svalues:", 4*delta+indent, sp);
                    char **val = NULL;
                    for (int i = 0; i < obazl_meta_values_count(vals); i++) {
                        val = obazl_meta_values_nth(vals, i);
                        log_debug("%*s'%s'", 5*delta+indent, sp, *val);

                    /* while ((val=(char **)utarray_next(vals, val))) { */
                    /*     log_debug("%*s'%s'", 4*delta+indent, sp, *val); */
                    /* } */
                    }
                }
            }
        } else {
            log_debug("%*spackage:", delta+indent, sp);
            obazl_meta_package *pkg = obazl_meta_entry_package(e);
            /* void *pkg = obazl_meta_entry_package(e); */
            /* log_debug("%*spackage name:      %s", delta+delta+indent, sp, obazl_meta_package_name(pkg)); */
            /* log_debug("%*spackage directory: %s", delta+delta+indent, sp, obazl_meta_package_directory(pkg)); */
            /* log_debug("%*spackage metafile:  %s", delta+delta+indent, sp, obazl_meta_package_src(pkg)); */
            dump_pkg(delta+delta+indent, pkg);
        }
    }
    /* dump_package(0, ast); */
}

void dump_dune_pkg(int indent, struct obazl_dune_package_s *pkg)
{
    log_debug("dump_dune_pkg: %s", pkg->path);
    log_debug("stanza ct: %d", stanzas_len(pkg->stanzas));

    struct stanza_field_s *field, *tmpfield;
    char **s;

    struct stanza_s *stanza, **iter=NULL;
    while( (iter=(struct stanza_s**)utarray_next(pkg->stanzas, iter))) {
        stanza = *iter;
        log_debug("stanza type: %d", stanza->type);

        struct stanza_field_s *fields = stanza->fields;
        log_debug("fld ct: %d", HASH_CNT(hh, fields)); //fields_len(fields));
        for (field = fields; field != NULL; field = field->hh.next) {
            log_debug("fld type: %d", field->type);
            switch(field->type) {
            case FIELD_NAME:
                log_debug("\tNAME: %s", field->name);
                log_debug("\t  %s", field->rawstr);
                break;
            case FIELD_PUBLIC_NAME:
                log_debug("\tPUBLIC_NAME: %s", field->name);
                log_debug("\t  %s", field->rawstr);
                break;
            case FIELD_LIBRARIES:
                log_debug("\tLIBARIES:");
                log_debug("\t  %s", field->rawstr);
                s = NULL;
                while ( (s=(char**)utarray_next(field->libraries, s))) {
                    log_debug("\t\t%s",*s);
                }
                break;
            case FIELD_MODULES:
                log_debug("\tMODULES:");
                log_debug("\t  %s", field->rawstr);
                s = NULL;
                if (field->modules) {
                    /* while ( (s=(char**)utarray_next(field->modules->fileseq, s))) { */
                    /*     log_debug("\t\tfile: %s",*s); */
                    /* } */
                    while ( (s=(char**)utarray_next(field->modules->include, s))) {
                        log_debug("\t\tinclude: %s",*s);
                    }
                    while ( (s=(char**)utarray_next(field->modules->exclude, s))) {
                        log_debug("\t\texclude: %s",*s);
                    }
                } else {
                    log_debug("\t\t<null>");
                }
                break;
            case FIELD_PREPROCESS:
                log_debug("\tPREPROCESS:");
                log_debug("\t  %s", field->rawstr);
                /* s = NULL; */
                /* if (field->modules) { */
                /*     while ( (s=(char**)utarray_next(field->modules, s))) { */
                /*         log_debug("\t\t%s",*s); */
                /*     } */
                /* } else { */
                /*     log_debug("\t\t<null>"); */
                /* } */
                break;
            }
        }
    }
}

