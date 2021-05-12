#include "log.h"
#include "utarray.h"

#include "debug.h"

void dump_dune_pkg(struct obzl_dune_package_s *pkg)
{
    struct stanza_field_s **p=NULL;
    struct stanza_field_s *item, *tmp;
    char **s;
    while( (p=(struct stanza_field_s**)utarray_next(pkg->stanzas, p))) {
        log_debug("stanza:");
        for (item = *p; item != NULL; item = (struct stanza_field_s*)(item->hh.next)) {
            switch(item->type) {
            case FIELD_NAME:
                log_debug("\tNAME: %s", item->name);
                log_debug("\t  %s", item->rawstr);
                break;
            case FIELD_PUBLIC_NAME:
                log_debug("\tPUBLIC_NAME: %s", item->name);
                log_debug("\t  %s", item->rawstr);
                break;
            case FIELD_LIBRARIES:
                log_debug("\tLIBARIES:");
                log_debug("\t  %s", item->rawstr);
                s = NULL;
                while ( (s=(char**)utarray_next(item->libraries, s))) {
                    log_debug("\t\t%s",*s);
                }
                break;
            case FIELD_MODULES:
                log_debug("\tMODULES:");
                log_debug("\t  %s", item->rawstr);
                s = NULL;
                if (item->modules) {
                    /* while ( (s=(char**)utarray_next(item->modules->fileseq, s))) { */
                    /*     log_debug("\t\tfile: %s",*s); */
                    /* } */
                    while ( (s=(char**)utarray_next(item->modules->include, s))) {
                        log_debug("\t\tinclude: %s",*s);
                    }
                    while ( (s=(char**)utarray_next(item->modules->exclude, s))) {
                        log_debug("\t\texclude: %s",*s);
                    }
                } else {
                    log_debug("\t\t<null>");
                }
                break;
            case FIELD_PREPROCESS:
                log_debug("\tPREPROCESS:");
                log_debug("\t  %s", item->rawstr);
                /* s = NULL; */
                /* if (item->modules) { */
                /*     while ( (s=(char**)utarray_next(item->modules, s))) { */
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
