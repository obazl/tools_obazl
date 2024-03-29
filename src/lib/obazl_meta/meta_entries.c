#include <stdbool.h>
#include "utarray.h"

#include <stdio.h>
#include "uthash.h"

#include "log.h"
#include "meta_entries.h"

LOCAL int indent = 2;
LOCAL int delta = 2;
LOCAL char *sp = " ";


/* **************************************************************** */
/* ****************
   struct obazl_meta_entry_s
   **************** */
#if INTERFACE
enum obazl_meta_entry_type_e { OMP_PROPERTY, OMP_PACKAGE };
struct obazl_meta_entry {
    enum obazl_meta_entry_type_e type;
    union {
        struct obazl_meta_property *property;
        struct obazl_meta_package  *package;
    };
};

struct obazl_meta_entries {
    UT_array *list;          /* list of obazl_meta_entry */
};
#endif

EXPORT UT_icd entry_icd = {sizeof(struct obazl_meta_entry), NULL, entry_copy, entry_dtor};

EXPORT obazl_meta_entry *obazl_meta_entries_nth(obazl_meta_entries *_entries, int _i)
{
    if (_i < 0) {
        log_error("Index < min (1)");
    } else {
        if (_i > utarray_len(_entries->list)) {
            log_error("Index > max (%d)", utarray_len(_entries->list));
        } else {
            return utarray_eltptr(_entries->list, _i);
        }
    }
    /* FIXME: set errno, return NULL? */
    exit(EXIT_FAILURE);
}

void entry_copy(void *_dst, const void *_src) {
    struct obazl_meta_entry *dst = (struct obazl_meta_entry*)_dst;
    struct obazl_meta_entry *src = (struct obazl_meta_entry*)_src;
    dst->type = src->type;
    dst->property = src->property;
    dst->package = src->package;
}

void entry_dtor(void *_elt) {
    /* struct obazl_meta_entry *elt = (struct obazl_meta_entry*)_elt; */
    /* if (elt->s) free(elt->s); */
}

/* **************************************************************** */
void normalize_entries(obazl_meta_entries *entries, obazl_meta_entry *_entry)
{
#if DEBUG_TRACE
    /* log_trace("normalize_entries()"); */
    /* if (_entry->type == OMP_PROPERTY) { */
    /*     log_trace("new entry type: property"); */
    /*     log_trace("\tname: %s", _entry->property->name); */
    /* } else { */
    /*     log_trace("new entry type: package"); */
    /* } */
#endif

    int ct = obazl_meta_entries_count(entries);
    /* log_trace("entries ct: %d", ct); */
    obazl_meta_entry *e = NULL;

    bool matched = false;
    for (int i = 0; i < ct; i++) {
        e = obazl_meta_entries_nth(entries, i);

        if (e->type == _entry->type) {
            /* log_trace("old entry type: property"); */
            /* log_trace("\tname: %s", e->property->name); */
            if (e->type == OMP_PROPERTY) {
                if (strncmp(e->property->name, _entry->property->name, 32) == 0) {
                    /* log_trace("match"); */
                    matched = true;
                    /* update the entry  */
                    /* obazl_meta_settings *settings = e->property->settings; */
                    /* obazl_meta_settings *new_settings = _entry->property->settings; */
                    utarray_concat(e->property->settings->list, _entry->property->settings->list);
                }
            }
        /* } else { */
            /* log_trace("old entry type: package"); */
        }
    }
    if ( !matched ) {
        utarray_push_back(entries->list, _entry);
    }
}

/* **************************************************************** */
#if DEBUG_TRACE
void dump_entry(int indent, struct obazl_meta_entry *entry)
{
    /* log_trace("%*sdump_entry:", indent, sp); */
    log_debug("%*sentry type: %d", delta+indent, sp, entry->type);
    if (entry->type == OMP_PROPERTY) {
        dump_property(delta+indent, entry->property);
    } else {
        dump_package(delta+indent, entry->package);
    }
}

void dump_entries(int indent, struct obazl_meta_entries *entries)
{
    /* log_trace("%*sdump_entries() %p", indent, sp, entries); */
    if (entries == NULL) {
        log_trace("%*sentries: none", indent, sp);
    } else {
        obazl_meta_entry *e = NULL;
        for (int i = 0; i < obazl_meta_entries_count(entries); i++) {
            e = obazl_meta_entries_nth(entries, i);
            /* log_trace("e: %p", e); */
            /* log_trace("e type: %d", e->type); */
            dump_entry(delta+indent, e);
        }
        /* log_trace("%*sdump_entries() DONE", indent, sp); */
    }
}
#endif

