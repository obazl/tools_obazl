#include <stdbool.h>
#include <stdio.h>
#include "utarray.h"
#include "uthash.h"

#include "log.h"
#include "meta_properties.h"

static int indent = 2;
static int delta = 2;
static char *sp = " ";


/* **************************************************************** */
EXPORT char *obazl_meta_property_name(obazl_meta_property *prop)
{
    return prop->name;
}


EXPORT obazl_meta_settings *obazl_meta_property_settings(obazl_meta_property *prop)
{
    return prop->settings;
}

#if INTERFACE
struct obazl_meta_property {
    char     *name;
    /* UT_array *settings;         /\* array of struct obazl_meta_setting *\/ */
    obazl_meta_settings *settings;         /* array of struct obazl_meta_setting */
};
#endif

/* **************************************************************** */
UT_icd property_icd = {sizeof(struct obazl_meta_property), NULL, property_copy, property_dtor};

struct obazl_meta_property *obazl_meta_property_new(char *name)
{
#if DEBUG_TRACE
    /* log_trace("obazl_meta_property_new(%s)", name); */
#endif
    struct obazl_meta_property *new_prop= (struct obazl_meta_property*)malloc(sizeof(struct obazl_meta_property));
    new_prop->name = name;
    new_prop->settings = obazl_meta_settings_new();
#if DEBUG_TRACE
    /* log_trace("obazl_meta_property_new(%s) done", name); */
#endif
    return new_prop;
}

void property_copy(void *_dst, const void *_src) {
#if DEBUG_TRACE
    /* log_debug("property_copy"); */
#endif
    struct obazl_meta_property *src = (struct obazl_meta_property*)_src;
    struct obazl_meta_property *dst = (struct obazl_meta_property*)_dst;

    /* *dst is allocated but not initialized by utarray_new */
    dst->name = src->name ? strdup(src->name) : NULL;
    if (src->settings != NULL) {
        /* struct obazl_meta_setting *new_setting = (struct obazl_meta_setting*)calloc(sizeof(struct obazl_meta_setting), 1); */
        /* setting_copy(new_setting, src->setting); */
        /* dst->setting = new_setting; */
        utarray_new(dst->settings->list, &obazl_meta_setting_icd);
        utarray_concat(dst->settings->list, src->settings->list); /* copies src to dst */
    }
}

void property_dtor(void *_elt) {
    log_trace("property_dtor");
    if (((obazl_meta_property*)_elt)->name) free(((obazl_meta_property*)_elt)->name);
    /* utarray_free(((obazl_meta_property*)_elt)->settings); */
    obazl_meta_settings_dtor(((obazl_meta_property*)_elt)->settings);
    /* setting_dtor(elt->setting); */
    free((obazl_meta_property*)_elt);
}

/* struct obazl_meta_property *handle_compound_prop(union meta_token *token, */
/*                                         UT_array *flags, /\* list struct obazl_meta_flag *\/ */
/*                                         enum obazl_meta_opcode_e opcode, */
/*                                         UT_array *words) /\* list of strings *\/ */
/* { */
/*     log_trace(">>handle_compound_prop"); */
/*     log_trace("\ttoken: %s", token->s); */
/*     log_trace("\topcode: %d", opcode); */

/*     struct obazl_meta_property *new_prop= (struct obazl_meta_property*)malloc(sizeof(struct obazl_meta_property)); */
/*     new_prop->name = strdup(token->s); */
/*     new_prop->setting = (struct obazl_meta_setting*)malloc(sizeof(struct obazl_meta_setting)); */
/*     new_prop->setting->flags = flags_new_copy(flags); */
/*     /\* dump_flags(indent, new_prop->setting->flags); *\/ */
/*     new_prop->setting->opcode = opcode; */
/*     new_prop->setting->values = words; */
/*     /\* dump_values(indent, new_prop->setting->values); *\/ */

/*     return new_prop; */
/* } */

/* **************************************************************** */
EXPORT struct obazl_meta_entry *handle_primitive_prop(int token_type, union meta_token *token)
{
#if DEBUG_TRACE
    log_trace("%*shandle_primitive_prop", indent, sp);
    log_trace("%*stoken type: %d: %s", indent, sp, token_type, token_names[token_type]);
    log_trace("%*stoken str:  %s", indent, sp, token->s);
#endif
    char *n;
    if (token_names[token_type])
        n = strdup(token_names[token_type]);
    else {
        log_error("Parse ERROR: token type name not found: %d", token_type);
        exit(EXIT_FAILURE);
    }
    struct obazl_meta_property *new_prop= obazl_meta_property_new(n);

    /* In the case of "primitive", the token contains the value, e.g. VERSION = '1.2.3' */
    obazl_meta_values *values = obazl_meta_values_new(token->s);

    struct obazl_meta_setting *new_setting = obazl_meta_setting_new(NULL, OP_SET, values);

#if DEBUG_TRACE
    log_trace("PRIM DUMPING NEW SETTING");
    dump_setting(0, new_setting);
    log_trace("                PRIM PUSHING NEW SETTING");
#endif

    utarray_push_back(new_prop->settings->list, new_setting);

#if DEBUG_TRACE
    log_trace("PRIM DUMPING NEW SETTING AGAIN");
    dump_setting(0, new_setting);
    log_trace("                PRIM DUMPING NEW PROPERTY");
    dump_property(0, new_prop);
#endif

    struct obazl_meta_entry* new_entry = (struct obazl_meta_entry*)calloc(sizeof(struct obazl_meta_entry), 1);
    new_entry->type = OMP_PROPERTY;
    new_entry->property = new_prop;

    return new_entry;
}

EXPORT struct obazl_meta_entry *handle_simple_prop(union meta_token *token,
                                              enum obazl_meta_opcode_e opcode,
                                              union meta_token *word)
{
#if DEBUG_TRACE
    log_trace(">>handle_simple_prop");
    log_trace("\tproperty ::= PWORD opcode WORD");
    log_trace("\ttoken: %s", token->s);
    log_trace("\topcode: %d", opcode);
    log_trace("\tword->s: %p", word->s);
    /* if (word->s) */
    /*     log_trace("\tword:  %s", word->s); */
    /* else */
    /*     log_trace("\tword:  "); */
#endif

    struct obazl_meta_property *new_prop= obazl_meta_property_new(token->s);

    obazl_meta_values *values = obazl_meta_values_new(word->s);

    struct obazl_meta_setting *new_setting = obazl_meta_setting_new(NULL, OP_SET, values);

    utarray_push_back(new_prop->settings->list, new_setting);

    struct obazl_meta_entry* new_entry = (struct obazl_meta_entry*)calloc(sizeof(struct obazl_meta_entry), 1);
    new_entry->type = OMP_PROPERTY;
    new_entry->property = new_prop;


    /* utarray_new(new_prop->settings->list, &obazl_meta_setting_icd); */

    /* struct obazl_meta_setting *new_setting = (struct obazl_meta_setting*)malloc(sizeof(struct obazl_meta_setting)); */
    /* new_setting->flags = NULL; */
    /* new_setting->opcode = opcode; */
    /* new_setting->values  = obazl_meta_values_new(word->s); */

    /* utarray_push_back(new_prop->settings->list, new_setting); */

    /* struct obazl_meta_entry* new_entry = (struct obazl_meta_entry*)malloc(sizeof(struct obazl_meta_entry)); */
    /* new_entry->type = OMP_PROPERTY; */
    /* new_entry->property = new_prop; */

    return new_entry;
}

/* **************************************************************** */

#if DEBUG_TRACE
void dump_property(int indent, struct obazl_meta_property *prop)
{
    /* log_trace("dump_property %p", prop); */
    log_debug("%*sproperty:", indent, sp);
    log_debug("%*sname: %s", delta+indent, sp, prop->name);
    dump_settings(delta+indent, prop->settings);
}

void dump_properties(int indent, UT_array *props)
{
    /* log_trace("dump_properties: %p", props); */
    struct obazl_meta_property *p = NULL;
    while ( (p=(struct obazl_meta_property *)utarray_next(props, p))) {
        dump_property(delta+indent, p);
    }
}
#endif
