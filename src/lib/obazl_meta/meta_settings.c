#include <stdbool.h>
#include <stdio.h>
#include "utarray.h"
#include "uthash.h"

#include "log.h"
#include "meta_settings.h"

static int indent = 2;
static int delta = 2;
static char *sp = " ";


/* **************************************************************** */
EXPORT int obazl_meta_settings_count(obazl_meta_settings *_settings)
{
    return utarray_len(_settings->list);
}

EXPORT obazl_meta_setting *obazl_meta_settings_nth(obazl_meta_settings *_settings, int _i)
{
    return utarray_eltptr(_settings->list, _i);
}

EXPORT obazl_meta_flags *obazl_meta_setting_flags(obazl_meta_setting *_setting)
{
    return _setting->flags;
}

EXPORT enum obazl_meta_opcode_e *obazl_meta_setting_opcode(obazl_meta_setting *_setting)
{
    return _setting->opcode;
}

EXPORT obazl_meta_values *obazl_meta_setting_values(obazl_meta_setting *_setting)
{
    return _setting->values;
}

#if INTERFACE
enum obazl_meta_opcode_e { OP_SET, OP_UPDATE };
struct obazl_meta_setting {
    obazl_meta_flags *flags;
    /* UT_array *flags;       /\* array of struct obazl_meta_flag *\/ */
    enum obazl_meta_opcode_e opcode;
    obazl_meta_values *values;
    /* UT_array *values;            /\* array of strings *\/ */
};

struct obazl_meta_settings {
    UT_array *list;             /* list of obazl_meta_setting* */
};
#endif

UT_icd obazl_meta_setting_icd = {
    sizeof(obazl_meta_setting),
    NULL, obazl_meta_setting_copy, obazl_meta_setting_dtor
};

struct obazl_meta_setting *obazl_meta_setting_new(char *flags,
                                                enum obazl_meta_opcode_e opcode,
                                                obazl_meta_values *values)
{
#if DEBUG_TRACE
    /* log_trace("obazl_meta_setting_new()"); //, flags: %s", flags); */
#endif
    struct obazl_meta_setting *new_setting = (struct obazl_meta_setting*)malloc(sizeof(struct obazl_meta_setting));
    if (flags == NULL)
        new_setting->flags  = NULL;
    else
        new_setting->flags  = obazl_meta_flags_new_tokenized(flags);
    new_setting->opcode = opcode;
    new_setting->values = values;
#if DEBUG_TRACE
    log_trace("obazl_meta_setting_new done; dumping:");
    dump_setting(0, new_setting);
#endif
    return new_setting;
}

struct obazl_meta_settings *obazl_meta_settings_new()
{
#if DEBUG_TRACE
    /* log_trace("obazl_meta_settings_new()"); */
#endif
    struct obazl_meta_settings *new_settings = (struct obazl_meta_settings*)malloc(sizeof(struct obazl_meta_settings));
    utarray_new(new_settings->list, &obazl_meta_setting_icd);
#if DEBUG_TRACE
    /* dump_settings(0, new_settings); */
    /* log_trace("obazl_meta_settings_new() done"); */
#endif
    return new_settings;
}

void obazl_meta_setting_copy(obazl_meta_setting *dst, const obazl_meta_setting *src) {
#if DEBUG_TRACE
    log_trace("obazl_meta_setting_copy(dst=%p,  src=%p)", dst, src);
#endif
    /* struct obazl_meta_setting *dst = (struct obazl_meta_setting*)_dst; */
    /* struct obazl_meta_setting *src = (struct obazl_meta_setting*)_src; */

    /* dump_setting(0, src); */

    /* dst->flags = obazl_meta_flags_new(); */
    if (src->flags != NULL) {
        dst->flags  = obazl_meta_flags_new_copy(src->flags);
    } else {
        dst->flags = NULL;
    }

    dst->opcode = (enum obazl_meta_opcode_e)src->opcode;

    if ( src->values != NULL) {
        dst->values = obazl_meta_values_new_copy(src->values);
    } else {
        dst->values == NULL;
        /* utarray_new(dst->values->list, &ut_str_icd); */
        /* char **old_str = NULL; */
        /* while ( (old_str=(char **)utarray_next(src->values->list, old_str))) { */
        /*     /\* log_trace("copying value: %s", *old_str); *\/ */
        /*     utarray_push_back(dst->values->list, old_str); */
        /* } */
    /* } else { */
    /*     log_trace("no vals..."); */
    }
}

void obazl_meta_setting_dtor(void *_elt) {
    struct obazl_meta_setting *elt = (struct obazl_meta_setting*)_elt;
    flags_dtor(elt->flags);
}

void obazl_meta_settings_dtor(void *_elt) {
    struct obazl_meta_settings *elt = (struct obazl_meta_settings*)_elt;
    obazl_meta_setting *setting = NULL;
    while ( (setting=(obazl_meta_setting*)utarray_next(elt->list, setting))) {
        obazl_meta_setting_dtor(setting);
    }
    free(elt);
}

#if DEBUG_TRACE
void dump_setting(int indent, struct obazl_meta_setting *setting)
{
    log_trace("%*ssetting:", indent, sp);
    dump_flags(2*delta+indent, setting->flags);
    log_debug("%*sopcode: %d", delta+indent, sp, setting->opcode);
    dump_values(2*delta+indent, setting->values);
    /* log_trace("%*sdump_setting() finished", indent, sp); */
}
#endif

#if DEBUG_TRACE
void dump_settings(int indent, obazl_meta_settings *settings)
{
    log_trace("%*ssettings:", indent, sp);
    obazl_meta_setting *setting = NULL;
    for(setting  = utarray_front(settings->list);
        setting != NULL;
        setting  = utarray_next(settings->list, setting)) {
        dump_setting(delta+indent, setting);
    }
}
#endif
