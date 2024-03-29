#include <stdbool.h>
#include <stdio.h>
#include "utarray.h"
#include "uthash.h"

#include "log.h"
#include "meta_values.h"

static int indent = 2;
static int delta = 2;
static char *sp = " ";

/* **************************************************************** */
EXPORT int obazl_meta_values_count(obazl_meta_values *_values)
{
    return utarray_len(_values->list);
}

EXPORT obazl_meta_value *obazl_meta_values_nth(obazl_meta_values *_values, int _i)
{
    return utarray_eltptr(_values->list, _i);
}

#if INTERFACE
typedef char *obazl_meta_value;

struct obazl_meta_values {
    UT_array *list;             /* list of ptrs to char ptrs  */
};
#endif

EXPORT obazl_meta_values *obazl_meta_values_new(char *valstr)
{
#if DEBUG_TRACE
    /* log_trace("obazl_meta_values_new(%s)", valstr); */
#endif
    obazl_meta_values *new_values = (obazl_meta_values*)calloc(sizeof(obazl_meta_values),1);
    utarray_new(new_values->list, &ut_str_icd);
    if (valstr != NULL)
        utarray_push_back(new_values->list, &valstr);
    /* dump_values(0, new_values); */
    return new_values;
}

EXPORT obazl_meta_values *obazl_meta_values_new_copy(obazl_meta_values *values)
{
#if DEBUG_TRACE
    log_trace("obazl_meta_values_new_copy");
#endif
    obazl_meta_values *new_values = (obazl_meta_values*)calloc(sizeof(obazl_meta_values),1);
    utarray_new(new_values->list, &ut_str_icd);
    utarray_concat(new_values->list, values->list);
    /* dump_values(0, new_values); */
    return new_values;
}

/* EXPORT UT_array *obazl_meta_values_new_tokenized(char *valstr) */
EXPORT obazl_meta_values *obazl_meta_values_new_tokenized(char *valstr)
{
#if DEBUG_TRACE
    log_trace("obazl_meta_values_new_tokenized");
#endif
    /* UT_array *new_values; */
    obazl_meta_values *new_values = (obazl_meta_values*)calloc(sizeof(obazl_meta_values), 1);
    utarray_new(new_values->list, &ut_str_icd);
    char *token, *sep = " ,\n";
    token = strtok(valstr, sep);
    while( token != NULL ) {
#if DEBUG_TRACE
        log_trace("pushing val %s", token);
#endif
        utarray_push_back(new_values->list, &token);
        token = strtok(NULL, sep);
    }
    return new_values;
}

/* **************************************************************** */
#if DEBUG_TRACE
EXPORT void dump_values(int indent, obazl_meta_values *values)
{
    /* log_trace("dump_values %p", values); */
    indent++;            /* account for width of log label */
    if ( values->list ) {
        if (utarray_len(values->list) == 0) {
            log_debug("%*svalues: none", indent, sp);
        } else {
            char **a_value = NULL;
            log_trace("%*svalues:", indent, sp);
            while ( (a_value=(char **)utarray_next(values->list, a_value))) {
                log_trace("%*s'%s'", delta+indent, sp, *a_value);
            }
        }
    } else {
        log_debug("%*svalues: none", indent, sp);
    }
}
#endif
