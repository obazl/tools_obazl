#if EXPORT_INTERFACE
#include "utarray.h"
#endif
#include "log.h"
#include "api.h"

/* EXPORT obzl_meta_package_s *obzl_meta_package_new() */
/* { */
/*     return (obzl_meta_package*)calloc(sizeof(obzl_meta_package), 1); */
/* } */

/* **************************************************************** */
EXPORT int obzl_meta_entries_count(obzl_meta_entries *_entries)
{
    return utarray_len(_entries->list);
}

/* EXPORT obzl_meta_entry *obzl_meta_entries_nth(obzl_meta_entries *_entries, int _i) */
/* { */
/*     return utarray_eltptr(_entries->list, _i); */
/* } */

/* **************************************************************** */
EXPORT obzl_meta_entry *obzl_meta_entry_new()
{
    return (obzl_meta_entry*)calloc(sizeof(obzl_meta_entry), 1);
}

EXPORT enum obzl_meta_entry_type_e obzl_meta_entry_type(obzl_meta_entry *e)
{
    return e->type;
}

EXPORT obzl_meta_property *obzl_meta_entry_property(obzl_meta_entry *e)
{
    return e->property;
}

EXPORT obzl_meta_package *obzl_meta_entry_package(obzl_meta_entry *e)
{
    return e->package;
}

/* **************************************************************** */
EXPORT int obzl_meta_flags_count(obzl_meta_flags *_flags)
{
    return utarray_len(_flags->list);
}

EXPORT obzl_meta_flag *obzl_meta_flags_nth(obzl_meta_flags *_flags, int _i)
{
    return utarray_eltptr(_flags->list, _i);
}

/* **************************************************************** */
EXPORT char *obzl_meta_flag_name(obzl_meta_flag *flag)
{
    return flag->s;
}

EXPORT bool obzl_meta_flag_polarity(obzl_meta_flag *flag)
{
    return flag->polarity;
}

