#if EXPORT_INTERFACE
#include "utarray.h"
#endif
#include "log.h"
#include "api.h"

/* EXPORT obazl_meta_package_s *obazl_meta_package_new() */
/* { */
/*     return (obazl_meta_package*)calloc(sizeof(obazl_meta_package), 1); */
/* } */

/* **************************************************************** */
EXPORT int obazl_meta_entries_count(obazl_meta_entries *_entries)
{
    return utarray_len(_entries->list);
}

/* EXPORT obazl_meta_entry *obazl_meta_entries_nth(obazl_meta_entries *_entries, int _i) */
/* { */
/*     return utarray_eltptr(_entries->list, _i); */
/* } */

/* **************************************************************** */
EXPORT obazl_meta_entry *obazl_meta_entry_new()
{
    return (obazl_meta_entry*)calloc(sizeof(obazl_meta_entry), 1);
}

EXPORT enum obazl_meta_entry_type_e obazl_meta_entry_type(obazl_meta_entry *e)
{
    return e->type;
}

EXPORT obazl_meta_property *obazl_meta_entry_property(obazl_meta_entry *e)
{
    return e->property;
}

EXPORT obazl_meta_package *obazl_meta_entry_package(obazl_meta_entry *e)
{
    return e->package;
}

/* **************************************************************** */
EXPORT int obazl_meta_flags_count(obazl_meta_flags *_flags)
{
    return utarray_len(_flags->list);
}

EXPORT obazl_meta_flag *obazl_meta_flags_nth(obazl_meta_flags *_flags, int _i)
{
    return utarray_eltptr(_flags->list, _i);
}

/* **************************************************************** */
EXPORT char *obazl_meta_flag_name(obazl_meta_flag *flag)
{
    return flag->s;
}

EXPORT bool obazl_meta_flag_polarity(obazl_meta_flag *flag)
{
    return flag->polarity;
}

