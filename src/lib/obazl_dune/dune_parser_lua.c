#include "utarray.h"
#include "uthash.h"

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <libgen.h>
#include <stdio.h>
#ifndef WIN32
# include <unistd.h>
#else
# define ssize_t int
# include <io.h>
# include <sys/types.h>
#endif
#include <fcntl.h>
#include <stdlib.h>

#include <sexp.h>
#include <sexp_ops.h>
#include <string.h>

#include "log.h"

#include "dune_parser_lua.h"

/* hashmap array - list of UT_hash pointers, not UT_hash structs */

/* void hashmap_copy(void *_dst, const void *_src) { */
/*   UT_hash *dst = (UT_hash*)_dst, *src = (UT_hash*)_src; */

/*   dst->s = src->s ? strdup(src->s) : NULL; */
/* } */

/* void hashmap_dtor(void *_elt) { */
/*   intchar_t *elt = (intchar_t*)_elt; */
/*   if (elt->s) free(elt->s); */
/* } */

#if EXPORT_INTERFACE
#include "utarray.h"

/*
  lua package = {
      name = "pgkname",
      stanzas = {
          type   = styp, -- int code
          fields = {
              type = ftyp,  -- int code
              rawstr = "(...)", -- from src
              -- union type, depends on ftyp
              -- name: string, for name, public_name flds
              -- libraries: string array, for 'libraries' fld
              -- modules: 3 lists: include, exclude, resolved (for 'modules' fld)
              -- preproc: struct, for preprocessing, e.g. ppx
          }
      }
      files   = {
      }
  }
 */

struct obazl_dune_package_s {
    char *path;
    UT_array *stanzas;          /* array of struct stanza_s pointer pointers */
    UT_array *files;            /* ???? */
}

struct stanza_s {
    enum stanza_type_e type;    /* name field */
    struct stanza_field_s *fields; /* hashmap */
};

#include "uthash.h"
struct stanza_field_s {
    enum field_type_e type; /* key; assumption: no dup field types in a stanza */
    char *rawstr;           /* the original sexp */
    union {
        char *name;           /* types FIELD_NAME, FIELD_PUBLIC_NAME */
        UT_array *libraries;  /* type FIELD_LIBRARIES; string array */
        modules_s *modules;    /* type FIELD_MODULES; string array */
        struct pp_s *preproc; /* special treatment for (preprocess ...) */
    };
    UT_hash_handle hh;         /* makes this structure hashable */
};

struct modules_s {
    /* wordlists */
    UT_array *resolved;
    /* UT_array *fileseq; */
    UT_array *include;
    UT_array *exclude;
};

#endif

/* char *stanza_name(enum stanza_type_e type) */
/* { */
/* } */

/* must not be in EXPORT_INTERFACE */
UT_icd stanza_list_icd = {sizeof(struct stanza_s*), NULL, NULL, NULL};

/* UT_array *stanza_list;              /\* array of stanza hashmap ptrs; initialized in parse_dunefile *\/ */

/* bool stanza_contains_field_type(struct stanza_field_s *stanza) */
/* { */
/*     /\* HASH_FIND only works for keys, so we have to interate the whole thing. *\/ */
/*     return false; */
/* } */

struct stanza_field_s* stanza_field_new(struct stanza_field_s *stanzas)
{
    stanza_field_s *field = (struct stanza_field_s*)calloc(sizeof(struct stanza_field_s), 1);
    /* int ct = HASH_CNT(hh, stanzas); */
    /* field->id = ct; */
    return field;
}

EXPORT int stanzas_len(UT_array *stanzas)
{
    return utarray_len(stanzas);
}

EXPORT int fields_len(struct stanza_field_s *fields)
{
    return HASH_CNT(hh, fields);
}

/* ************************** */
void handle_library(sexp_t *exp, obazl_dune_package_s *pkg)
{
    log_trace("handle_library");

    /* char *v; */
    sexp_t *tmpsx; //, *tmpsx2;

    struct stanza_s *stanza = calloc(sizeof(struct stanza_s), 1);
    stanza->type = STANZA_LIBRARY;

    log_debug("\tcdring over library sexp");
    tmpsx = exp->list->next;
    while(tmpsx) {
        dune_handle_field_sexp(tmpsx, stanza);
        tmpsx = tmpsx->next;
    }

    struct stanza_field_s *modules_field;
    int id = FIELD_MODULES;
    HASH_FIND_INT(stanza->fields, &id, modules_field);
    if (modules_field == NULL) {
        /* log_info("no 'modules' field in library sexp; adding."); */
        /* struct stanza_field_s *field = stanza_field_new(stanza); */
        struct stanza_field_s *field = (struct stanza_field_s*)calloc(sizeof(struct stanza_field_s), 1);
        field->type = FIELD_MODULES;
        //FIXME: verify no key dup?
        HASH_ADD_INT(stanza->fields, type, field);
    /* } else { */
    /*     log_info("library sexp contains 'modules' field."); */
    }
    utarray_push_back(pkg->stanzas, &stanza); /* NB: push ptr ptr */

    log_trace("1 stanza->fields ptr: %p", stanza->fields);

    log_debug("1 stanza fld ct: %d", HASH_CNT(hh, stanza->fields));
    log_debug("1 stanzas ct: %d", stanzas_len(pkg->stanzas));
    struct stanza_s *stest, **iter=NULL;
    while( (iter = (struct stanza_s**)utarray_next(pkg->stanzas, iter))) {
        stest = *iter;
        log_trace("2 stest->fields ptr: %p", stest->fields);

        log_debug("2 stanza type: %d", stest->type);
        log_debug("2 stanza fld ct: %d", HASH_CNT(hh, stest->fields));
    }
}

/* *********************** */
void handle_name(sexp_t *exp,  stanza_s *stanza)
{
    /* precon: exp->ty == SEXP_VALUE, exp->val (car exp) == "name" */
    log_trace("handle_name");

    sexp_t *the_list = exp->list;

    CSTRING *rawstr = NULL;
    int bufsz = print_sexp_cstr(&rawstr, exp, 128); /* 128 == buf start size */

    if (the_list->next->ty == SEXP_VALUE) {
        log_debug("\tName value: %s", the_list->next->val);
        /* struct stanza_field_s *field = stanza_field_new(*stanza); */
        stanza_field_s *field = (struct stanza_field_s*)calloc(sizeof(struct stanza_field_s), 1);
        field->type = FIELD_NAME;
        field->name = strdup(the_list->next->val);
        if (bufsz > 0)
            field->rawstr = strndup(rawstr->base, rawstr->len);
        //FIXME: verify no key dup?
        log_trace("1 xxxx");
        HASH_ADD_INT(stanza->fields, type, field);
        log_trace("2 xxxx");
        log_debug("stanza fld ct: %d", HASH_CNT(hh, stanza->fields));
    } else {
        log_warn("Unexpected type for (cadr exp) for name clause.");
    }
    /* need we verify no more args? */
    if (the_list->next->next) {
        log_warn("unexpected elt in name sexp");
    }
    sdestroy(rawstr);
}

void handle_public_name(sexp_t *exp,  stanza_s *stanza)
{
    log_trace("handle_public_name");

    sexp_t *the_list = exp->list;

    /* char *v; */
    /* sexp_t *tmpsx, *tmpsx2; */
    /* the_list->ty == SEXP_VALUE, the_list->val (car exp) == "public_name" */

    if (the_list->next->ty == SEXP_VALUE) {
        log_debug("\tName value: %s", the_list->next->val);
        /* struct stanza_field_s *field = stanza_field_new(*stanza); */
        stanza_field_s *field = (struct stanza_field_s*)calloc(sizeof(struct stanza_field_s), 1);
        field->type = FIELD_PUBLIC_NAME;
        field->name = strdup(the_list->next->val);

        CSTRING *rawstr = NULL;
        int bufsz = print_sexp_cstr(&rawstr, exp, 128); /* 128 == buf start size */
        if (bufsz > 0) {
            field->rawstr = strndup(rawstr->base, rawstr->len);
        }
        sdestroy(rawstr);

        //FIXME: verify no key dup?
        HASH_ADD_INT(stanza->fields, type, field);
        /* log_debug("stanza len: %d", HASH_CNT(hh, *stanza)); */
    } else {
        log_warn("Unexpected type for (cadr exp) for name clause.");
    }
    /* need we verify no more args? */
    if (the_list->next->next) {
        log_warn("unexpected elt in name sexp");
    }
}

/**
   param exp: sexp, ->list is the list
 */
void handle_libraries(sexp_t *exp, stanza_s *stanza)
{
    log_trace("handle_libraries");
    /* char *v; */
    sexp_t *tmpsx; //, *tmpsx2;
    /* exp->ty == SEXP_VALUE, exp->val (car exp) == "libraries" */

    sexp_t *the_list = exp->list;

    /* (cdr exp) should be a list? value */
    if (the_list->next->ty == SEXP_LIST) {
        log_debug("(cdr exp) for 'libraries' is SEXP_LIST");
    } else {
        if (the_list->next->ty == SEXP_VALUE) {
            /* log_debug("(cdr exp) for 'libraries' is SEXP_VALUE"); */

            /* struct stanza_field_s *field = stanza_field_new(*stanza); */
            stanza_field_s *field = (struct stanza_field_s*)calloc(sizeof(struct stanza_field_s), 1);
            field->type = FIELD_LIBRARIES;
            /* field->name = strdup("libstest"); */

            CSTRING *rawstr = NULL;
            int bufsz = print_sexp_cstr(&rawstr, exp, 128); /* 128 == buf start size */
            if (bufsz > 0) {
                field->rawstr = strndup(rawstr->base, rawstr->len);
            }
            sdestroy(rawstr);

            tmpsx = the_list->next;
            if (tmpsx) {
                UT_array *libs;
                utarray_new(libs, &ut_str_icd);

                /* log_debug("cdring over libraries"); */
                while (tmpsx != NULL) {
                    // FIXME: what if libraries contains a list?
                    // e.g.  (libraries (re_export foo))
                    // esp. (select ... from ...) can occur in the list
                    /* log_debug("libraries value: %s", tmpsx->val); */
                    utarray_push_back(libs, &tmpsx->val);
                    /* dune_handle_field_sexp(tmpsx); */
                    tmpsx = tmpsx->next;
                }
                field->libraries = libs;
                //FIXME: verify no key dup?
                HASH_ADD_INT(stanza->fields, type, field);
            }
        } else {
            log_warn("Unexpected type for (cdr exp) for libraries clause.");
        }
    }
    /* log_trace("exiting handle_libraries, STANZA LEN: %d", HASH_CNT(hh, *stanza)); */
}

/** handle Ordered Set Language sexp
    OSL used in: modules, dirs, flags
 */
void handle_osl(sexp_t *list_exp, UT_array *exclusions)
{
    log_debug("handle_osl");
    /* list_exp may be a list, e.g. (modules (:standard \ foo))
       or it may be an atom, e.g. (modules :standard \ main) */

    if (list_exp->ty == SEXP_VALUE) {
        log_debug("\tosl: improper list");
        if (strncmp(list_exp->val, ":standard", 9) == 0) {
            log_debug("\thead: %s", list_exp->val);
            //FIXME: abstract this out into a fn
            sexp_t *exclude = find_sexp("\\", list_exp->next);
            if (exclude) {
                log_debug("found exclude");
                sexp_t *excludes = exclude->next;
                log_debug("exclude->next: %s", excludes->val);
                while (excludes != NULL) {
                    if (excludes->ty == SEXP_VALUE) {
                        log_trace("Excluding %s", excludes->val);
                        utarray_push_back(exclusions, &excludes->val);
                        /* dune_handle_field_sexp(excludes); */
                        excludes = excludes->next;
                        continue;
                    }
                    if (excludes->ty == SEXP_LIST) {
                        log_warn("found SEXP_LIST in exclusions sexp");
                        /* should be an OSL sexp */
                        /* handle_osl(excludes, exclusions); */
                        excludes = excludes->next;
                        continue;
                    }
                }
            } else {
                log_error("FIXME: osl :standard not followed by lib list.");
            }
        }
        goto exit;
    }

    if (list_exp->ty == SEXP_LIST) {
        sexp_t *head = hd_sexp(list_exp);
        if (strncmp(head->val, ":standard", 9) == 0) {
            log_debug("head: %s", head->val);
            /* handle_osl_standard(list_exp, exclusions); */
            sexp_t *exclude = find_sexp("\\", head); //list_exp);
            if (exclude) {
                log_debug("found exclude");
                sexp_t *excludes = exclude->next;
                log_debug("exclude->next: %s", excludes->val);
                while (excludes != NULL) {
                    if (excludes->ty == SEXP_VALUE) {
                        log_trace("Excluding %s", excludes->val);
                        utarray_push_back(exclusions, &excludes->val);
                        /* dune_handle_field_sexp(excludes); */
                        excludes = excludes->next;
                        continue;
                    }
                    if (excludes->ty == SEXP_LIST) {
                        log_warn("found SEXP_LIST in exclusions sexp");
                        /* should be an OSL sexp */
                        /* handle_osl(excludes, exclusions); */
                        excludes = excludes->next;
                        continue;
                    }
                }
            } else {
                log_error("FIXME: osl :standard not followed by lib list.");
            }
        } else {
            log_error("FIXME: osl does not start with :standard");

        }
    }
 exit:
    return;
}

/**

https://dune.readthedocs.io/en/stable/dune-files.html#library

Field 'modules' "uses the Ordered set language where elements are module names and don’t need to start with a uppercase letter. For instance to exclude module Foo: (modules (:standard \ foo))"

Ordered Set Language (OSL): https://dune.readthedocs.io/en/stable/concepts.html#ordered-set-language
*/
void handle_modules(sexp_t *list_exp, stanza_s *stanza)
{
    log_trace("handle_modules");

    /* sexp_t *list_hd = hd_sexp(list_exp); */

    /* char *v; */
    sexp_t *list_tl;
    /* list_hd->ty == SEXP_VALUE, list_hd->val (car list_exp) == "modules" */

    /* (cdr list_exp) should be a list? value */
    /* if (list_hd->next->ty == SEXP_LIST) { */
    /*     log_debug("(cdr list_exp) for 'modules' is SEXP_LIST"); */
    /* } else { */
    /* if (list_hd->next->ty == SEXP_VALUE) { */
        /* log_debug("(cdr list_exp) for 'modules' is SEXP_VALUE"); */

    /* struct stanza_field_s *field = stanza_field_new(*stanza); */
    stanza_field_s *field = (struct stanza_field_s*)calloc(sizeof(struct stanza_field_s), 1);
    field->type = FIELD_MODULES;
    /* field->name = strdup("modules test"); */

    CSTRING *rawstr = NULL;
    int bufsz = print_sexp_cstr(&rawstr, list_exp, 128); /* 128 == buf start size */
    if (bufsz > 0) {
        field->rawstr = strndup(rawstr->base, rawstr->len);
    }
    sdestroy(rawstr);

    list_tl = tl_sexp(list_exp);
    if (list_tl) {

        struct modules_s *modules = (struct modules_s*)calloc(sizeof(struct modules_s), 1);
        /* UT_array *fileseq; */
        /* utarray_new(fileseq, &ut_str_icd); */
        UT_array *inclusions;
        utarray_new(inclusions, &ut_str_icd);
        UT_array *exclusions;
        utarray_new(exclusions, &ut_str_icd);

        /* log_debug("cdring over modules"); */
        while (list_tl != NULL) {
            if (list_tl->ty == SEXP_LIST) {
                log_warn("found SEXP_LIST in modules sexp");
                /* should be an OSL sexp */
                handle_osl(list_tl, exclusions);
                list_tl = list_tl->next;
                continue;
            }
            if (list_tl->ty == SEXP_VALUE) {
                if (strncmp(list_tl->val, ":standard", 9) == 0) {
                    log_debug("list len: %d", sexp_list_length(list_exp));
                    log_debug("cdr len: %d", sexp_list_length(list_tl));
                    handle_osl(list_tl, exclusions);
                    break;
                } else {
                    utarray_push_back(inclusions, &list_tl->val);
                }
                /* dune_handle_field_sexp(list_tl); */
                list_tl = list_tl->next;
            }
        }
        /* modules->fileseq = fileseq; */
        modules->include = inclusions;
        modules->exclude = exclusions;
        field->modules = modules;
        //FIXME: verify no key dup?
        HASH_ADD_INT(stanza->fields, type, field);
    }
}

void handle_preprocess(sexp_t *exp, stanza_s *stanza)
{
    log_trace("handle_preprocess");

    /* sexp_t *the_list = exp->list; */

    /* char *v; */
    /* sexp_t *tmpsx, *tmpsx2; */
    /* the_list->ty == SEXP_VALUE, the_list->val (car exp) == "preprocess" */

    /* (cdr exp) should be a list? value */
    /* if (the_list->next->ty == SEXP_LIST) { */
    /*     log_debug("(cdr exp) for 'preprocess' is SEXP_LIST"); */
    /* } else { */
    /* if (the_list->next->ty == SEXP_VALUE) { */
        /* log_debug("(cdr exp) for 'preprocess' is SEXP_VALUE"); */

    /* struct stanza_field_s *field = stanza_field_new(*stanza); */
    stanza_field_s *field = (struct stanza_field_s*)calloc(sizeof(struct stanza_field_s), 1);
    field->type = FIELD_PREPROCESS;
    field->name = strdup("preprocess test");

    CSTRING *rawstr = NULL;
    int bufsz = print_sexp_cstr(&rawstr, exp, 128); /* 128 == buf start size */
    if (bufsz > 0) {
        field->rawstr = strndup(rawstr->base, rawstr->len);
    }
    sdestroy(rawstr);

    /* tmpsx = the_list->next; */
    /* if (tmpsx) { */
    /*     UT_array *preprocess; */
    /*     utarray_new(preprocess, &ut_str_icd); */

    /*     /\* log_debug("cdring over preprocess"); *\/ */
    /*     while (tmpsx != NULL) { */
    /*         if (tmpsx->ty == SEXP_LIST) { */
    /*             log_warn("found SEXP_LIST in preprocess sexp"); */
    /*             /\* should be an OSL sexp *\/ */
    /*             /\* handle_osl(tmpxs, preprocess); *\/ */
    /*             tmpsx = tmpsx->next; */
    /*             continue; */
    /*         } */
    /*         // e.g.  (preprocess (re_export foo)) */
    /*         // esp. (select ... from ...) can occur in the list */
    /*         /\* log_debug("preprocess value: %s", tmpsx->val); *\/ */
    /*         utarray_push_back(preprocess, &tmpsx->val); */
    /*         /\* dune_handle_field_sexp(tmpsx); *\/ */
    /*         tmpsx = tmpsx->next; */
    /*     } */
    /*     field->preprocess = preprocess; */
    /*     //FIXME: verify no key dup? */
    /* } */
    HASH_ADD_INT(stanza->fields, type, field);
}

void dune_handle_field_sexp(sexp_t *exp, stanza_s *stanza)
{
    log_trace("dune_handle_field_sexp");

    char *v;
    /* sexp_t *tmpsx, *tmpsx2; */

    log_trace("\texp->ty: %d", exp->ty);
    if (exp->ty == SEXP_LIST) {
        log_trace("\texp->ty: SEXP_LIST (type %d)", exp->ty);
        if (exp->list->ty == SEXP_VALUE) {
            log_trace("\texp->list->ty: SEXP_VALUE (type %d)", exp->list->ty);
            v = exp->list->val;
        /* } else { */
        /*     return env; */
        }
    } else {
        if (exp->ty == SEXP_VALUE) {
            log_trace("\texp->ty: SEXP_VALUE (type %d): %s", exp->ty, exp->val);
        } else {
            log_fatal("\tUnexpected sexp type");
        }
    }

    /* always pass the exp, not the head (exp->list) */
    if (strcmp(v, "name") == 0) {
        handle_name(exp, stanza);
        goto exit;
    }
    if (strcmp(v, "public_name") == 0) {
        handle_public_name(exp, stanza);
        goto exit;
    }
    if (strcmp(v, "libraries") == 0) {
        handle_libraries(exp, stanza);
        goto exit;
    }
    if (strcmp(v, "modules") == 0) {
        handle_modules(exp, stanza);
        goto exit;
    }
    if (strcmp(v, "preprocess") == 0) {
        handle_preprocess(exp, stanza);
        goto exit;
    }

    log_debug("EVAL: Unknown = %s",v);

 exit:
    /* log_trace("exiting dune_handle_field_sexp"); */
    return;
}

void dune_handle_top_sexp(sexp_t *exp, obazl_dune_package_s *pkg) // stanza_s *stanza)
{
    log_trace("dune_handle_top_sexp");

    char *v;
    /* sexp_t *tmpsx, *tmpsx2; */

    log_trace("\texp->ty: %d", exp->ty);
    if (exp->ty == SEXP_LIST) {
        log_trace("\texp->ty: SEXP_LIST (type %d)", exp->ty);
        if (exp->list->ty == SEXP_VALUE) {
            log_trace("\texp->list->ty: SEXP_VALUE (type %d)", exp->list->ty);
            v = exp->list->val;
        /* } else { */
        /*     return env; */
        }
    } else {
        if (exp->ty == SEXP_VALUE) {
            log_trace("\texp->ty: SEXP_VALUE (type %d): %s", exp->ty, exp->val);
        } else {
            log_fatal("\tUnexpected sexp type");
        }
    }

    /* always pass the exp, not the head (exp->list) */
    if (strcmp(v, "library") == 0) {
        handle_library(exp, pkg);
        goto exit;
    }
    /* if (strcmp(v, "executable") == 0) { */
    /*     handle_executable(exp, pkg); */
    /*     goto exit; */
    /* } */
    /* if (strcmp(v, "rule") == 0) { */
    /*     handle_rule_name(exp, pkg); */
    /*     goto exit; */
    /* } */
    log_debug("EVAL: Unknown top level sexp = %s",v);

 exit:
    /* log_trace("exiting dune_handle_top_sexp"); */
    return;
}

struct module_s {
    char *name;                 /* key */
    char *structfile;
    char *sigfile;
    UT_hash_handle hh;
};

/* char *fname_to_mname(char *fname) */
/* { */
/*     /\* log_debug("fname_to_mname: %s", fname); *\/ */
/*     char *bname = basename(fname); */
/*     char *result; */
/*     int len = strlen(bname); */
/*     char *p = bname; */
/*     if (strncmp(".ml", p+len-3, 3) == 0) { */
/*         result = strndup(bname, len-3); */
/*     } else { */
/*         if (strncmp(".mli", p+len-4, 4) == 0) { */
/*             result = strndup(bname, len-4); */
/*         } else { */
/*             return NULL; */
/*         } */
/*     } */
/*     result[0] = toupper(result[0]); */
/*     return result; */
/* } */

/* bool is_structfile(char *fname) */
/* { */
/*     /\* log_debug("is_structfile %s", fname); *\/ */
/*     int len = strlen(fname); */
/*     char *p = fname; */
/*     if (strncmp(".ml", p+len-3, 3) == 0) { */
/*         /\* log_debug("structfile yes"); *\/ */
/*         return true; */
/*     } */
/*     else return false; */
/* } */

/* bool is_sigfile(char *fname) */
/* { */
/*     /\* log_debug("is_sigfile %s", fname); *\/ */
/*     int len = strlen(fname); */
/*     char *p = fname; */
/*     if (strncmp(".mli", p+len-4, 4) == 0) { */
/*         /\* log_debug("sigfile yes"); *\/ */
/*         return true; */
/*     } */
/*     else return false; */
/* } */

struct module_s *inventory_modules(char *basedir)
{
    /* log_debug("inventory_modules: %s", basedir); */

    struct module_s *modules = NULL;

    DIR *dir;
    struct dirent *dir_entry;
    errno = 0;
    dir = opendir(basedir);
    if (dir == NULL) {
        errnum = errno;
        perror(basedir);
        log_error("opendir failure for %s", basedir);
        exit(EXIT_FAILURE);
    }
    /* printf("opened dir %s\n", currdir); */

    struct module_s *mod = NULL;
    char *mname;
    while ((dir_entry = readdir(dir)) != NULL) {
        if (dir_entry->d_type == DT_REG) {
            /* log_debug("entry: %s", dir_entry->d_name); */
            if (strncmp("dune", dir_entry->d_name, 4) == 0) continue;

            /* log_debug("File: %s", dir_entry->d_name); */
            mname = fname_to_mname(dir_entry->d_name);
            /* log_debug("mname: %s", mname); */

            if (mname == NULL) continue; /* not an Ocaml src file */

            HASH_FIND_STR(modules, mname, mod);  /* mname already in the hash? */
            if (mod == NULL) {
                mod = (struct module_s*)calloc(sizeof(struct module_s), 1);
                mod->name = strdup(mname);
                /* log_trace("1 xxxx"); */
                if (is_structfile(dir_entry->d_name)) {
                    mod->structfile = dir_entry->d_name;
                } else {
                    if (is_sigfile(dir_entry->d_name)) {
                        /* log_trace("sigfile yes"); */
                        mod->sigfile = dir_entry->d_name;
                    } else {
                        continue; // skip all but *.ml, *.mli - what about .mll, .mly, etc?
                    }
                }
                HASH_ADD_STR(modules, name, mod);  /* 'name' is key field */
            } else {
                if (is_structfile(dir_entry->d_name)) {
                    mod->structfile = dir_entry->d_name;
                } else {
                    if (is_sigfile(dir_entry->d_name)) {
                        mod->sigfile = dir_entry->d_name;
                    } else {
                        continue; // skip all but *.ml, *.mli - what about .mll, .mly, etc?
                    }
                }
            }
        }
    }
    /* log_debug("done inventory"); */
    closedir(dir);
    return modules;
}

/**
   returns obazl_dune_package_s
*/
EXPORT struct obazl_dune_package_s *obazl_dune_parse_file(char *fname)
{
    log_debug("obazl_dune_parse_file: %s", fname);

    /* first inventory modules */
    char *dir = dirname(fname);
    log_debug("dirname: %s", dir);
    struct module_s *all_modules = inventory_modules(dir);
    struct module_s *item, *tmp;
    HASH_ITER(hh, all_modules, item, tmp) {
        log_debug("module: %s; structfile: %s; sigfile: %s",
                  item->name, item->structfile, item->sigfile);
    }

    char buf[BUFSIZ]; // , outbuf[BUFSIZ];
    sexp_t *in; // , *out;
    sexp_iowrap_t *iow;
    int fd;

    struct obazl_dune_package_s *pkg = calloc(sizeof(struct obazl_dune_package_s), 1);
    pkg->path = fname;          /* FIXME: use dirname */
    utarray_new(pkg->stanzas, &stanza_list_icd);

    fd = open(fname,O_RDONLY);
    if (fd < 0) {
        perror(fname);
        log_fatal("open failure");
        exit(EXIT_FAILURE);
    }
    iow = init_iowrap(fd);

    in = read_one_sexp(iow);
    while (in != NULL) {
        /* log_debug("readed one sexp"); */
        /* memset(buf, 0, BUFSIZ); */
        print_sexp(buf,BUFSIZ,in);
        log_trace("readed sexp: %s", buf);

        dune_handle_top_sexp(in, pkg);

        log_trace("handled_sexp");
        /* log_debug("STANZAS ct: %d", HASH_CNT(hh, stanza)); */

        /* debugging: */
        /* struct stanza_field_s *item; // , *tmp; */
        /* for (item = stanza; item != NULL; item = (struct stanza_field_s*)(item->hh.next)) { */
        /*     log_debug("added stanza: %s", item->name); */
        /* } */
        /* HASH_ITER(hh, stanza, item, tmp) { */
        /*     log_debug("added stanza map for %s", item->name); */
        /* } */
        /* log_debug("stanza field: %s", stanza->name); */

        /* utarray_push_back(pkg->stanzas, &stanza); */

        destroy_sexp(in);
        /* log_trace("6 xxxxxxxxxxxxxxxx"); */

        /* if (out != NULL) { */
        /*     /\* log_trace("7 xxxxxxxxxxxxxxxx"); *\/ */
        /*     /\* fprintf(stderr, "7 xxxx\n"); *\/ */
        /*     /\* destroy_sexp(out); *\/ */
        /*     sexp_cleanup(); */
        /*     /\* fprintf(stderr, "8 xxxx\n"); *\/ */
        /*     /\* log_trace("8 xxxxxxxxxxxxxxxx"); *\/ */
        /*     /\* FREE_CHECKPOINT(out); *\/ */
        /*     /\* fprintf(stderr, "9 xxxx\n"); *\/ */
        /*     /\* log_trace("9 xxxxxxxxxxxxxxxx"); *\/ */
        /* } */
        sexp_cleanup();
        log_debug("reading next sexp");
        /* report_memory_counters(); */
        /* reset_memory_counters(); */

        in = read_one_sexp(iow);
    }
    log_debug("finished reading sexps");

    destroy_iowrap(iow);

    return pkg;
}
