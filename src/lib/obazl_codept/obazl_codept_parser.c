#include <dirent.h>
#include <errno.h>
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

#include "utarray.h"
#include "uthash.h"
#include "utstring.h"

#include "log.h"

#include "obazl_codept_parser.h"

char *testbuf;
long  testbuf_len;

#define MAX_MODNAME_LEN 256

#if EXPORT_INTERFACE
enum module_type_e {
    M_LOCAL,
    M_EXTERNAL,
    M_UNKNOWN
};

struct module_s {
    char *name;                 /* key */
    enum module_type_e type;
    union {                     /* type: M_LOCAL */
        struct {
            char *structfile;
            char *sigfile;
            /* char *lexfile; */
            /* char *yaccfile; */
            /* other files needing preprocessing, e.g. .mlg for coq */
        };
        char *lib;    /* type: M_EXTERNAL */
    };
    UT_hash_handle hh;
};
#endif

struct module_s *codept_modules = NULL; /* hashmap of modules */

#if EXPORT_INTERFACE
enum file_type_e { F_STRUCTFILE, F_SIGFILE };

struct filedeps_s {
    char *name;
    enum file_type_e type;
    char *deps_sexp_str;             /* normalized sexp string, for diffing (from print_sexp) */
    UT_array *deps;             /* list of modname strings */
    UT_hash_handle hh;
};
#endif

struct filedeps_s *codept_filedeps = NULL; /* hashmap of filedep specs */

UT_array *dirty_fdeps;          /* filenames with changed deps */

/* struct obazl_codept_package_s { */
/*     char *path; */
/*     UT_array *stanzas;          /\* array of struct stanza_s pointer pointers *\/ */
/*     UT_array *files;            /\* ???? *\/ */
/* } */


/*
  codept sexp output

  top:
  - version
      ( major minor patch )
  - dependencies ;; file deps. files depend on modules
      - (file fname)
      - (deps ( (dep1) (dep2) ... (depn) ))

  - local        ;; project modules
      - ( (module (modname))
          (ml mlfilename)
          (mli mlifilename) )

  - lib          ;; external (i.e. opam) modules
      - ( (module (modname))
          (lib
              ( pathseg1 pathseg2 ... pathsegn )
          )
        )
*/

bool sexp_diff(char **saved_sexp_str, int *saved_sexp_len, sexp_t *the_sexp)
{
    log_debug("");
    log_debug("sexp_diff"); //  %p:", the_sexp);

    /* log_debug("saved_sexp_len %d; str:", *saved_sexp_len); */
    /* log_debug("%s", *saved_sexp_str); */

    memset(testbuf, '\0', testbuf_len);
    int ct = print_sexp(testbuf, testbuf_len, the_sexp);
    if (ct < 0) {
        log_error("Error on print_sexp, code: %d", sexp_errno);
    }
    /* log_debug("testbuf len %d; print ct: %d", testbuf_len, ct); */
    /* log_debug("%s", testbuf); */

    /* char *saved_sexp_str = *saved_sexp_buf; */
    /* int save_sexp_len = *saved_sexp_sz; */

    if (*saved_sexp_str == NULL) {
        log_debug("New sexp");
        *saved_sexp_str = calloc(ct, 1);
        if (*saved_sexp_str == NULL) {
            perror("callocating saved_sexp_str");
            log_fatal("error on calloc for saved_sexp_str");
            exit(EXIT_FAILURE); /* FIXME: cleanup */
        }
        memcpy(*saved_sexp_str, testbuf, ct);
        (*saved_sexp_str)[ct] = '\0';
        *saved_sexp_len = ct;
        log_debug("calloc saved_sexp_len: %d", *saved_sexp_len);
        return true;
    } else {
        if ( *saved_sexp_len != ct ) {
            log_warn("sexp length changed; saved_sexp_len: %d, new len: %d;",
                     *saved_sexp_len, ct);
            *saved_sexp_str = realloc(*saved_sexp_str, ct);
            if (*saved_sexp_str == NULL) {
                perror("reallocating *saved_sexp_str");
                log_fatal("error on calloc for *saved_sexp_str");
                exit(EXIT_FAILURE); /* FIXME: cleanup */
            } else {
                memset(*saved_sexp_str, '\0', ct);
                memcpy(*saved_sexp_str, testbuf, ct);
                (*saved_sexp_str)[ct] = '\0';
                *saved_sexp_len = ct;
                log_debug("realloc saved_sexp_len: %d", *saved_sexp_len);
            }
            return true;
        } else {
            if ( strncmp(testbuf, *saved_sexp_str, *saved_sexp_len) != 0) {
                log_warn("sexp changed; saved_sexp_len: %d, new len: %d;",
                         *saved_sexp_len, ct);
                *saved_sexp_str = realloc(*saved_sexp_str, ct);
                if (*saved_sexp_str == NULL) {
                    perror("reallocating *saved_sexp_str");
                    log_fatal("error on calloc for *saved_sexp_str");
                    exit(EXIT_FAILURE); /* FIXME: cleanup */
                } else {
                    memset(*saved_sexp_str, '\0', ct);
                    memcpy(*saved_sexp_str, testbuf, ct);
                    (*saved_sexp_str)[ct] = '\0';
                    *saved_sexp_len = ct;
                    /* log_debug("realloc saved_sexp_len: %d", *saved_sexp_len); */
                }
                return true;
            } else {
                /* log_info("no change in sexp"); */
                return false;
            }
        }
    }
}

/* **************************************************************** */
/**
   grammar: (file fname)
*/
void handle_file(sexp_t *exp)
{
    log_trace("handle_public_name");

    /* grammar: (file fname) */

    /* sexp_t *the_list = exp->list; */

    /* /\* char *v; *\/ */
    /* /\* sexp_t *tmpsx, *tmpsx2; *\/ */
    /* /\* the_list->ty == SEXP_VALUE, the_list->val (car exp) == "public_name" *\/ */

    /* if (the_list->next->ty == SEXP_VALUE) { */
    /*     log_debug("\tName value: %s", the_list->next->val); */
    /*     /\* struct stanza_field_s *field = stanza_field_new(*stanza); *\/ */
    /*     stanza_field_s *field = (struct stanza_field_s*)calloc(sizeof(struct stanza_field_s), 1); */
    /*     field->type = FIELD_PUBLIC_NAME; */
    /*     field->name = strdup(the_list->next->val); */

    /*     CSTRING *rawstr = NULL; */
    /*     int testbuf_len = print_sexp_cstr(&rawstr, exp, 128); /\* 128 == buf start size *\/ */
    /*     if (testbuf_len > 0) { */
    /*         field->rawstr = strndup(rawstr->base, rawstr->len); */
    /*     } */
    /*     sdestroy(rawstr); */

    /*     //FIXME: verify no key dup? */
    /*     HASH_ADD_INT(stanza->fields, type, field); */
    /*     /\* log_debug("stanza len: %d", HASH_CNT(hh, *stanza)); *\/ */
    /* } else { */
    /*     log_warn("Unexpected type for (cadr exp) for name clause."); */
    /* } */
    /* /\* need we verify no more args? *\/ */
    /* if (the_list->next->next) { */
    /*     log_warn("unexpected elt in name sexp"); */
    /* } */
}

/* **************************************************************** */
/**
   ( (dep1) (dep2) ... (depn) )
*/
UT_array *handle_file_deps(sexp_t *the_deps_list)
{
    /* log_trace("handle_file_deps"); */
    /* example: ((Uri) (String) (Array)) */
    /* log_debug("the_deps_list type: %d", the_deps_list->ty); */
    /* log_debug("the_deps_list->list type: %d", the_deps_list->list->ty); */
    /* log_debug("the_deps_list len: %d", sexp_list_length(the_deps_list)); */

    UT_array *file_deps;
    utarray_new(file_deps, &ut_str_icd);

    sexp_t *deps = the_deps_list->list;
    /* deps is a list of lists, each of one atom */
    while (deps != NULL) {
        if (sexp_list_length(deps->list) != 1) {
            log_warn("list len should be 1: %d", sexp_list_length(deps->list));
        }
        utarray_push_back(file_deps, &(deps->list->val));
        /* log_debug("deps->ty: %d", deps->ty); */
        /* log_debug("deps->list->ty->: %d", deps->list->ty); */
        /* log_debug("deps->list->val->: %s", deps->list->val); */
        /* log_debug("deps->list len: %d", sexp_list_length(deps->list)); /\* 1 *\/ */
        /* log_debug("deps->next len: %d", sexp_list_length(deps->next)); /\* 1 *\/ */
        deps = deps->next;
    }
    return file_deps;
}

/* **************************************************************** */
/*
  grammar: list of two lists:  ( (file fpath) (deps ( (dep1) (dep2) ... (depn) ) ) )
      example: ( (file etc/uri_services.mli) (deps ((Uri))) )
  output: updates codept_filedeps with new filedeps_s
*/
void handle_file_deps_spec(sexp_t *fdeps_spec)
{
    log_debug("handle_file_deps_spec");

    /* log_debug("fdeps_spec type: %d", fdeps_spec->ty); */
    /* log_debug("fdeps_spec->list type: %d", fdeps_spec->list->ty); */
    /* log_debug("fdeps_spec->list->list type: %d", fdeps_spec->list->list->ty); */
    /* log_debug("fdeps_spec->list->list val: %s", fdeps_spec->list->list->val); */

    static char *saved_sexp = NULL;
    static int saved_sexp_len = 0;

    /* 1. lookup in filedeps hashmap */
    /*     a. get the key (filename) from the sexp */
    sexp_t *file_atom = find_sexp("file", fdeps_spec->list);
    sexp_t *deps_atom = find_sexp("deps", fdeps_spec);

    char *fname;
    if (file_atom != NULL) {
        memset(testbuf, '\0', testbuf_len);
        size_t write_len = print_sexp(testbuf, testbuf_len, file_atom);
        log_debug("find 'file' (len: %d)", write_len);
        log_debug("%s", testbuf);
        write_len = print_sexp(testbuf, testbuf_len, file_atom->next);
        if (write_len < 0) {
            ; // FIXME
        }
        /* log_debug("find 'file' -> next: %s", testbuf); */
        /* log_debug("file_atom->next->val: %s", file_atom->next->val); */

        fname = strndup(file_atom->next->val, strlen(file_atom->next->val));
        /* fdeps->name = strndup(file_atom->next->val, strlen(file_atom->next->val)); */
    } else {
        ; // FIXME
    }
    log_debug("fname: %s", fname);

    /* search hashmap for key */
    struct filedeps_s *fdeps;
    HASH_FIND_STR(codept_filedeps, fname, fdeps);
    if (fdeps == NULL) {
        /* not found */
        /* log_debug("fname '%s' not found in filedeps hashmap", fname); */
        fdeps = calloc(sizeof(struct filedeps_s), 1);
        fdeps->name = fname;
        memset(testbuf, '\0', testbuf_len);
        int ct = print_sexp(testbuf, testbuf_len, deps_atom->next);
        /* log_debug("NEW FILEDEPS len %d (strlen: %d)", ct, strlen(testbuf)); */
        fdeps->deps_sexp_str = calloc(sizeof(char*), ct + 1); /* one extra for '\0' */
        memcpy(fdeps->deps_sexp_str, testbuf, ct);
        fdeps->deps_sexp_str[ct] = '\0';
        /* log_debug("NEW FILEDEPS str %s", fdeps->deps_sexp_str); */
    } else {
        /* log_debug("found fname '%s' in filedeps hashmap", fname); */
        /* log_debug("saved deps_sexp_str %d; str:", strlen(fdeps->deps_sexp_str)); */
        /* log_debug("%s", fdeps->deps_sexp_str); */
        /* testbuf[0] = '\0'; */
        /* memset(testbuf, '\0', testbuf_len); */
        /* int ct = print_sexp(testbuf, testbuf_len, fdeps_spec); */
        /* if (ct < 0) { */
        /*     log_error("Error on print_sexp, code: %d", sexp_errno); */
        /* } */
        /* log_debug("fdeps testbuf len %d; str:", ct); */
        /* log_debug("%s", testbuf); */

        int len = strlen(fdeps->deps_sexp_str);
        bool diff =  sexp_diff(&fdeps->deps_sexp_str, &len, deps_atom->next);
        log_debug("%s diff? %d", fname, diff);
        if (diff) {
            utarray_push_back(dirty_fdeps, &fname);
        }
    }

    UT_array *file_deps;
    if (deps_atom != NULL) {
        /* size_t write_len = print_sexp(testbuf, BUFSIZ, deps_atom); */
        /* log_debug("find 'deps': %s", testbuf); */
        /* write_len = print_sexp(testbuf, BUFSIZ, deps_atom->next); */
        /* log_debug("find 'deps' -> next: %s", testbuf); */

        fdeps->deps = handle_file_deps(deps_atom->next);

        /* debugging: */
        /* char **p = NULL; */
        /* log_debug("fdeps->name: %s", fdeps->name); */
        /* log_debug("fdeps->deps:"); */
        /* while ( (p=(char**)utarray_next(fdeps->deps, p))) { */
        /*     log_debug("\t%s",*p); */
        /* } */

    } else {
        // FIXME
    }
    HASH_ADD_KEYPTR(hh, codept_filedeps, fdeps->name, strlen(fdeps->name), fdeps);

    if (deps_atom->next->next != NULL) {
        log_warn("deps_atom->next->next should be NULL?");
    }
}

/* **************************************************************** */
/**
   grammar:
   (dependencies
       ( ( (file fname)
           (deps ( (dep1) (dep2) ... (depn) ))
          )
          ... repeat ...
        )
    )

   example:
(dependencies
 (
  ( (file etc/uri_services.mli) (deps ((Uri))) )
  ( (file etc/uri_services_full.mli) (deps ((Uri))) )))
*/
void handle_dependencies(sexp_t *the_sexp)
{
    log_trace("handle_dependencies");

    static char *saved_sexp = NULL;
    static int saved_sexp_len = 0;

    /* sexp_diff(&saved_sexp, &saved_sexp_len, the_sexp); */

    /* log_debug("saved dependencies len %d,  sexp %p:", saved_sexp_len, saved_sexp); */
    /* log_debug("%s", saved_sexp); */

    sexp_t *the_list = the_sexp->list;

    log_debug("list head, (ty =? SEXP_VALUE): %d, val: %s", (SEXP_VALUE == the_list->ty), the_list->val);

    //  ( ( (file etc/uri_services.mli) (deps ((Uri))) ) ...
    sexp_t *files_list = the_list->next; /* tail: list of lists */
    log_debug("files_list->next (tail) type: %d", files_list->ty);

    //  ( (file etc/uri_services.mli) (deps ((Uri))) )
    printf("xxxxxxxxxxxxxxxx\n");
    dump_codept_dirty_filedeps();
    utarray_clear(dirty_fdeps);
    sexp_t *file_deps_spec = files_list->list;
    while (file_deps_spec != NULL) {
        printf("xxxxxxxxxxxxxxxx\n");
        handle_file_deps_spec(file_deps_spec);
        file_deps_spec = file_deps_spec->next;
    }
}

/* *********************** */
/**
   grammar: ( major minor patch)
 */
void handle_version(sexp_t *exp)
{
    /* precon: exp->ty == SEXP_VALUE, exp->val (car exp) == "name" */
    log_trace("handle_version");
}

/* **************************************************************** */
/**
   grammar:  (pathseg1 pathseg2 ... pathsegn)
   assumption: path is absolute
*/
UT_string *handle_module_lib(sexp_t *the_list)
{
    /* log_debug("handle_module_lib"); */
    /* log_debug("the_list type: %d", the_list->ty); */
    size_t write_len = print_sexp(testbuf, BUFSIZ, the_list);
    /* log_debug("the_list: %s", testbuf); */

    UT_string *libpath;
    utstring_new(libpath);

    sexp_t *this = the_list->list;
    while (this != NULL) {
        utstring_printf(libpath, "/%s", this->val);
        /* log_debug("%s", this->val); */
        /* log_debug("\t%s", utstring_body(libpath)); */
        this = this->next;
    }
    return libpath;
}

/* **************************************************************** */
/**
   grammar:
   module spec: ( (module (modname)) (ml mlfilename) (mli mlifilename) )
       example: ( (module (Uri)) (ml lib/uri.ml) (mli lib/uri.mli) )

   args:
   the_module: list. hd == list, (module (modname))'; tl == list of ml, mli
*/
void handle_module_spec(sexp_t *the_module_spec)
{
    /* log_debug(""); */
    /* log_trace("handle_module"); */

    size_t write_len = print_sexp(testbuf, BUFSIZ, the_module_spec);
    /* log_debug("sexp: %s", testbuf); */

    struct module_s *module = calloc(sizeof(struct module_s), 1);

    sexp_t *modspec_hd;
    sexp_t *modname_sexp;

    /* the_module_spec is a list of lists */
    /* log_debug("the_module_spec type: %d", the_module_spec->ty); */
    if (the_module_spec->ty == SEXP_LIST) {

        /* handle_modspec_hd: (module (modname)) */
        modspec_hd = hd_sexp(the_module_spec);
        /* log_debug("modspec_hd type: %d", modspec_hd->ty); */
        /* log_debug("modspec_hd head val: %s", hd_sexp(modspec_hd)->val); */
        /* log_debug("modspec_hd cdr type: %d", next_sexp(modspec_hd)->ty); */
        modname_sexp = tl_sexp(modspec_hd); /* a list */
        /* log_debug("modname_sexp type: %d, head val: %s", */
        /*           modname_sexp->ty, hd_sexp(modname_sexp)->val); */
        module->name = strndup(hd_sexp(modname_sexp)->val, strlen(hd_sexp(modname_sexp)->val));
        /* log_debug("modname: %s", module->name); */
        /* log_debug("modname_sexp->next: %d", modname_sexp->next); /\* should be null *\/ */

        /* handle modspec components. should be at most two: ml and mli */
        sexp_t *modspec_components = next_sexp(modspec_hd);
        write_len = print_sexp(testbuf, BUFSIZ, modspec_components);
        /* log_debug("modspec_components: %s", testbuf); */
        write_len = print_sexp(testbuf, BUFSIZ, modspec_components->next);
        /* log_debug("modspec_components: %s", testbuf); */

        sexp_t *ml = find_sexp("ml", modspec_components);
        if (ml != NULL) {
            /* write_len = print_sexp(testbuf, BUFSIZ, ml); */
            /* log_debug("find: %s", testbuf); */
            /* write_len = print_sexp(testbuf, BUFSIZ, ml->next); */
            /* log_debug("find next: %s", testbuf); */
            module->type = M_LOCAL;
            module->structfile = strndup(ml->next->val, strlen(ml->next->val));
        /* } else { */
        /*     log_debug("ml not found"); */
        }

        sexp_t *mli = find_sexp("mli", modspec_components);
        if (mli != NULL) {
            /* write_len = print_sexp(testbuf, BUFSIZ, mli); */
            /* log_debug("find: %s", testbuf); */
            /* write_len = print_sexp(testbuf, BUFSIZ, mli->next); */
            /* log_debug("find next: %s", testbuf); */
            module->type = M_LOCAL;
            module->sigfile = strndup(mli->next->val, strlen(mli->next->val));
        /* } else { */
        /*     log_debug("mli not found"); */
        }

        sexp_t *lib = find_sexp("lib", modspec_components);
        if (lib != NULL) {
            module->type = M_EXTERNAL;
            /* write_len = print_sexp(testbuf, BUFSIZ, lib); */
            /* log_debug("find: %s", testbuf); */
            /* write_len = print_sexp(testbuf, BUFSIZ, lib->next); */
            /* log_debug("find next: %s", testbuf); */
            UT_string *libpath = handle_module_lib(lib->next);
            module->lib = strndup(utstring_body(libpath), utstring_len(libpath) + 1);
            utstring_free(libpath);
        /* } else { */
        /*     log_debug("lib not found"); */
        }

        /* log_debug("module->name: %s", module->name); */
        /* log_debug("module->type: %d", module->type); */
        /* log_debug("module->structfile: %s", module->structfile); */
        /* log_debug("module->sigfile: %s", module->sigfile); */
        /* log_debug("module->lib: %s", module->lib); */

    } else {
        log_warn("Unexpected SEXP_VALUE as head of module_spec sexp; val: %s", the_module_spec->val);
    }
    HASH_ADD_STR(codept_modules, name, module);
}

/**
   grammar:
   head:  local
   tail:  list of module specs
   module spec: ( (module (modname)) (ml mlfilename) (mli mlifilename) )
       example: ( (module (Uri)) (ml lib/uri.ml) (mli lib/uri.mli) )

   args:
   the_list: head node of list struct. ty == SEXP_VALUE; val == 'local'; next == tail of list
 */
void handle_local_modules(sexp_t *the_list)
{
    log_trace("handle_local_modules");
    /* log_debug("list head, (ty =? SEXP_VALUE): %d, val: %s", (SEXP_VALUE == the_list->ty), the_list->val); */

    sexp_t *mod_specs = next_sexp(the_list); /* tail */
    sexp_t *mod_spec;

    if (mod_specs->ty == SEXP_LIST) {
        /* log_debug("processing mod specs"); */
        mod_spec = mod_specs->list;
        int i = 0;
        while (mod_spec != NULL) {
            /* every entry should be a list */
            /* log_trace("mod spec entry %d, type: %d", i, mod_spec->ty); */
            if (mod_spec->ty == SEXP_LIST) {
                /* log_debug("processing mod spec LIST"); */
                handle_module_spec(mod_spec);
            } else {
                log_warn("Unexpected VALUE node in mod specs list; val: %s", mod_spec->val);
            }
            mod_spec = mod_spec->next;
            i++;
        }
    } else {
        log_error("Unexpected tail of 'local' sexp");
    }
}

/**
   grammar:
   (lib
       ( (module (modname))
          (lib
              ( pathseg1 pathseg2 ... pathsegn )
          )
        )
    )
 */
void handle_external_modules(sexp_t *the_list)
{
    log_trace("handle_external_modules");

    /* log_debug("list head, (ty =? SEXP_VALUE): %d, val: %s", (SEXP_VALUE == the_list->ty), the_list->val); */

    sexp_t *mod_specs = next_sexp(the_list); /* tail */
    sexp_t *mod_spec;

    if (mod_specs->ty == SEXP_LIST) {
        /* log_debug("processing mod specs"); */
        mod_spec = mod_specs->list;
        int i = 0;
        while (mod_spec != NULL) {
            /* every entry should be a list */
            /* log_trace("mod spec entry %d, type: %d", i, mod_spec->ty); */
            if (mod_spec->ty == SEXP_LIST) {
                /* log_debug("processing mod spec LIST"); */
                handle_module_spec(mod_spec);
            } else {
                log_warn("Unexpected VALUE node in mod specs list; val: %s", mod_spec->val);
            }
            mod_spec = mod_spec->next;
            i++;
        }
    } else {
        log_error("Unexpected tail of 'local' sexp");
    }
}

/**
   grammar:
   (unkown ( (mod1) (mod2) ... (modn) ) )
 */
void handle_unknown(sexp_t *unknown_sexp)
{
    log_trace("handle_unknown");
    /* log_debug("list head, (ty =? SEXP_VALUE): %d, val: %s", (SEXP_VALUE == the_list->ty), the_list->val); */

    sexp_t *the_list = unknown_sexp->list;
    log_debug("unk list type: %d, val: %s", the_list->ty, the_list->val);

    sexp_t *mods_unknown = next_sexp(the_list); /* tail */
    sexp_t *mod_spec;

    if (mods_unknown->ty == SEXP_LIST) { /* expected: list of singleton mod lists */
        log_debug("processing mod specs");
        mod_spec = mods_unknown->list;
        int i = 0;
        struct module_s *module;
        while (mod_spec != NULL) {
            /* every entry should be a singleton list */
            log_trace("mod spec entry %d, type: %d", i, mod_spec->ty);
            if (mod_spec->ty == SEXP_LIST) {
                /* log_debug("processing unknown mod_spec LIST"); */
                log_debug("unknown module: %s", mod_spec->list->val);
                module = calloc(sizeof(struct module_s), 1);
                module->type = M_UNKNOWN;
                module->name = strndup(mod_spec->list->val, MAX_MODNAME_LEN);
            } else {
                log_warn("Unexpected VALUE node in mod_spec of mods_unknown list; val: %s", mod_spec->val);
            }
            mod_spec = mod_spec->next;
            HASH_ADD_STR(codept_modules, name, module);
            i++;
        }
    } else {
        log_error("Unexpected: tail of 'unknown' sexp is not SEXP_LIST");
    }
}

int codept_handle_root_sexp(sexp_t *the_sexp)
{
    log_trace("codept_handle_root_sexp");

    static char *saved_sexp = NULL;
    static int saved_sexp_len = 0;

    /* sexp_diff(&saved_sexp, &saved_sexp_len, the_sexp); */

    size_t write_len = print_sexp(testbuf, testbuf_len, the_sexp);
    /* log_debug("%s", testbuf); */

    /* root should always be a list */
    if (the_sexp->ty != SEXP_LIST) {
        log_fatal("Root node not a list");
        return -1;
    }

    sexp_t *next_sexp = the_sexp->list; /* start with list head */
    while (next_sexp != NULL) {
        /* root should have four list children: version, dependencies, local, and lib */
        printf("next\n");
        if (next_sexp->ty == SEXP_LIST) {
            if (strncmp(next_sexp->list->val, "version", 7) == 0) {
                /* log_debug("list node head: version"); */
                /* skip? */
                goto next;
            }
            if (strncmp(next_sexp->list->val, "dependencies", 12) == 0) {
                handle_dependencies(next_sexp);
                printf("xxxxxxxxxxxxxxxx\n");
                goto next;
            }
            if (strncmp(next_sexp->list->val, "local", 5) == 0) {
                /* log_debug("list node head: local"); */
                handle_local_modules(hd_sexp(next_sexp));
                goto next;
            }
            if (strncmp(next_sexp->list->val, "lib", 3) == 0) {
                /* log_debug("list node head: lib"); */
                handle_external_modules(next_sexp->list);
                goto next;
            }
            if (strncmp(next_sexp->list->val, "unknown", 7) == 0) {
                handle_unknown(next_sexp);
                goto next;
            }
            log_error("Unexpected root child: %s", next_sexp->list->val);
        } else {
            if (next_sexp->ty == SEXP_VALUE) {
                log_warn("unexpected value node: %s", next_sexp->val);
            }
        }
    next:
        next_sexp = next_sexp->next;
    }

 exit:
    /* log_trace("exiting codept_handle_root_sexp"); */
    return 0;
}

/**
   returns obazl_deps_s
*/
EXPORT struct obazl_deps_s *obazl_deps_parse_file(char *fname)
{
    log_debug("obazl_deps_parse_file: %s", fname);

    utarray_new(dirty_fdeps, &ut_str_icd);   /* obazl_codept_parser.c */

    /* int fd; */
    char *work_buf;
    sexp_t *the_sexp = NULL;

    FILE *fp;
    fp = fopen(fname, "r");
    if (fp == NULL) {
        perror(fname);
        log_error("fopen failure");
        return NULL;
    }
    fseek(fp, 0, SEEK_END);
    testbuf_len = (size_t) ftell(fp);
    if (testbuf_len == 0) {
        log_warn("file size is zero");
        fclose(fp);
        errno = -1;
        return NULL;
    }
    log_debug("testbuf_len: %d", testbuf_len);
    fseek(fp, 0, SEEK_SET);
    work_buf = (char*) malloc(testbuf_len + 1);
    testbuf = (char*) malloc(testbuf_len + 1);

    size_t read_len = fread(work_buf, 1, testbuf_len, fp);
    /* log_debug("readed %d bytes", read_len); */
    work_buf[read_len] = '\0';       /* make sure it's properly terminated */
    the_sexp = parse_sexp(work_buf, read_len);
    if (the_sexp == NULL) {
        log_fatal("parse_sexp error: %d", sexp_errno);
        if (sexp_errno == SEXP_ERR_INCOMPLETE) {
            log_info("SEXP_ERR_INCOMPLETE: parsing is incomplete and needs more data to complete it.");
        }
        free(work_buf);
        free(testbuf);
        fclose(fp);
        exit(EXIT_FAILURE);
    }
    /* check the parse result by serializing it to testbuf */
    /* size_t write_len = print_sexp(testbuf, testbuf_len, the_sexp); */
    /* if (write_len < 0) { */
    /*     log_error("sexp_errno: %d", sexp_errno); /\* 8 = SEXP_ERR_BUFFER_FULL *\/ */
    /*     free(work_buf); */
    /*     free(testbuf); */
    /*     close(fd); */
    /*     exit(EXIT_FAILURE); */
    /* } */
    /* log_debug("Printed %d bytes of sexp", write_len); */
    /* log_info("sexp: '%s'", testbuf); */

    /* process the_sexp ... */
    int rc = codept_handle_root_sexp(the_sexp); //, pkg);
    if (rc) {
        log_error("codept_handle_root_sexp error, rc: %d", rc);
    }

    destroy_sexp(the_sexp);
    free(work_buf);
    free(testbuf);
    fclose(fp);
    return NULL; //pkg;
}
