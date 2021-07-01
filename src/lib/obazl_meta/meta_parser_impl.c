#if INTERFACE
#include <stdbool.h>
#include <stdio.h>
#include "utarray.h"
#include "uthash.h"
#endif

#include "log.h"
#include "meta_parser_impl.h"

/* #include "omp.h" */

LOCAL int indent = 2;
LOCAL int delta = 2;
LOCAL char *sp = " ";

/* (1<<(pos))) */

/* /enums.h */

#if EXPORT_INTERFACE

#define TOKEN_NAME(x) (char*)#x
#endif

char *token_names[256] = {
    /* [ARCHIVE] = TOKEN_NAME(archive), */
    /* [AUTOLINK] = TOKEN_NAME(autolink), */
    /* [BYTE] = TOKEN_NAME(byte), */
    /* [COMMENT] = TOKEN_NAME(comment), */
    /* [CREATE_TOPLOOP] = TOKEN_NAME(create_toploop), */
    /* [CUSTOM_PPX] = TOKEN_NAME(custom_ppx), */
    /* [DEPLIST] = TOKEN_NAME(deplist), */
    [DESCRIPTION] = TOKEN_NAME(description),
    [DIRECTORY] = TOKEN_NAME(directory),
    [DQ] = TOKEN_NAME(dq),
    [EQ] = TOKEN_NAME(eq),
    /* [DQSTR] = TOKEN_NAME(dqstr), */
    [ERROR] = TOKEN_NAME(error),
    /* [GPROF] = TOKEN_NAME(gprof), */
    /* [EXISTS_IF] = TOKEN_NAME(exists_if), */
    /* [LIBRARY_KIND] = TOKEN_NAME(library_kind), */
    /* [LINKOPTS] = TOKEN_NAME(linkopts), */
    /* [MT] = TOKEN_NAME(mt), */
    /* [MT_POSIX] = TOKEN_NAME(mt_posix), */
    /* [MT_VM] = TOKEN_NAME(mt_vm), */
    [LPAREN] = TOKEN_NAME(lparen),
    /* [NATIVE] = TOKEN_NAME(native), */
    /* [NPRED] = TOKEN_NAME(npred), */
    [PACKAGE] = TOKEN_NAME(package),
    /* [PLUGIN] = TOKEN_NAME(plugin), */
    [PLUSEQ] = TOKEN_NAME(pluseq),
    /* [PPX] = TOKEN_NAME(ppx), */
    /* [PPXOPT] = TOKEN_NAME(ppxopt), */
    /* [PPX_DRIVER] = TOKEN_NAME(ppx_driver), */
    /* [PPX_RUNTIME_DEPS] = TOKEN_NAME(ppx_runtime_deps), */
    /* [PRED] = TOKEN_NAME(pred), */
    /* [PREPROCESSOR] = TOKEN_NAME(preprocessor), */
    /* [PWORD] = TOKEN_NAME(pword), */
    [REQUIRES] = TOKEN_NAME(requires),
    [RPAREN] = TOKEN_NAME(rparen),
    /* [SYNTAX] = TOKEN_NAME(syntax), */
    /* [TOPLOOP] = TOKEN_NAME(toploop), */
    /* [VALTOK] = TOKEN_NAME(valtok), */
    [VERSION] = TOKEN_NAME(version),
    [VNAME] = TOKEN_NAME(vname),
    [WARNING] = TOKEN_NAME(warning),
    [WORD]    = TOKEN_NAME(word),
    [WORDS]    = TOKEN_NAME(words),
};

/* EXPORT char* mystrcat( char* dest, char* src ) */
/* { */
/*     while (*dest) dest++; */
/*     while ( (*dest++ = *src++) ); */
/*     return --dest; */
/* } */

/* #define utarray_oom() do { log_error("ERROR: OOM"); exit(EXIT_FAILURE); } while (0) */
/* void my_oom() { log_trace("OOM"); exit(EXIT_FAILURE); } */

/* ****  AST  **** */

/*
  package   ::= list of properties
  property  ::= (name * list of settings)
  setting   ::= (list of flags * opcode * list of values)
  flag ::= (polarity * name string)
 */

/* **************************************************************** */
/* bool validate_requires_args(UT_array *args) */
/* { */
/*     /\* log_trace("validate_requires_args"); *\/ */
/*     /\* TODO *\/ */
/* } */
