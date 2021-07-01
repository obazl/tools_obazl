#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif

#include "log.h"
#include "utarray.h"

#include "driver.h"

int errnum;

//FIXME: logging. each major lib has its own?
/* #if EXPORT_INTERFACE */
struct logging {
    int verbosity;
    int log_level;
    int parse_verbosity;
    int parse_log_level;
    int lex_verbosity;
    int lex_log_level;
    bool quiet;
    bool log_color;
};
/* #endif */

/* EXPORT */
struct logging logger;

struct obazl_meta_package *ast;

LOCAL char *package_name_from_file_name(char *fname)
{
    char *bn = basename(fname);
    int x = strlen(bn) - 5;
    if ( strncmp(&bn[x], ".META", 5) == 0) {
        bn[x] = '\0';
        return bn;
    } else {
        return basename(dirname(fname));
    }
}

EXPORT struct obazl_meta_package *obazl_meta_parse_file(char *fname)
{
    log_set_quiet(false);

    /* log_info("obazl_meta_parse_file: %s", fname); */
    FILE *f;

    f = fopen(fname, "r");
    if (f == NULL) {
        /* errnum = errno; */
        /* log_error("fopen failure for %s", fname); */
        /* log_error("Value of errno: %d", errnum); */
        /* log_error("fopen error %s", strerror( errnum )); */
        return NULL;
    }
    fseek(f, 0, SEEK_END);
    const size_t fsize = (size_t) ftell(f);
    if (fsize == 0) {
        fclose(f);
        errno = -1;
        return NULL;
    }
    fseek(f, 0, SEEK_SET);
    char *buffer = (char*) malloc(fsize + 1);
    fread(buffer, 1, fsize, f);
    buffer[fsize] = 0;
    fclose(f);

    if (is_empty(buffer)) {
        fclose(f);
        errno = -2;
        return NULL;
    }

    struct meta_lexer * lexer = malloc(sizeof(struct meta_lexer));
    lexer_init(lexer, buffer);

    void* pParser = ParseAlloc (malloc);
    /* InitParserState(ast); */
    /* ParseTrace(stdout, "trace_"); */
    int tok;
    union meta_token *mtok = malloc(sizeof(union meta_token));

    if (logger.lex_verbosity == 0)
        log_set_quiet(true);
    else
        log_set_quiet(logger.quiet);
    log_set_level(logger.lex_log_level);

    /* log_set_quiet(false); */
    /* log_set_level(LOG_TRACE); */
    /* log_info("starting"); */
    /* log_set_quiet(true); */

    while ( (tok = get_next_token(lexer, mtok)) != 0 ) {
        log_set_quiet(true);
        switch(tok) {
        case DIRECTORY:
            log_trace("lex DIRECTORY: %s", mtok->s); break;
        case FLAGS:
            log_trace("lex FLAGS: %s", mtok->s); break;
        case VNAME:
            log_trace("lex VNAME: %s", mtok->s); break;
        case WORD:
            log_trace("lex WORD: %s", mtok->s); break;
        case WORDS:
            log_trace("lex WORDS: %s", mtok->s); break;
        /* case DEPLIST: */
        /*     log_trace("lex DEPLIST: %s", mtok->s); break; */
        /* case DQSTR: */
        /*     log_trace("DQSTR: %s", mtok->s); break; */
        /* case VALTOK: */
        /*     log_trace("lex VALTOK: %s", mtok->s); */
        /*     break; */
        case DQ:
            log_trace("DQ"); break;
        case EQ:
            log_trace("lex EQ"); break;
        case PLUSEQ:
            log_trace("lex PLUSEQ"); break;
        case LPAREN:
            log_trace("lex LPAREN"); break;
        case RPAREN:
            log_trace("lex RPAREN"); break;
        /* case COMMENT: */
        /*     log_trace("lex COMMENT"); break; */
        case VERSION:
            log_trace("lex VERSION: %s", mtok->s);
            break;
        case DESCRIPTION:
            log_trace("lex DESCRIPTION: %s", mtok->s);
            break;
        case REQUIRES:
            log_trace("lex REQUIRES"); break;
        /* case PRED: */
        /*     log_trace("lex PRED: %s", mtok->s); break; */
        /* case NPRED: */
        /*     log_trace("lex NPRED: %s", mtok->s); */
        /*     break; */
        /* case ARCHIVE: */
        /*     log_trace("lex ARCHIVE"); break; */
        case PACKAGE:
            log_trace("lex PACKAGE: %s", mtok->s); break;
        /* case PLUGIN: */
        /*     log_trace("lex PLUGIN"); break; */
        /* case LINKOPTS: */
        /*     log_trace("lex LINKOPTS"); break; */
        case WARNING:
            log_trace("WARNING"); break;
        case ERROR:
            log_trace("ERROR"); break;
        /* case EXISTS_IF: */
        /*     log_trace("EXISTS_IF"); break; */
        /* case PPX: */
        /*     log_trace("PPX"); break; */
        /* case PPXOPT: */
        /*     log_trace("PPXOPT"); break; */
        /* predicates */
        /* case BYTE: */
        /*     log_trace("BYTE"); break; */
        /* case NATIVE: */
        /*     log_trace("NATIVE"); break; */
        /* case TOPLOOP: */
        /*     log_trace("TOPLOOP"); break; */
        /* case CREATE_TOPLOOP: */
        /*     log_trace("CREATE_TOPLOOP"); break; */
        /* case MT: */
        /*     log_trace("MT"); break; */
        /* case MT_POSIX: */
        /*     log_trace("MT_POSIX"); break; */
        /* case MT_VM: */
        /*     log_trace("MT_VM"); break; */
        /* case GPROF: */
        /*     log_trace("GPROF"); break; */
        /* case AUTOLINK: */
        /*     log_trace("AUTOLINK"); break; */
        /* case PREPROCESSOR: */
        /*     log_trace("PREPROCESSOR"); break; */
        /* case SYNTAX: */
        /*     log_trace("SYNTAX"); break; */
        /* case LIBRARY_KIND: */
        /*     log_trace("LIBRARY_KIND"); break; */
        /* case PPX_RUNTIME_DEPS: */
        /*     log_trace("PPX_RUNTIME_DEPS"); break; */
        /* case CUSTOM_PPX: */
        /*     log_trace("CUSTOM_PPX"); break; */
        /* case PPX_DRIVER: */
        /*     log_trace("PPX_DRIVER"); break; */
        /* case PWORD: */
        /*     log_trace("lex PWORD: %s", mtok->s); break; */
        default:
            log_trace("other: %d", tok); break;
        }
        if (logger.parse_verbosity == 0)
            log_set_quiet(false);
        else
            log_set_quiet(logger.quiet);
            log_set_level(logger.parse_log_level);

        Parse(pParser, tok, mtok, &ast); // , &sState);

        mtok = malloc(sizeof(union meta_token));
        if (logger.lex_verbosity == 0)
            log_set_quiet(false);
        else
            log_set_quiet(logger.quiet);
            log_set_level(logger.lex_log_level);
    }

        if (logger.parse_verbosity == 0)
            log_set_quiet(false);
        else
            log_set_quiet(logger.quiet);
            log_set_level(logger.parse_log_level);
    log_set_quiet(true);

    log_trace("lex: end of input");

    Parse(pParser, 0, mtok, &ast); // , &sState);
    ParseFree(pParser, free );

    ast->name      = package_name_from_file_name(fname);
    ast->directory = dirname(fname);
    ast->metafile  = fname;

    if (logger.verbosity == 0)
        log_set_quiet(false);
    else
        log_set_quiet(logger.quiet);
    log_set_level(logger.log_level);

    log_set_quiet(false);

    /* log_trace("PARSED %s", fname); */

    free(buffer);
    return ast;
}

EXPORT char *obazl_meta_version()
{
    return "0.1.0";
}
