#include <errno.h>
#include <fcntl.h>
#include <libgen.h>

#if INTERFACE
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#endif

#include <unistd.h>

#include "log.h"
#include "utarray.h"
#include "utstring.h"

#if INTERFACE
#include "utstring.h"
#endif

#include "test_lex_mtags.h"

UT_string *build_file;

void _lex_file(char *fname)
{
    log_set_quiet(false);

    log_info("_lex_file: %s", fname);
    FILE *f;

    f = fopen(fname, "r");
    if (f == NULL) {
        perror(fname);
        log_error("fopen failure for %s", fname);
        /* log_error("Value of errno: %d", errnum); */
        /* log_error("fopen error %s", strerror( errnum )); */
        exit(EXIT_FAILURE);
    }
    fseek(f, 0, SEEK_END);
    const size_t fsize = (size_t) ftell(f);
    if (fsize == 0) {
        fclose(f);
        errno = -1;
        exit(EXIT_FAILURE);
    }
    fseek(f, 0, SEEK_SET);
    char *buffer = (char*) malloc(fsize + 1);
    fread(buffer, 1, fsize, f);
    buffer[fsize] = 0;
    fclose(f);

    if (is_empty(buffer)) {
        fclose(f);
        errno = -2;
        exit(EXIT_FAILURE);
    }

    struct bf_lexer_s * lexer = malloc(sizeof(struct bf_lexer_s));
    lexer_init(lexer, buffer);

    /* void* pParser = ParseAlloc (malloc); */
    /* InitParserState(ast); */
    /* ParseTrace(stdout, "trace_"); */
    int tok;
    struct node_s *btok = calloc(sizeof(struct node_s), 1);

    log_set_quiet(false);
    log_set_level(LOG_TRACE);
    log_info("starting lex");

    while ( (tok = get_next_token(lexer, &btok)) != 0 ) {
        /* log_debug("token: %d (%d:%d)", tok, btok->line, btok->col); */
        /* log_debug("token name: %s", token_name[tok]); */
        /* if (btok->s != NULL) { */
        /*     log_debug("token str: %p", btok->s); */
        /* } */
        /* log_debug("lexer pos: line %d col %d", lexer->pos.line, lexer->pos.col); */
        log_debug("token: %s (%d) @ (%d:%d) %p",
                  token_name[tok],
                  tok,
                  btok->line, btok->col,
                  btok
                  /* lexer->mode, */
                  /* btok->s // load.label */
                  /* lexer->clean_line, */
                  /* lexer->indent */
                  );
        log_debug("\trawstr: [[%s]]", btok->s);
        btok->type = tok;
        dump_node(btok);
        /* log_debug("subexpr ct: %d", btok->load.sym_ct / 4); */
        /* if (tok == TK_LOAD) { */
        /*     log_debug("load.syms ptr: %p", btok->load.syms); */
        /*     log_debug("Subexpr ct: %d", utarray_len(btok->load.syms)); */
        /*     char **p = NULL; */
        /*     int ct = utarray_len(btok->load.syms); */
        /*     char **s, **e; */
        /*     for (int i = 0; i < ct; i+=4) { */
        /*         log_debug("\tidx %d", i); */
        /*         s = utarray_eltptr(btok->load.syms, i); */
        /*         e = utarray_eltptr(btok->load.syms, i+1); */
        /*         if (*s == NULL) { log_debug("\talias: NULL"); } */
        /*         else { */
        /*             log_debug("\talias: %.*s", *e-*s, *s); */
        /*         } */
        /*         s = utarray_eltptr(btok->load.syms, i+2); */
        /*         e = utarray_eltptr(btok->load.syms, i+3); */
        /*         log_debug("\tsym: %.*s", *e-*s, *s); */
        /*     } */
        /* } */

        /* Parse(pParser, tok, btok, &ast); // , &sState); */

        btok = calloc(sizeof(struct node_s), 1);
    }
    /* return 0; */
}

int main(int argc, char *argv[])
{
    int opt;

    utstring_new(build_file);

    while ((opt = getopt(argc, argv, "f:hv")) != -1) {
        switch (opt) {
        case 'f':
            /* BUILD.bazel or BUILD file */
            utstring_printf(build_file, "%s", optarg);
            break;
        case 'h':
            log_info("Help: ");
            exit(EXIT_SUCCESS);
        default:
            log_error("Usage: %s [-f] [buildfile]", argv[0]);
            exit(EXIT_FAILURE);
        }
    }

    if (utstring_len(build_file) == 0) {
        log_error("-f <buildfile> must be provided.");
        exit(EXIT_FAILURE);
    }

    _lex_file(utstring_body(build_file));
}
