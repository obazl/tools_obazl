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

#include "test_parse.h"

UT_string *build_file;

struct obazl_buildfile_s *ast;

void _parse_file(char *fname)
{
    log_set_quiet(false);

    log_info("_parse_file: %s", fname);
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

    struct bf_lexer * lexer = malloc(sizeof(struct bf_lexer));
    lexer_init(lexer, buffer);

    void* pParser = ParseAlloc (malloc);
    parser_init(fname, &ast);
    log_debug("parsing %s", ast->fname);
    /* ParseTrace(stdout, "trace_"); */
    int tok;
    /* struct bzl_token_s *btok = calloc(sizeof(struct bzl_token_s), 1); */
    struct node_s *btok = calloc(sizeof(struct node_s), 1);

    log_set_quiet(false);
    log_set_level(LOG_TRACE);
    log_info("starting lex");

    while ( (tok = get_next_token(lexer, &btok)) != 0 ) {
        btok->type = tok;
        /* btok->line = lexer->pos.line; */
        /* btok->col  = lexer->pos.col; */
        log_debug("token code: %d, %d", tok, btok->type);
        /* log_debug("token name: %s", token_name[tok]); */
        /* if (btok->s != NULL) { */
        /*     log_debug("token str: %p", btok->s); */
        /* } */
        log_debug("lexer pos: line %d col %d", lexer->pos.line, lexer->pos.col);

        log_debug("token: %s (%d), mode: %d, (%d:%d), str: '%s'\n",
                  token_name[tok],
                  /* yyTokenName[tok], */
                  tok,
                  lexer->mode,
                  btok->line, btok->col,
                  btok->s
                  /* lexer->clean_line, */
                  /* lexer->indent */
                  );
        /* log_debug("btok pos: line %d col %d", btok->pos.line, btok->pos.col); */
        Parse(pParser, tok, btok, &ast); // , &sState);

        btok = calloc(sizeof(struct node_s), 1);
    }
    Parse(pParser, 0, btok, &ast); // , &sState);
    ParseFree(pParser, free );

    free(buffer);
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

    _parse_file(utstring_body(build_file));

    log_debug("parsed file %s", ast->fname);
    dump_ast(ast);

    /* for(p=(intpair_t*)utarray_front((*ast)->nodelist); */
    /*     p!=NULL; */
    /*     p=(intpair_t*)utarray_next((*ast)->nodelist,p)) { */
    /*     printf("%d %d\n", p->a, p->b); */
    /* } */
    /* utarray_free((*ast)->nodelist); */
    /* free(ast); */
}
