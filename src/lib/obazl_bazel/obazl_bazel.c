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

#include "obazl_bazel.h"

UT_string *build_file;

struct obazl_buildfile_s *ast;

/* const char *punctuation[256] = { */
/* [TK_STARSTAR] = "**", */
/* [TK_ARROW]     = "->", */
/* [TK_LE] = "<=", */
/* [TK_GE] = "=>", */
/* }; */

UT_array *obazl_bazel_lex_file(char *fname)
{
    log_set_quiet(false);

    UT_array *token_list;
    utarray_new(token_list, &node_icd);

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
        btok->type = tok;
        /* log_debug("token: %d (%d:%d)", tok, btok->line, btok->col); */
        /* log_debug("token name: %s", token_name[tok]); */
        /* if (btok->s != NULL) { */
        /*     log_debug("token str: %p", btok->s); */
        /* } */
        log_debug("LEXED TOKEN: %s (%d/%d) @ (%d:%d) %p",
                  token_name[tok][0],
                  tok, btok->type,
                  btok->line, btok->col,
                  btok
                  /* lexer->mode, */
                  /* btok->s // load.label */
                  /* lexer->clean_line, */
                  /* lexer->indent */
                  );
        log_debug("\tlexer posn: (%d:%d)", lexer->pos.line, lexer->pos.col);
        log_debug("\tstarttok: :]%s[:", btok->s);
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

        utarray_push_back(token_list, btok);

        btok = calloc(sizeof(struct node_s), 1);
    }
    return token_list;
}

UT_array *obazl_bazel_lex_string(const char *buffer)
{
    log_set_quiet(false);

    log_info("obazl_bazel_lex_string:\n%s", buffer);
    /* log_info("buf len: %d", strlen(buffer)); */

    UT_array *token_list;
    utarray_new(token_list, &node_icd);

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
        /* log_debug("token name: %s", token_name[tok][0]); */
        /* if (btok->s != NULL) { */
        /*     log_debug("token str: %p", btok->s); */
        /* } */
        log_debug("TOKEN: %s (%d) @ (%d:%d) %p",
                  token_name[tok][0],
                  tok,
                  btok->line, btok->col,
                  btok
                  /* lexer->mode, */
                  /* btok->s // load.label */
                  /* lexer->clean_line, */
                  /* lexer->indent */
                  );
        log_debug("\tlexer posn: (%d:%d)", lexer->pos.line, lexer->pos.col);
        log_debug("\tstarttok: :]%s[:", btok->s);
        btok->type = tok;
        /* dump_node(btok); */
        utarray_push_back(token_list, btok);
        /* Parse(pParser, tok, btok, &ast); // , &sState); */

        btok = calloc(sizeof(struct node_s), 1);
    }
    return token_list;
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
UT_array *obazl_bazel_parse_file(char *fname)
{
    log_set_quiet(false);

    log_info("obazl_bazel_parse_file: %s", fname);
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
        /* log_debug("token code: %d, %d", tok, btok->type); */
        /* log_debug("token name: %s", token_name[tok]); */
        /* if (btok->s != NULL) { */
        /*     log_debug("token str: %p", btok->s); */
        /* } */
        log_debug("TOKEN: %s (%d/%d) @ (%d:%d) %p",
                  token_name[tok][0],
                  tok, btok->type,
                  btok->line, btok->col,
                  btok
                  );
        /* log_debug("TOKEN: %s (%d), mode: %d, (%d:%d), str: '%s'\n", */
        /*           token_name[tok][0], */
        /*           /\* yyTokenName[tok], *\/ */
        /*           tok, */
        /*           lexer->mode, */
        /*           btok->line, btok->col, */
        /*           btok->s */
        /*           /\* lexer->clean_line, *\/ */
        /*           /\* lexer->indent *\/ */
        /*           ); */
        log_debug("lexer posn (%d:%d)", lexer->pos.line, lexer->pos.col);

        /* log_debug("btok pos: line %d col %d", btok->pos.line, btok->pos.col); */
        Parse(pParser, tok, btok, &ast); // , &sState);

        btok = calloc(sizeof(struct node_s), 1);
    }
    Parse(pParser, 0, btok, &ast); // , &sState);
    ParseFree(pParser, free );

    free(buffer);
    /* return 0; */
}

int line;
int col;

void node2string(struct node_s *node, UT_string *buffer)
{
    int i;
    /* node->line is absolute; relativize it; */
    /* int l = node->line - line; */
    for (i=line; i < node->line; i++) {
        utstring_printf(buffer, "\n");
        line++;
        col = 0;
    }

    /* if (node->type == TK_BLANK) */
    /*     utstring_printf(buffer, "\n"); */

        for (i=col; i < node->col; i++) {
            utstring_printf(buffer, " ");
            col++;
        }
    if (node->s) {
        if (node->type == TK_STRING) {
            utstring_printf(buffer, "%s%s%s",
                            node->q, node->s, node->q);
            /* adjust line count for embedded escaped newlines */
            for (char *p = node->s; *p != 0; p++) {
                if (*p == '\n') {
                    line++;
                }
            }
            col += strlen(node->s) + 2; /* allow for quote marks */
        } else {
            utstring_printf(buffer, "%s", node->s);
            col += strlen(node->s);
        }
    } else {
        utstring_printf(buffer, "%s", token_name[node->type][1]);
        col += strlen(token_name[node->type][1]);
    }

    if (node->comments) {
        comments2string(node->comments, buffer);
    }
}

void comments2string(UT_array *nodes, UT_string *buffer)
{
    struct node_s *node=NULL;
    while( (node=(struct node_s*)utarray_next(nodes, node))) {
        node2string(node, buffer);
    }
}

void nodelist2string(UT_array *nodes, UT_string *buffer)
{
    line = col = 0;
    struct node_s *node=NULL;
    while( (node=(struct node_s*)utarray_next(nodes, node))) {
        node2string(node, buffer);
    }
    if (utstring_body(buffer)[utstring_len(buffer)-1] != '\n')
        utstring_printf(buffer, "\n");
}
