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

#include "obazl_starlark.h"

UT_string *build_file;

/* struct obazl_buildfile_s *ast; */

int line;
int col;

 /* void parser_init(char *fname, struct obazl_buildfile_s **ast) */
 /* { */
 /*     log_debug("parser_init"); */
 /*     *ast = (struct obazl_buildfile_s*)calloc(sizeof(struct obazl_buildfile_s), 1); */
 /*     (*ast)->fname = strdup(fname); */
 /*     utarray_new( (*ast)->nodelist, &node_icd); */
 /* } */

UT_array *obazl_starlark_lex_string(const char *buffer)
{
    log_set_quiet(false);

    log_info("obazl_starlark_lex_string:\n%s", buffer);

    UT_array *token_list;
    utarray_new(token_list, &node_icd);

    int tok;
    struct node_s *btok = calloc(sizeof(struct node_s), 1);

    struct bf_lexer_s * lexer = malloc(sizeof(struct bf_lexer_s));
    lexer_init(lexer, buffer);


    log_set_quiet(false);
    log_set_level(LOG_TRACE);
    log_info("starting lex");

    while ( (tok = get_next_token(lexer, &btok)) != 0 ) {
        log_debug("TOKEN: %s (%d) @ (%d:%d) %p",
                  token_name[tok][0],
                  tok,
                  btok->line, btok->col,
                  btok
                  );
        log_debug("\tlexer posn: (%d:%d)", lexer->pos.line, lexer->pos.col);
        log_debug("\tstarttok: :]%s[:", btok->s);
        btok->type = tok;
        /* dump_node(btok); */
        utarray_push_back(token_list, btok);

        btok = calloc(sizeof(struct node_s), 1);
    }
    return token_list;
}

UT_array *obazl_starlark_lex_file(char *fname)
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

    int tok;
    struct node_s *btok = calloc(sizeof(struct node_s), 1);

    log_set_quiet(false);
    log_set_level(LOG_TRACE);
    log_info("starting lex");

    while ( (tok = get_next_token(lexer, &btok)) != 0 ) {
        btok->type = tok;
        log_debug("LEXED TOKEN: %s (%d/%d) @ (%d:%d) %p",
                  token_name[tok][0],
                  tok, btok->type,
                  btok->line, btok->col,
                  btok
                  );
        log_debug("\tlexer posn: (%d:%d)", lexer->pos.line, lexer->pos.col);
        log_debug("\tstarttok: :]%s[:", btok->s);
        btok->type = tok;
        dump_node(btok);

        utarray_push_back(token_list, btok);

        btok = calloc(sizeof(struct node_s), 1);
    }
    return token_list;
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
struct node_s *obazl_starlark_parse_string(char *buffer)
{
    log_set_quiet(false);

    log_info("obazl_starlark_parse_string: %s", buffer);

    int tok;
    struct node_s *btok = calloc(sizeof(struct node_s), 1);

    struct bf_lexer_s * lexer = malloc(sizeof(struct bf_lexer_s));
    lexer_init(lexer, buffer);

    struct node_s *root;
    void* pParser = ParseAlloc (malloc);
    /* parser_init(fname, &ast); */
    /* log_debug("parsing %s", ast->fname); */
    /* ParseTrace(stdout, "trace_"); */

    log_set_quiet(false);
    log_set_level(LOG_TRACE);
    log_info("starting parse");

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
        dump_node(btok);
        log_debug(">>>>>>>>>>>>>>>>call parser for tok %d/%d", tok, btok->type);
        log_debug("root: %p", root);
        Parse(pParser, tok, btok, &root);
        log_debug(">>>>>>>>>>>>>>>>/call parser");
        btok = calloc(sizeof(struct node_s), 1);
    }
    log_debug(">>>>>>>>>>>>>>>>FINAL call parser");
    Parse(pParser, 0, btok, &root);
    log_debug(">>>>>>>>>>>>>>>>/FINAL call parser");
    /* ParseFree(pParser, free ); */

    /* fflush(stdout); */
    /* fflush(stderr); */
    return root;
}

struct node_s *obazl_starlark_parse_file(char *fname)
{
    log_set_quiet(false);

    log_info("obazl_starlark_parse_file: %s", fname);
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

    int tok;
    /* struct bzl_token_s *btok = calloc(sizeof(struct bzl_token_s), 1); */
    struct node_s *btok = calloc(sizeof(struct node_s), 1);
    struct bf_lexer_s * lexer = malloc(sizeof(struct bf_lexer_s));
    lexer_init(lexer, buffer);

    struct node_s *root;
    void* pParser = ParseAlloc (malloc);
    /* parser_init(fname, &ast); */
    /* log_debug("parsing %s", ast->fname); */
    /* ParseTrace(stdout, "trace_"); */

    log_set_quiet(false);
    log_set_level(LOG_TRACE);
    log_info("starting parse");

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
        dump_node(btok);
        log_debug(">>>>>>>>>>>>>>>>call parser for tok %d/%d", tok, btok->type);
        log_debug("root: %p", root);
        Parse(pParser, tok, btok, &root);
        log_debug(">>>>>>>>>>>>>>>>/call parser");
        btok = calloc(sizeof(struct node_s), 1);
    }
    log_debug(">>>>>>>>>>>>>>>>FINAL call parser");
    Parse(pParser, 0, btok, &root);
    log_debug(">>>>>>>>>>>>>>>>/FINAL call parser");
    ParseFree(pParser, free );
    /* dump_node(root); */

    free(buffer);
    return root;
}

void root2string(struct node_s *node, UT_string *buffer)
{
    /* log_debug("root2string, line %d", line); */
    line = col = 0;
    node2string(node, buffer);
    /* if (utstring_body(buffer)[utstring_len(buffer)-1] != '\n') { */
        utstring_printf(buffer, "\n");
    /* } */
}

void rootlist2string(UT_array *nodes, UT_string *buffer)
{
    /* log_debug("rootlist2string, line %d", line); */
    line = col = 0;
    nodelist2string(nodes, buffer);
    if (utstring_body(buffer)[utstring_len(buffer)-1] != '\n') {
        utstring_printf(buffer, "\n");
    }
}

void node2string(struct node_s *node, UT_string *buffer)
{
    /* log_debug("node2string, line %d", line); */
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
        /* log_debug("STRINGED TOK: %d %s: :]%s[:", */
        /*           node->type, token_name[node->type][0], */
        /*           node->s); */
        if (node->type == TK_STRING) {
            utstring_printf(buffer, "%c%s%c",
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
        /* log_debug("TOK[%d] %s: :]%s[: (len: %d)", */
        /*           node->type, */
        /*           token_name[node->type][0], */
        /*           token_name[node->type][1], */
        /*           strlen(token_name[node->type][1])); */
        if (strlen(token_name[node->type][1]) > 0) {
            utstring_printf(buffer, "%s", token_name[node->type][1]);
            col += strlen(token_name[node->type][1]);
        }
    }

    if (node->comments) {
        if (utarray_len(node->comments) > 0) {
            comments2string(node->comments, buffer);
        }
    }
    if (node->subnodes) {
        if (utarray_len(node->subnodes) > 0) {
            nodelist2string(node->subnodes, buffer);
        }
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
    /* log_debug("nodelist2string"); */
    /* line = col = 0; */
    struct node_s *node=NULL;
    while( (node=(struct node_s*)utarray_next(nodes, node))) {
        node2string(node, buffer);
    }
}
