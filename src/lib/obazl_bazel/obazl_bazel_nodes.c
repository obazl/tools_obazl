#include "log.h"
#include "obazl_bazel_nodes.h"

#if INTERFACE
#include "utarray.h"

struct obazl_buildfile_s {
    char *fname;
    UT_array *nodelist;
};

struct node_s {
    /* enum node_type_e type; */
    int type;
    int line, col;
    union {
        char *s;
        UT_array *subnodes;
    };
    UT_array *comments;
};
#endif

void nodelist_copy(UT_array *_dst, UT_array *_src)
{
    log_debug("node_copy: %p <- %p", _dst, _src);
}

void node_copy(void *_dst, const void *_src)
{
    log_debug("node_copy: %p <- %p", _dst, _src);
    struct node_s *dst = (struct node_s*)_dst;
    struct node_s *src = (struct node_s*)_src;
    dst->type = src->type;
    /* log_debug("node type: %d", src->type); */
    switch(src->type) {
    case TK_ALIAS:
        log_debug("TK_ALIAS");
        /* log_debug("alias: %s = %s", */
        /*           src->alias->alias.s, src->alias->sym.s); */
        /* dst->subnodes = (struct node_s*)calloc(sizeof(struct node_s), 1); */
        /* nodelist_copy(dst->subnodes, src->subnodes); */
        *dst = *src;
        /* dst->subnodes = src->subnodes; */
        break;
    case TK_COLON:
        log_debug("TK_COLON");
        *dst = *src;
        break;
    case TK_COMMA:
        log_debug("TK_COMMA");
        *dst = *src;
        break;
    case TK_COMMENT:
        log_debug("TK_COMMENT (%d:%d): %s",
                  src->line, src->col,
                  src->s);
        *dst = *src;
        /* dst->line = src->line; */
        /* dst->col  = src->col; */
        break;
    case TK_DEF:
        log_debug("TK_DEF");
        *dst = *src;
        break;
    case TK_DEF_PARAMS:
        log_debug("TK_DEF_PARAMS");
        *dst = *src;
        break;
    case TK_DEF_STMT:
        log_debug("TK_DEF_STMT");
        *dst = *src;
        break;
    case TK_EQ:
        log_debug("TK_EQ");
        *dst = *src;
        break;
    case TK_FLOAT:
        log_debug("TK_FLOAT");
        *dst = *src;
        break;
    case TK_ID:
        log_debug("TK_ID: %s", src->s);
        *dst = *src;
        break;
    case TK_INT:
        log_debug("TK_INT: %s", src->s);
        *dst = *src;
        break;
    case TK_LOAD:
        log_debug("TK_LOAD");
        *dst = *src;
        break;
    case TK_LOAD_STMT:
        log_debug("TK_LOAD_STMT");
        *dst = *src;
        break;
    case TK_LPAREN:
        log_debug("TK_LPAREN");
        *dst = *src;
        break;
    case TK_RPAREN:
        log_debug("TK_RPAREN");
        *dst = *src;
        break;
    case TK_STAR_ARGS:
        log_debug("TK_STAR_ARGS");
        *dst = *src;
        break;
    case TK_STARSTAR_ARGS:
        log_debug("TK_STARSTAR_ARGS");
        *dst = *src;
        break;
    case TK_STRING:
        log_debug("TK_STRING: %s", src->s);
        *dst = *src;
        break;
    default:
        log_debug("DEFAULT case for type %d", src->type);
        break;
    }
}

void node_dtor(void *_elt) {
    log_debug("NODE_DTOR type %d", ((struct node_s*)_elt)->type);
    struct node_s *elt = (struct node_s*)_elt;
    /* if (elt->s) free(elt->s); */
}

/* nodelist: UT_array of node_s */
UT_icd node_icd = {sizeof(struct node_s), NULL, node_copy, node_dtor};

#if INTERFACE
struct comma_s {
    int line, col;
}
#endif

#if INTERFACE
struct comment_s {
    int line, col;
    char *s;
}
#endif

void comment_copy(void *_dst, const void *_src)
{
    /* log_debug("comment_copy"); */
    struct comment_s *src = (struct comment_s*)_src;
    struct comment_s *dst = (struct comment_s*)_dst;
    dst->line = src->line;
    dst->col  = src->col;
    dst->s = src->s? strdup(src->s) : NULL;
}

void comment_dtor(void *_comment) {
    log_debug("XXXXXXXXXXXXXXXX COMMENT_DTOR");
    struct comment_s *comment = (struct comment_s*)_comment;
    if (comment->s) free(comment->s);
}

UT_icd comments_icd = {sizeof(struct comment_s), NULL,
                       comment_copy, comment_dtor};

#if INTERFACE
struct symdecl_s {
    int line, col;
    char *sym;
}
#endif

void sym_copy(void *_dst, void *_src)
{
    /* log_debug("sym_copy"); */
    struct symdecl_s *src = (struct symdecl_s*)_src;
    struct symdecl_s *dst = (struct symdecl_s*)_dst;
    dst->line = src->line;
    dst->col  = src->col;
    dst->sym = src->sym? strdup(src->sym) : NULL;
}

void sym_dtor(void *_sym) {
    log_debug("XXXXXXXXXXXXXXXX SYM_DTOR");
    struct symdecl_s *sym = (struct symdecl_s*)_sym;
    if (sym->sym) free(sym->sym);
}

UT_icd symdecl_icd = {sizeof(struct symdecl_s), NULL, NULL, NULL};

#if INTERFACE
struct identifier_s {
    int line, col;
    char *s;
}
#endif

#if INTERFACE
struct alias_s {
    int line, col;
    UT_array *subnodes;         /* ID cmt* EQ cmt* SYM */
    /* struct { */
    /*     int line, col; */
    /*     char *s; */
    /* } alias; */
    /* struct { */
    /*     int line, col; */
    /*     char *s; */
    /* } sym; */
}
#endif

void alias_copy(void *_dst, void *_src)
{
    log_debug("alias_copy");
    _dst = _src;
    /* struct alias_s *src = (struct alias_s*)_src; */
    /* struct alias_s *dst = (struct alias_s*)_dst; */
    /* dst->alias.line = src->alias.line; */
    /* dst->alias.col  = src->alias.col; */
    /* dst->alias.s = src->alias.s? strdup(src->alias.s) : NULL; */
    /* dst->sym.line = src->sym.line; */
    /* dst->sym.col  = src->sym.col; */
    /* dst->sym.s = src->sym.s? strdup(src->sym.s) : NULL; */
}

void alias_dtor(void *_alias) {
    log_debug("XXXXXXXXXXXXXXXX ALIAS_DTOR");
    struct alias_s *alias = (struct alias_s*)_alias;
    /* if (alias->alias) free(alias->alias); */
}

void parser_init(char *fname, struct obazl_buildfile_s **ast)
{
    log_debug("parser_init");
    *ast = (struct obazl_buildfile_s*)calloc(sizeof(struct obazl_buildfile_s), 1);
    (*ast)->fname = strdup(fname);
    utarray_new( (*ast)->nodelist, &node_icd);
}

