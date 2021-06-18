#include "log.h"
#include "obazl_bazel_nodes.h"

#if INTERFACE
#include "utarray.h"

struct obazl_buildfile_s {
    char *fname;
    UT_array *nodelist;
};

/* enum node_type_e { */
/*                   NODE_ALIAS = 1, */
/*                   NODE_COMMA, */
/*                   NODE_COMMENT, */
/*                   NODE_EQ, */
/*                   NODE_ID, */
/*                   NODE_LOAD, */
/*                   NODE_SYM */
/* }; */

struct node_s {
    /* enum node_type_e type; */
    int type;
    int line, col;
    union {
        char *s;
        UT_array *subnodes;
        struct comment_s *comment;
        /* struct alias_s *alias; */
        struct identifier_s *id;
        struct symdecl_s *sym;
    };
};
#endif

void nodelist_copy(UT_array *_dst, UT_array *_src)
{
    log_debug("node_copy: %p <- %p", _dst, _src);
}

void node_copy(void *_dst, void *_src)
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
        dst->subnodes = src->subnodes;
        break;
    case TK_COMMA:
        log_debug("comma");
        dst->line = src->line;
        dst->col  = src->col;
        break;
    case TK_COMMENT:
        /* log_debug("cmt: %s", src->comment->s); */
        dst->comment = (struct comment_s*)calloc(sizeof(struct comment_s), 1);
        comment_copy(dst->comment, src->comment);
        /* log_debug("cmt: %s", dst->comment->s); */
        break;
    case TK_EQ:
        log_debug("TK_EQ");
        /* dst->line = src->line; */
        /* dst->col   = src->col; */
        *dst = *src;
        break;
    case TK_ID:
        log_debug("TK_ID: %s", src->s);
        /* dst->line = src->line; */
        /* dst->col   = src->col; */
        /* dst->s = strdup(src->s); */
        *dst = *src;
        break;
    case TK_LOAD:
        log_debug("TK_LOAD");
        break;
    case TK_SYM:
        log_debug("TK_SYM: %s", src->s);
        /* dst->sym = (struct symdecl_s*)calloc(sizeof(struct symdecl_s), 1); */
        /* sym_copy(dst->sym, src->sym); */
        /* dst->line = src->line; */
        /* dst->col   = src->col; */
        /* dst->s = strdup(src->s); */
        *dst = *src;
        break;
    default:
        log_debug("DEFAULT case for type %d", src->type);
        break;
    }
}

void node_dtor(void *_elt) {
    log_debug("XXXXXXXXXXXXXXXX NODE_DTOR");
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

void comment_copy(void *_dst, void *_src)
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

/* void log_symcomments(UT_array *comments) */
/* { */
/*     struct comment_s *p=NULL; */
/*     while( (p=(struct comment_s*)utarray_next(comments, p))) { */
/*         log_debug("\t'%s'; (ln: %d, col: %d)", */
/*                       p->s, */
/*                       p->line, p->col); */
/*     } */
/* } */

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

