#include "log.h"
#include "utarray.h"
#include "token_debug.h"

const char *token_name[256] = {
[TK_ALIAS] = "TK_ALIAS",
[TK_AMP] = "TK_AMP",
[TK_AMP_EQ] = "TK_AMP_EQ",
[TK_AND] = "TK_AND",
[TK_ARROW] = "TK_ARROW",
[TK_AS] = "TK_AS",
[TK_ASSERT] = "TK_ASSERT",
[TK_BANG] = "TK_BANG",
[TK_BANG_EQ] = "TK_BANG_EQ",
/* [TK_BDQ] = "TK_BDQ", */
[TK_BDQ3] = "TK_BDQ3",
[TK_BREAK] = "TK_BREAK",
/* [TK_BSQ] = "TK_BSQ", */
/* [TK_BSQ3] = "TK_BSQ3", */
[TK_BYTESTR] = "TK_BYTESTR",
[TK_CARET] = "TK_CARET",
[TK_CARET_EQ] = "TK_CARET_EQ",
[TK_CLASS] = "TK_CLASS",
[TK_COLON] = "TK_COLON",
[TK_COMMA] = "TK_COMMA",
[TK_COMMENT] = "TK_COMMENT",
[TK_CONTINUE] = "TK_CONTINUE",
[TK_DEF] = "TK_DEF",
[TK_DEF_STMT] = "TK_DEF_STMT",
[TK_DEL] = "TK_DEL",
[TK_DIVDIV] = "TK_DIVDIV",
[TK_DIVDIV_EQ] = "TK_DIVDIV_EQ",
[TK_DIV_EQ] = "TK_DIV_EQ",
[TK_DOT] = "TK_DOT",
[TK_DQ] = "TK_DQ",
[TK_ELIF] = "TK_ELIF",
[TK_ELSE] = "TK_ELSE",
[TK_EQ] = "TK_EQ",
[TK_EQEQ] = "TK_EQEQ",
[TK_ESC_BACKSLASH] = "TK_ESC_BACKSLASH",
[TK_EXCEPT] = "TK_EXCEPT",
[TK_FINALLY] = "TK_FINALLY",
[TK_FLOAT] = "TK_FLOAT",
[TK_FOR] = "TK_FOR",
[TK_FROM] = "TK_FROM",
[TK_GE] = "TK_GE",
[TK_GLOBAL] = "TK_GLOBAL",
[TK_ID] = "TK_ID",
[TK_IF] = "TK_IF",
[TK_IMPORT] = "TK_IMPORT",
[TK_INT] = "TK_INT",
[TK_IN] = "TK_IN",
[TK_IS] = "TK_IS",
[TK_LAMBDA] = "TK_LAMBDA",
[TK_LANGLE] = "TK_LANGLE",
[TK_LBRACE] = "TK_LBRACE",
[TK_LBRACK] = "TK_LBRACK",
[TK_LE] = "TK_LE",
[TK_LLANGLE] = "TK_LLANGLE",
[TK_LLANGLE_EQ] = "TK_LLANGLE_EQ",
[TK_LOAD] = "TK_LOAD",
[TK_LOAD_STMT] = "TK_LOAD_STMT",
[TK_LPAREN] = "TK_LPAREN",
[TK_MINUS] = "TK_MINUS",
[TK_MINUS_EQ] = "TK_MINUS_EQ",
[TK_NONLOCAL] = "TK_NONLOCAL",
[TK_NOT] = "TK_NOT",
[TK_OR] = "TK_OR",
[TK_PASS] = "TK_PASS",
[TK_PCT] = "TK_PCT",
[TK_PCT_EQ] = "TK_PCT_EQ",
[TK_PLUS] = "TK_PLUS",
[TK_PLUS_EQ] = "TK_PLUS_EQ",
[TK_RAISE] = "TK_RAISE",
[TK_RANGLE] = "TK_RANGLE",
[TK_RBDQ] = "TK_RBDQ",
[TK_RBRACE] = "TK_RBRACE",
[TK_RBRACK] = "TK_RBRACK",
[TK_RBSQ] = "TK_RBSQ",
[TK_RDQ] = "TK_RDQ",
[TK_RETURN] = "TK_RETURN",
[TK_RPAREN] = "TK_RPAREN",
[TK_RRANGLE] = "TK_RRANGLE",
[TK_RRANGLE_EQ] = "TK_RRANGLE_EQ",
[TK_RSQ] = "TK_RSQ",
[TK_SEMI] = "TK_SEMI",
[TK_SLASH] = "TK_SLASH",
[TK_SQ] = "TK_SQ",
[TK_SQ3] = "TK_SQ3",
[TK_STAR] = "TK_STAR",
[TK_STARSTAR] = "TK_STARSTAR",
[TK_STAR_EQ] = "TK_STAR_EQ",
[TK_MLSTRING] = "TK_MLSTRING",
[TK_RAWSTRING] = "TK_RAWSTRING",
[TK_STRING] = "TK_STRING",
[TK_TILDE] = "TK_TILDE",
[TK_TRY] = "TK_TRY",
[TK_VBAR] = "TK_VBAR",
[TK_VBAR_EQ] = "TK_VBAR_EQ",
[TK_WHILE] = "TK_WHILE",
[TK_WITH] = "TK_WITH",
[TK_YIELD] = "TK_YIELD",
[TK_NUMBER] = "TK_NUMBER",
[TK_NEWLINE] = "TK_NEWLINE",
};

void dump_ast(struct obazl_buildfile_s *ast)
{
    log_debug("dump_ast");
    log_debug("fname: %s", ast->fname);

    struct node_s *p;
    for(p=(struct node_s*)utarray_front(ast->nodelist);
        p!=NULL;
        p=(struct node_s*)utarray_next(ast->nodelist, p)) {
        log_debug("node type: %d", p->type);
    }
}

void dump_node(struct node_s *node)
{
    log_debug("dump_node");

    switch(node->type) {
    case TK_ALIAS:
        log_debug("TK_ALIAS %d,%d",
                  node->line, node->col);
        dump_nodes(node->subnodes);
        break;
    case TK_COLON:
        log_debug("TK_COLON: %d, %d", node->line, node->col);
        break;
    case TK_COMMA:
        log_debug("TK_COMMA: %d, %d", node->line, node->col);
        break;
    case TK_COMMENT:
        log_debug("TK_COMMENT %d:%d: %s", node->line, node->col, node->s);
        break;
    case TK_DEF:
        log_debug("TK_DEF %d:%d", node->line, node->col);
        break;
    case TK_DEF_PARAMS:
        log_debug("TK_DEF_PARAMS %d:%d", node->line, node->col);
        if (node->subnodes)
            dump_nodes(node->subnodes);
        log_debug("end TK_DEF_PARAMS");
        break;
    case TK_DEF_STMT:
        log_debug("TK_DEF_STMT %d:%d", node->line, node->col);
        dump_nodes(node->subnodes);
        break;
    case TK_EQ:
        log_debug("TK_EQ %d:%d", node->line, node->col);
        break;
    case TK_FLOAT:
        log_debug("TK_FLOAT %d,%d: %s", node->line, node->col, node->s);
        break;
    case TK_ID:
        log_debug("TK_ID %d,%d: %s", node->line, node->col, node->s);
        break;
    case TK_INT:
        log_debug("TK_INT %d,%d: %s", node->line, node->col, node->s);
        break;
    case TK_LOAD:
        log_debug("TK_LOAD %d:%d", node->line, node->col);
        break;
    case TK_LOAD_STMT:
        log_debug("TK_LOAD_STMT %d:%d", node->line, node->col);
        dump_nodes(node->subnodes);
        break;
    case TK_LPAREN:
        log_debug("TK_LPAREN %d:%d", node->line, node->col);
        break;
    case TK_RPAREN:
        log_debug("TK_RPAREN %d:%d", node->line, node->col);
        break;
    case TK_STAR_ARGS:
        log_debug("TK_STAR_ARGS %d:%d", node->line, node->col);
        if (node->subnodes) dump_nodes(node->subnodes);
        break;
    case TK_STARSTAR_ARGS:
        log_debug("TK_STARSTAR_ARGS %d:%d", node->line, node->col);
        if (node->subnodes) dump_nodes(node->subnodes);
        break;
    case TK_STRING:
        log_debug("TK_STRING (%d:%d): %s",
                  node->line, node->col, node->s);
        break;
    default:
        log_debug("DEFAULT for tok type %d (%d:%d)",
                  node->type,
                  node->line, node->col);
        break;
    }
    if (node->comments) {
        log_debug("dumping comments");
        dump_nodes(node->comments);
        log_debug("end dumping comments");
    }
}

void dump_nodes(UT_array *nodes)
{
    log_debug("dump_nodes: %p", nodes);

    struct node_s *p=NULL;
    while( (p=(struct node_s*)utarray_next(nodes, p))) {
        /* log_debug("type: %d", p->type); */
        switch(p->type) {
        case TK_ALIAS:
            log_debug("TK_ALIAS %d,%d",
                      p->line, p->col);
            dump_nodes(p->subnodes);
            break;
        case TK_COLON:
            log_debug("TK_COLON: %d, %d", p->line, p->col);
            break;
        case TK_COMMA:
            log_debug("TK_COMMA: %d, %d", p->line, p->col);
            break;
        case TK_COMMENT:
            log_debug("TK_COMMENT %d:%d: %s", p->line, p->col, p->s);
            break;
        case TK_DEF:
            log_debug("TK_DEF %d:%d", p->line, p->col);
            break;
        case TK_DEF_PARAMS:
            log_debug("TK_DEF_PARAMS %d:%d", p->line, p->col);
            if (p->subnodes)
                dump_nodes(p->subnodes);
            log_debug("end TK_DEF_PARAMS");
            break;
        case TK_DEF_STMT:
            log_debug("TK_DEF_STMT %d:%d", p->line, p->col);
            dump_nodes(p->subnodes);
            break;
        case TK_EQ:
            log_debug("TK_EQ %d:%d", p->line, p->col);
            break;
        case TK_FLOAT:
            log_debug("TK_FLOAT %d,%d: %s", p->line, p->col, p->s);
            break;
        case TK_ID:
            log_debug("TK_ID %d,%d: %s", p->line, p->col, p->s);
            break;
        case TK_INT:
            log_debug("TK_INT %d,%d: %s", p->line, p->col, p->s);
            break;
        case TK_LOAD:
            log_debug("TK_LOAD %d:%d", p->line, p->col);
            break;
        case TK_LOAD_STMT:
            log_debug("TK_LOAD_STMT %d:%d", p->line, p->col);
            dump_nodes(p->subnodes);
            break;
        case TK_LPAREN:
            log_debug("TK_LPAREN %d:%d", p->line, p->col);
            break;
        case TK_RPAREN:
            log_debug("TK_RPAREN %d:%d", p->line, p->col);
            break;
        case TK_STAR_ARGS:
            log_debug("TK_STAR_ARGS %d:%d", p->line, p->col);
            if (p->subnodes) dump_nodes(p->subnodes);
            break;
        case TK_STARSTAR_ARGS:
            log_debug("TK_STARSTAR_ARGS %d:%d", p->line, p->col);
            if (p->subnodes) dump_nodes(p->subnodes);
            break;
        case TK_STRING:
            log_debug("TK_STRING %d,%d: %s",
                      p->line, p->col, p->s);
            break;
        default:
            log_debug("DEFAULT for tok type %d", p->type);
            break;
        }
        if (p->comments) {
            log_debug("dumping comments");
            dump_nodes(p->comments);
            log_debug("end dumping comments");
        }
    }
}
