%include {
#include <setjmp.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "utarray.h"
#include "log.h"

static int indent = 2;
static int delta = 2;
static char *sp = " ";

#if INTERFACE
#ifndef YYMALLOCARGTYPE
#define YYMALLOCARGTYPE size_t
#endif
#endif
}

%token_prefix TK_

/* TK_ prefix will be added by lemon */
%token AMP .
%token AMP_EQ .
%token AND .
%token ARROW .
%token AS .
%token ASSERT .
%token BANG .
%token BANG_EQ .
%token BDQ .
%token BDQ3 .
%token BREAK .
%token BSQ .
%token BSQ3 .
%token CARET .
%token CARET_EQ .
%token CLASS .
%token COLON .
%token COMMA .
%token COMMENT .
%token CONTINUE .
%token DEF .
%token DEL .
%token DIVDIV .
%token DIVDIV_EQ .
%token DIV_EQ .
%token DOT .
%token DQ .
%token ELIF .
%token ELSE .
%token EQ .
%token EQEQ .
%token ESC_BACKSLASH .
%token EXCEPT .
%token FINALLY .
%token FOR .
%token FROM .
%token GE .
%token GLOBAL .
%token ID .
%token IF .
%token IMPORT .
%token IN .
%token IS .
%token LAMBDA .
%token LANGLE .
%token LBRACE .
%token LBRACK .
%token LE .
%token LLANGLE .
%token LLANGLE_EQ .
%token LOAD .
%token LPAREN .
%token MINUS .
%token MINUS_EQ .
%token NEWLINE .
%token NONLOCAL .
%token NOT .
%token NUMBER .
%token OR .
%token PASS .
%token PCT .
%token PCT_EQ .
%token PLUS .
%token PLUS_EQ .
%token RAISE .
%token RANGLE .
%token RBDQ .
%token RBRACE .
%token RBRACK .
%token RBSQ .
%token RDQ .
%token RETURN .
%token RPAREN .
%token RRANGLE .
%token RRANGLE_EQ .
%token RSQ .
%token SEMI .
%token SLASH .
%token SQ .
%token SQ3 .
%token STAR .
%token STARSTAR .
%token STAR_EQ .
%token DQSTRING .
%token TILDE .
%token TRY .
%token VBAR .
%token VBAR_EQ .
%token WHILE .
%token WITH .
%token YIELD .
/* token constants used to tag non-terminals */
%token ALIAS .
%token SYM .

%extra_argument { struct obazl_buildfile_s **ast}

/* **************** */
%token_type { struct node_s* }

%type buildfile { struct obazl_buildfile_s* }
%destructor node {
    log_trace("freeing obazl_buildfile");
    free($$);
}

%type node { struct node_s* }
%destructor node {
    log_trace("freeing obazl_buildfile_node");
    free($$);
}

%type nodelist { UT_array* }
%destructor nodelist {
    log_trace("freeing nodelist");
    /* utarray_free($$->list); */
}

%type alias_node { struct node_s* }
%destructor alias_node {
    log_trace("freeing alias_node");
    /* utarray_free($$->list); */
}

%type sym_list { UT_array* }    /* list of struct node_s */
%destructor sym_list {
    log_trace("freeing sym_list");
    /* utarray_free($$->list); */
}

%type cmt_list { UT_array* }    /* list of struct node_s ???? */
%destructor cmt_list {
    log_trace("freeing cmt_list");
    /* utarray_free($$->list); */
}

%syntax_error {
    log_trace("**************** Syntax error! ****************");
    exit(EXIT_FAILURE);
}

%parse_failure {
    log_trace("\n\n\t\t%%%%%%%%%%%%%%%% PARSE: FAIL %%%%%%%%%%%%%%%%\n");
}

%parse_accept {
    log_trace("\n\n\t\t%%%%%%%%%%%%%%%% PARSE: SUCCESS %%%%%%%%%%%%%%%%\n");
}

%start_symbol symlist_test

/* **************************************************************** */
/* buildfile(BLDFILE) ::= nodelist(NODES) . { */
/* #if DEBUG_TRACE */
/*     log_trace("\n"); */
/*     log_trace(">>buildfile ::= nodelist"); */
/*     log_trace("  buildfile (BLDFILE)"); */
/*     log_trace("  nodelist (NODES)"); */
/*     /\* dump_nodes(0, NODES); *\/ */
/* #endif */
/*     /\* BLDFILE = (struct obazl_buildfile_s*)calloc(sizeof(struct obazl_buildfile_s), 1); *\/ */
/*     (*ast)->nodelist = NODES; */
/* #if DEBUG_TRACE */
/*     /\* dump_buildfile(0, PKG); *\/ */
/*     /\* *ast = BLDFILE; *\/ */
/* #endif */
/* } */

/* **************************************************************** */
symlist_test ::= sym_list(SL) COMMA . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>symlist_test ::= sym_list COMMA .");
    log_trace("  rhs STRINGLIST (SL)");
#endif
    struct node_s *p=NULL;
    while( (p=(struct node_s*)utarray_next(SL, p))) {
        switch(p->type) {
        case TK_ALIAS:
            log_debug("dump alias");
            /* log_debug("alias: '%s' (%d, %d) = '%s' (%d, %d)", */
            /*           p->alias->alias.s? p->alias->alias.s : "", */
            /*           p->alias->alias.line, p->alias->alias.col, */
            /*           p->alias->sym.s, */
            /*           p->alias->sym.line, p->alias->sym.col); */
            break;
        case TK_COMMA:
            log_debug("COMMA: %d, %d", p->line, p->col);
            break;
        case TK_COMMENT:
            log_debug("COMMENT: %s (ln: %d, col: %d)",
                      p->comment->s,
                      p->comment->line, p->comment->col);
            break;
        case TK_LOAD:
            log_debug("LOAD");
            break;
        case TK_SYM:
            log_debug("SYM: %s", p->sym->sym);
            break;
        }
    }
}

symlist_test ::= sym_list(SL) . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>symlist_test ::= sym_list .");
    log_trace("  rhs sym_list (SL)");
#endif
    struct node_s *p=NULL;
    while( (p=(struct node_s*)utarray_next(SL, p))) {
        switch(p->type) {
        case TK_ALIAS:
            log_debug("TK_ALIAS");
            /* log_debug("dump alias: %p, %d", */
            /*           p->subnodes, */
            /*           utarray_len(p->subnodes)); */
            struct node_s *subnode=NULL;
            int subnodes_len = utarray_len(p->subnodes);
            for(int i=0; i<subnodes_len; i++) {
                subnode = utarray_eltptr(p->subnodes, i);
                switch(subnode->type) {
                case TK_ALIAS:
                    log_debug("SUBNODE_ALIAS %d,%d",
                              subnode->line, subnode->col);
                    break;
                case TK_COMMA:
                    log_debug("SUBNODE_COMMA %d:%d",
                              subnode->line, subnode->col);
                    break;
                case TK_COMMENT:
                    log_debug("SUBNODE_COMMENT %d:%d",
                              subnode->line, subnode->col);
                    break;
                case TK_EQ:
                    log_debug("SUBNODE_EQ %d:%d",
                              subnode->line, subnode->col);
                    break;
                case TK_ID:
                    log_debug("SUBNODE_ID %d,%d: %s",
                              subnode->line, subnode->col, subnode->s);
                    break;
                case TK_LOAD:
                    log_debug("SUBNODE_LOAD %d:%d",
                              subnode->line, subnode->col);
                    /* should not happen */
                    break;
                case TK_SYM:
                    log_debug("SUBNODE_SYM %d,%d: %s",
                              subnode->line, subnode->col, subnode->s);
                    break;
                default:
                    log_debug("SUBNODE DEFAULT");
                    break;
                }
            }
            break;
        case TK_COMMA:
            log_debug("COMMA: %d, %d", p->line, p->col);
            break;
        case TK_COMMENT:
            log_debug("COMMENT: %s", p->comment->s);
            break;
        case TK_LOAD:
            log_debug("LOAD");
            break;
        case TK_SYM:
            log_debug("SYM: %s", p->sym->sym);
            break;
        }
    }
}

sym_list(SYMLIST) ::= sym_list(SL) cmt_list(CL1) COMMA(C) cmt_list(CL2) DQSTRING(A) . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>sym_list ::= sym_list cmt_list COMMA cmt_list DQSTRING");
    log_trace("  sym_list (SYMLIST)");
    log_trace("  rhs: cmt_list (CL)");
    log_trace("  rhs: DQSTRING (A): %s", A->s);
    log_trace("  rhs: STRINGLIST (SL)");
#endif
    utarray_concat(SL, CL1);

    struct node_s *comma_node = calloc(sizeof(struct node_s), 1);
    comma_node->type = TK_COMMA;
    comma_node->line  = C->line;
    comma_node->col   = C->col;
    utarray_push_back(SL, comma_node);

    utarray_concat(SL, CL2);

    struct node_s *node = calloc(sizeof(struct node_s), 1);
    node->type = TK_SYM;
    node->line  = A->line;
    node->col   = A->col;

    struct symdecl_s *sym = calloc(sizeof(struct symdecl_s), 1);
    sym->line  = A->line;
    sym->col   = A->col;
    sym->sym = A->s;            /* just copy the ptr? */
    node->sym  = sym;
    utarray_push_back(SL, node);
    SYMLIST = SL;
}

sym_list(SYMLIST) ::= sym_list(SL) COMMA(C) cmt_list(CL) DQSTRING(A) . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>sym_list ::= sym_list cmt_list COMMA cmt_list DQSTRING");
    log_trace("  sym_list (SYMLIST)");
    log_trace("  rhs: cmt_list (CL)");
    log_trace("  rhs: DQSTRING (A): %s", A->s);
#endif
    struct node_s *comma_node = calloc(sizeof(struct node_s), 1);
    comma_node->type = TK_COMMA;
    comma_node->line  = C->line;
    comma_node->col   = C->col;
    utarray_push_back(SL, comma_node);

    utarray_concat(SL, CL);

    struct node_s *node = calloc(sizeof(struct node_s), 1);
    node->type = TK_SYM;
    node->line  = A->line;
    node->col   = A->col;

    struct symdecl_s *sym = calloc(sizeof(struct symdecl_s), 1);
    sym->line  = A->line;
    sym->col   = A->col;
    sym->sym = A->s;            /* just copy the ptr? */
    node->sym  = sym;
    utarray_push_back(SL, node);
    SYMLIST = SL;
}

sym_list(SYMLIST) ::= sym_list(SL) cmt_list(CL) COMMA DQSTRING(A) . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>sym_list ::= sym_list cmt_list COMMA DQSTRING");
    log_trace("  sym_list (SYMLIST)");
    log_trace("  rhs: cmt_list (CL)");
    log_trace("  rhs: DQSTRING (A): %s", A->s);
    log_trace("  rhs: STRINGLIST (SL)");
#endif
    struct symdecl_s *sym = calloc(sizeof(struct symdecl_s), 1);
    sym->sym = A->s;            /* just copy the ptr? */
    utarray_push_back(SL, sym);
    SYMLIST = SL;
}

sym_list(SYMLIST) ::= sym_list(SL) COMMA(C) DQSTRING(A) . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>sym_list ::= sym_list COMMA DQSTRING");
    log_trace("  sym_list (SYMLIST)");
    log_trace("  DQSTRING (A): %s", A->s);
    log_trace("  rhs STRINGLIST (SL)");
#endif
    struct node_s *comma_node = calloc(sizeof(struct node_s), 1);
    comma_node->type = TK_COMMA;
    comma_node->line  = C->line;
    comma_node->col   = C->col;
    utarray_push_back(SL, comma_node);

    struct node_s *node = calloc(sizeof(struct node_s), 1);
    node->type = TK_SYM;
    node->line  = A->line;
    node->col   = A->col;

    struct symdecl_s *sym = calloc(sizeof(struct symdecl_s), 1);
    sym->line  = A->line;
    sym->col   = A->col;
    sym->sym = A->s;            /* just copy the ptr? */
    node->sym  = sym;
    utarray_push_back(SL, node);

    SYMLIST = SL;
}

sym_list(SYMLIST) ::= sym_list(SL) COMMA(C) alias_node(ALIAS) . {
 //ID(IDENT) EQ DQSTRING(S). {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>sym_list ::= sym_list COMMA alias_node");
    log_trace("  lhs sym_list (SYMLIST)");
    log_trace("  rhs sym_list (SL)");
    log_trace("  rhs COMMA (C)");
    log_trace("  rhs alias_node (ALIAS)");
#endif
    struct node_s *comma_node = calloc(sizeof(struct node_s), 1);
    comma_node->type = TK_COMMA;
    comma_node->line  = C->line;
    comma_node->col   = C->col;
    utarray_push_back(SL, comma_node);
    utarray_push_back(SL, ALIAS);
    SYMLIST = SL;
}

alias_node(ALIAS_NODE) ::= ID(IDENT) EQ(Eq) DQSTRING(S). {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>alias_node ::= ID EQ DQSTRING");
    log_trace(" lhs alias_node(ALIAS_NODE)");
    log_trace(" rhs ID(IDENT): %s", IDENT->s);
    log_trace(" rhs DQSTRING(S): %s", S->s);
#endif
    /* utarray_new(ALIAS, &node_icd); */
    ALIAS_NODE = calloc(sizeof(struct node_s), 1);
    ALIAS_NODE->type = TK_ALIAS;
    ALIAS_NODE->line = IDENT->line;
    ALIAS_NODE->col  = IDENT->col;

    /* struct node_s *alias = calloc(sizeof(struct node_s), 1); */
    /* alias->line  = IDENT->line; */
    /* alias->col   = IDENT->col; */
    utarray_new(ALIAS_NODE->subnodes, &node_icd);

    /* struct node_s *id_node = calloc(sizeof(struct node_s), 1); */
    /* id_node->type = TK_ID; */
    /* id_node->line = IDENT->line; */
    /* id_node->col  = IDENT->col; */
    /* id_node->s    = strdup(IDENT->s); */
    utarray_push_back(ALIAS_NODE->subnodes, IDENT); // id_node);

    struct node_s *eq_node = calloc(sizeof(struct node_s), 1);
    eq_node->type = TK_EQ;
    eq_node->line = Eq->line;
    eq_node->col  = Eq->col;
    utarray_push_back(ALIAS_NODE->subnodes, eq_node);

    struct node_s *sym_node = calloc(sizeof(struct node_s), 1);
    sym_node->type = TK_SYM;
    sym_node->line = S->line;
    sym_node->col  = S->col;
    sym_node->s    = strdup(S->s);
    utarray_push_back(ALIAS_NODE->subnodes, sym_node);

    /* ALIAS_NODE->alias = alias; */
}

sym_list(SL) ::= DQSTRING(A). {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>sym_list ::= DQSTRING .");
    log_trace("  sym_list (SL)");
    log_trace("  DQSTRING (A): %s", A->s);
#endif
    utarray_new(SL, &node_icd);
    struct node_s *node = calloc(sizeof(struct node_s), 1);
    node->type = TK_SYM;
    node->line  = A->line;
    node->col   = A->col;

    struct symdecl_s *sym = calloc(sizeof(struct symdecl_s), 1);
    sym->line  = A->line;
    sym->col   = A->col;
    sym->sym   = A->s;

    node->sym = sym;
    utarray_push_back(SL, node);
}

sym_list(SL) ::= alias_node(ALIAS) . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>sym_list ::= alias_node .");
    log_trace("  sym_list (SL)");
    log_trace("  rhs alias_node(ALIAS)"); //: %s", IDENT->s);
    /* log_trace("  DQSTRING (A): %s", A->s); */
#endif
    utarray_new(SL, &node_icd);
    /* struct node_s *node = calloc(sizeof(struct node_s), 1); */
    /* node->type = TK_ALIAS; */
    /* node->line  = ALIAS->line; */
    /* node->col   = ALIAS->col; */

    /* node->alias = sym; */
    utarray_push_back(SL, ALIAS);
}

cmt_list(COMMENTS) ::= cmt_list(CL) COMMENT(C) . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>cmt_list ::= cmt_list COMMENT");
    log_trace("  cmt_list (COMMENTS)");
    log_trace("  rhs cmt_list (CL)");
    log_trace("  COMMENT (C): %s", C->s);
#endif
    struct node_s *node = calloc(sizeof(struct node_s), 1);
    node->type = TK_COMMENT;
    node->line  = C->line;
    node->col   = C->col;

    struct comment_s *cmt = calloc(sizeof(struct comment_s), 1);
    cmt->line = C->line;
    cmt->col  = C->col;
    cmt->s    = strdup(C->s);
    node->comment = cmt;
    utarray_push_back(CL, node); /* NB: push_back copies node */

    COMMENTS = CL;
}

cmt_list(COMMENTS) ::= COMMENT(C) . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>cmt_list ::= COMMENT");
    log_trace("  cmt_list (COMMENTS)");
    log_trace("  COMMENT (C): '%s'", C->s);
#endif
    utarray_new(COMMENTS, &node_icd);
    struct node_s *node = calloc(sizeof(struct node_s), 1);
    node->type = TK_COMMENT;
    node->line  = C->line;
    node->col   = C->col;

    struct comment_s *cmt = calloc(sizeof(struct comment_s), 1);
    cmt->line = C->line;
    cmt->col  = C->col;
    cmt->s    = strdup(C->s);
    node->comment = cmt;
    utarray_push_back(COMMENTS, node); /* NB: push_back copies node */
}

/* nodelist(NODES) ::= node(NODE) . { */
/* #if DEBUG_TRACE */
/*     log_trace("\n"); */
/*     log_trace(">>nodelist ::= node"); */
/*     log_trace("%*snodelist lhs (NODES)", indent, sp); */
/*     log_trace("%*snode (NODE)", indent, sp); */
/*     log_trace("%*snode type: %d", indent, sp, NODE->type); */
/*     /\* dump_node(indent, NODE); *\/ */
/* #endif */
/*     /\* lhs NODELIST: lemon only allocates the type (a ptr); we must allocate/init the struct *\/ */
/*     utarray_new(NODES, &node_icd); */
/*     utarray_push_back(NODES, NODE); */
/* #if DEBUG_TRACE */
/*     /\* dump_nodes(indent, NODES); *\/ */
/* #endif */
/* } */

/* nodelist(NODES) ::= nodelist(PREVNODES) node(NODE) . { */
/* #if DEBUG_TRACE */
/*     log_trace("\n"); */
/*     log_trace(">>nodelist ::= nodelist node"); */
/*     log_trace("%*snodelist lhs(NODES)", indent, sp); */
/*     log_trace("%*snodelist rhs(PREVNODES)", indent, sp); */
/*     /\* dump_nodes(delta+indent, PREVNODES); *\/ */
/*     log_trace("%*snode (NODE)", indent, sp); //, obzl_meta_nodes_count(PREVNODES)); */
/*     /\* dump_node(delta+indent, NODE); *\/ */
/* #endif */
/*     utarray_push_back(PREVNODES, NODE); */
/*     NODES = PREVNODES; */
/*     /\* log_trace("after normalization"); *\/ */
/* #if DEBUG_TRACE */
/*     /\* log_trace("//nodes ::= nodes node DONE"); *\/ */
/*     /\* dump_nodes(delta+indent, NODES); *\/ */
/* #endif */
/* } */

/* **************************************************************** */
/* LoadStmt = 'load' '(' string {',' [identifier '='] string} [','] ')' . */

/* node(NODE) ::= LOAD LPAREN DQSTRING(A) COMMA DQSTRING(B) RPAREN . { */
/* #if DEBUG_TRACE */
/*     log_trace("\n"); */
/*     log_trace(">>node ::= LOAD LPAREN DQSTRING COMMA DQSTRING RPAREN"); */
/*     log_trace("  node (NODE)"); */
/*     log_trace("  libstr (A): %s", A->s); */
/*     log_trace("  symstr (B): %s", B->s); */
/*     /\* if (W) *\/ */
/*     /\*     log_trace("  WORD (W): %s", W->s); *\/ */
/*     /\* else *\/ */
/*     /\*     log_trace("  WORD (W): <empty>", W->s); *\/ */
/* #endif */
/*     NODE = (struct node_s*)calloc(sizeof(struct node_s), 1); */
/*     NODE->type = TK_LOAD; */
/*     /\* NODE = handle_simple_prop(VAR, OPCODE, W); *\/ */
/*     /\* dump_node(indent, NODE); *\/ */
/* } */

/* node(NODE) ::= LOAD LPAREN DQSTRING(A) COMMA sym_list(B) RPAREN . { */
/* #if DEBUG_TRACE */
/*     log_trace("\n"); */
/*     log_trace(">>node ::= LOAD LPAREN DQSTRING COMMA sym_list RPAREN"); */
/*     log_trace("  node (NODE)"); */
/*     log_trace("  DQSTRING (A): %s", A->s); */
/*     log_trace("  sym_list (B)"); */
/*     /\* if (W) *\/ */
/*     /\*     log_trace("  WORD (W): %s", W->s); *\/ */
/*     /\* else *\/ */
/*     /\*     log_trace("  WORD (W): <empty>", W->s); *\/ */
/* #endif */
/*     NODE = (struct node_s*)calloc(sizeof(struct node_s), 1); */
/*     NODE->type = TK_LOAD; */
/*     /\* NODE = handle_simple_prop(VAR, OPCODE, W); *\/ */
/*     /\* dump_node(indent, NODE); *\/ */
/* } */

/* /\* **************************************************************** *\/ */
/* node(NODE) ::= COMMENT . { */
/* #if DEBUG_TRACE */
/*     log_trace("\n"); */
/*     log_trace(">>node ::= COMMENT"); */
/*     /\* log_trace("text: ", C->str); *\/ */
/* #endif */
/*     NODE = (struct node_s*)calloc(sizeof(struct node_s), 1); */
/*     NODE->type = TK_COMMENT; */

/* #if DEBUG_TRACE */
/*     /\* dump_node(0, NODE); *\/ */
/* #endif */
/* } */

