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
%token DEF_PARAMS .
%token DEF_STMT .
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
%token LOAD_STMT .
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
%token STRING .
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

%type str_alias { struct node_s* } /* subnodes: ID cmt* EQ cmt* STRING  */
%destructor str_alias {
    log_trace("freeing STR_ALIAS");
    /* utarray_free($$->list); */
}

/* %type comma_str_alias { UT_array* }    /\* list: COMMA str_alias *\/ */
/* %destructor comma_str_alias { */
/*     log_trace("freeing comma_str_alias"); */
/*     /\* utarray_free($$->list); *\/ */
/* } */

%type load_list { UT_array* }    /* list of struct node_s */
%destructor load_list {
    log_trace("freeing load_list");
    /* utarray_free($$->list); */
}

%type cmt_list { UT_array* }    /* list of cmt nodes */
%destructor cmt_list {
    log_trace("freeing cmt_list");
    /* utarray_free($$->list); */
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
%type def_stmt { struct node_s* }
%destructor def_stmt {
    log_trace("freeing def_stmt");
    /* utarray_free($$->list); */
}

%type load_stmt { struct node_s* }
%destructor load_stmt {
    log_trace("freeing load_stmt");
    /* utarray_free($$->list); */
}

%type params { struct node_s* }
%destructor params {
    log_trace("freeing params");
    /* utarray_free($$->list); */
}

%type params_req { UT_array* }
%destructor params {
    log_trace("freeing params");
    /* utarray_free($$->list); */
}

/* suite :: approximately 'body' (of a fn defn, for example) */
/* Suite = [newline indent {Statement} outdent] | SimpleStmt . */
%type suite { struct node_s* }
%destructor suite {
    log_trace("freeing suite");
    /* utarray_free($$->list); */
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
%type xCOLON { struct node_s* }
%destructor xCOLON {
    log_trace("freeing xCOLON");
}

%type xCOMMA { struct node_s* }
%destructor xCOMMA {
    log_trace("freeing xCOMMA");
}

%type xDEF { struct node_s* }
%destructor xDEF {
    log_trace("freeing xDEF");
    /* utarray_free($$->list); */
}

%type xEQ { struct node_s* }
%destructor xEQ {
    log_trace("freeing xEQ");
    /* utarray_free($$->list); */
}

%type xID { struct node_s* }
%destructor xID {
    log_trace("freeing xID");
    /* utarray_free($$->list); */
}

%type xLOAD { struct node_s* }    /* LOAD COMMENT* */
%destructor xLOAD {
    log_trace("freeing xLOAD");
    /* utarray_free($$->list); */
}

%type xLPAREN { struct node_s* }
%destructor xLPAREN {
    log_trace("freeing xLPAREN");
    /* utarray_free($$->list); */
}

%type xRPAREN { struct node_s* }
%destructor xRPAREN {
    log_trace("freeing xRPAREN");
    /* utarray_free($$->list); */
}

%type xSTRING { struct node_s* }
%destructor xSTRING {
    log_trace("freeing xSTRING");
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

%start_symbol nodes_test

/* **************************************************************** */
nodes_test ::= nodelist(NODES) . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>nodes_test ::= nodelist .");
    log_trace("  rhs nodelist (NODES)");
#endif
    dump_nodes(NODES);
}

/* load_stmt_test ::= load_stmt(LS) . { */
/* #if DEBUG_TRACE */
/*     log_trace("\n"); */
/*     log_trace(">>load_stmt_test ::= load_stmt ."); */
/*     log_trace("  rhs load_stmt (LS)"); */
/* #endif */
    /* dump_nodes(NODES); */
/* } */

/* load_list_test ::= load_list(SL) . { */
/* #if DEBUG_TRACE */
/*     log_trace("\n"); */
/*     log_trace(">>load_list_test ::= load_list ."); */
/*     log_trace("  rhs load_list (SL)"); */
/* #endif */
    /* dump_nodes(NODES); */
/* } */

str_alias(ALIAS) ::= xID(IDENT) xEQ(XEQ) xSTRING(S). {
    log_trace(">>str_alias ::= xID xEQ xSTRING");
    log_trace("  rhs xID(IDENT)");
    log_trace("  rhs xEQ(XEQ)");
    log_trace("  rhs xSTRING(S)");
    ALIAS = calloc(sizeof(struct node_s), 1);
    ALIAS->type = TK_ALIAS;
    ALIAS->line  = IDENT->line;
    ALIAS->col   = IDENT->col;
    utarray_new(ALIAS->subnodes, &node_icd);
    utarray_push_back(ALIAS->subnodes, IDENT);
    utarray_push_back(ALIAS->subnodes, XEQ);
    utarray_push_back(ALIAS->subnodes, S);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
xCOLON(XC) ::= COLON(C) . {
    log_trace(">>xCOLON ::= COLON .");
    XC = C;
}

/* xCOLON(XC) ::= COLON(C) cmt_list(CL) . { */
/*     log_trace(">>xCOLON ::= COLON cmt_list"); */
/*     C->comments = CL; */
/*     XC = C; */
/* } */

xCOMMA(XC) ::= COMMA(C) . {
    log_trace(">>xCOMMA ::= COMMA .");
    XC = C;
}

xCOMMA(XC) ::= COMMA(C) cmt_list(CL) . {
    log_trace(">>xCOMMA ::= COMMA cmt_list");
    C->comments = CL;
    XC = C;
}

xDEF(XDEF) ::= DEF(D) . {
    log_trace(">>xDEF ::= DEF .");
    XDEF = D;
}

xDEF(XDEF) ::= DEF(D) cmt_list(CL) . {
    log_trace(">>xDEF ::= DEF cmt_list");
    /* D->comments = CL; */
    XDEF = D;
}

xEQ(XEQ) ::= EQ(Eq) . {
    log_trace(">>xEQ ::= EQ");
    XEQ = Eq;
}

xEQ(XEQ) ::= EQ(Eq) cmt_list(CL) . {
    log_trace(">>xEQ ::= Eq cmt_list");
    log_trace("   EQ(Eq)");
    log_trace("   cmt_list(CL)");
    Eq->comments = CL;
    XEQ = Eq;
}

xID(XID) ::= ID(IDENT) . {
    log_trace(">>xID ::= ID");
    XID = IDENT;
}

xID(XID) ::= ID(IDENT) cmt_list(CL) . {
    log_trace(">>xID ::= ID cmt_list");
    log_trace("   ID(IDENT): %s", IDENT->s);
    log_trace("   cmt_list(CL)");
    IDENT->comments = CL;
    XID = IDENT;
}

xLOAD(XLD) ::= LOAD(LD) . {
    log_trace("");
    log_trace(">>xLOAD ::= LOAD");
    log_trace("  lhs xLOAD (XLD)");
    log_trace("  rhs LOAD (LD)");
    /* utarray_new(NODE, &node_icd); */
    /* utarray_push_back(NODE, LD); */
    XLD = LD;
}

xLOAD(NODE) ::= LOAD(LD) cmt_list(CL) . {
    log_trace("");
    log_trace(">>xLOAD ::= LOAD cmt_list");
    log_trace("  xLOAD (LD)");
    log_trace("  LOAD (LD)");
    log_trace("  cmt_list (CL)");
    LD->comments = CL;
    NODE = LD;
}

xLPAREN(XLP) ::= LPAREN(LP) . {
    log_trace("");
    log_trace(">>xLPAREN ::= LPAREN");
    log_trace("  lhs xLPAREN(NODE)");
    log_trace("  rhs LPAREN(LP)");
    /* utarray_new(NODE, &node_icd); */
    /* utarray_push_back(NODE, LP); */
    XLP = LP;
}

xLPAREN(NODE) ::= LPAREN(LP) cmt_list(CL) . {
    log_trace("");
    log_trace(">>xLPAREN ::= LPAREN cmt_list");
    log_trace("  LPAREN (LP)");
    log_trace("  cmt_list (CL)");
    LP->comments = CL;
    NODE = LP;
}

xRPAREN(XRP) ::= RPAREN(RP) . {
    log_trace("");
    log_trace(">>xRPAREN ::= RPAREN");
    log_trace("  lhs xRPAREN(NODE)");
    log_trace("  rhs RPAREN(RP)");
    XRP = RP;
}

xRPAREN(NODE) ::= RPAREN(RP) cmt_list(CL) . {
    log_trace("");
    log_trace(">>xRPAREN ::= RPAREN cmt_list");
    log_trace("  RPAREN (RP)");
    log_trace("  cmt_list (CL)");
    RP->comments = CL;
    NODE = RP;
}

xSTRING(XSTR) ::= STRING(S) . {
    log_trace("");
    log_trace(">>xSTRING ::= STRING");
    log_trace("  lhs xSTRING (XSTR)");
    XSTR = S;
}

xSTRING(XSTR) ::= STRING(S) cmt_list(CL) . {
    log_trace("");
    log_trace(">>xSTRING ::= STRING cmt_list");
    log_trace("  xSTRING (S)");
    log_trace("  STRING (S)");
    log_trace("  cmt_list (CL)");
    S->comments = CL;
    XSTR = S;
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
load_list(LOAD_LIST) ::= str_alias(A) . {
    log_trace(">>load_list ::= load_list str_alias");
    utarray_new(LOAD_LIST, &node_icd);
    utarray_push_back(LOAD_LIST, A);
}

load_list(LOAD_LIST) ::= xSTRING(XS) . {
    log_trace(">>load_list ::= STRING");
    utarray_new(LOAD_LIST, &node_icd);
    utarray_push_back(LOAD_LIST, XS);
}

load_list(LOAD_LIST) ::= load_list(LL) xCOMMA(XC) xSTRING(XS) . {
    log_trace(">>load_list ::= load_list xCOMMA xSTRING");
    utarray_push_back(LL, XC);
    utarray_push_back(LL, XS);
    LOAD_LIST = LL;
}

load_list(LOAD_LIST) ::= load_list(LL) xCOMMA(XC) str_alias(A) . {
    log_trace(">>load_list ::= load_list xCOMMA str_alias");
    utarray_push_back(LL, XC);
    utarray_push_back(LL, A);
    LOAD_LIST = LL;
}

/* optional trailing comma */
load_list(LOAD_LIST) ::= load_list(LL) xCOMMA(XC) . {
    log_trace(">>load_list ::= load_list xCOMMA");
    utarray_push_back(LL, XC);
    LOAD_LIST = LL;
}

cmt_list(COMMENTS) ::= cmt_list(CL) COMMENT(C) . {
    log_trace(">>cmt_list ::= cmt_list COMMENT");
    utarray_push_back(CL, C);
    COMMENTS = CL;
}

cmt_list(COMMENTS) ::= COMMENT(C) . {
    log_trace(">>cmt_list ::= COMMENT");
    utarray_new(COMMENTS, &node_icd);
    utarray_push_back(COMMENTS, C);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* Suite = [newline indent {Statement} outdent] | SimpleStmt . */
/* suite(SUITE) ::= . */
/* { */
/*     log_trace("\n"); */
/*     log_trace(">>suite"); */
/*     log_trace("  suite(SUITE)"); */
/* } */

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* Parameters = Parameter {',' Parameter}. */
/* Parameter = identifier | identifier '=' Test | '*' identifier | '**' identifier . */

/* id_params(PARAMS) ::= xID(IDPARAM) . */
/* { */
/*     log_trace("\n"); */
/*     log_trace(">>id_params ::= xID"); */
/*     log_trace("  id_params(PARAMS)"); */
/*     log_trace("  xID(IDPARAM)"); */
/* } */

/* id_params(PARAMS) ::= id_params(IDPARAMS) xID(IDPARAM) . */
/* { */
/*     log_trace("\n"); */
/*     log_trace(">>id_params ::= id_params xID"); */
/*     log_trace("  lhs: id_params(PARAMS)"); */
/*     log_trace("  rhs: id_params(IDPARAMS)"); */
/*     log_trace("  rhs: xID(IDPARAM)"); */
/* } */

/* param(PARAM) ::= id_params(IDPARAMS) . */
/* { */
/*     log_trace("\n"); */
/*     log_trace(">>param ::= id_params"); */
/*     log_trace("  lhs: param(PARAM)"); */
/*     log_trace("  rhs: id_params(IDPARAMS)"); */
/* } */

params_req(PARAMS) ::= xID(P) .
{
    log_trace("\n");
    log_trace(">>params_req ::= xID");
    log_trace("  lhs: params_req(PARAMS)");
    log_trace("  rhs: xID(P)");
    utarray_new(PARAMS, &node_icd);
    utarray_push_back(PARAMS, P);
}

params_req(PARAMS) ::= params_req(PREQS) xCOMMA(XC) xID(P) .
{
    log_trace("\n");
    log_trace(">>params_req ::= params_req xCOMMA xID");
    log_trace("  lhs: params_req(PARAMS)");
    log_trace("  rhs: params_req(PREQS)");
    log_trace("  rhs: xCOMMA(XC)");
    log_trace("  rhs: xID(P)");
    utarray_push_back(PREQS, XC);
    utarray_push_back(PREQS, P);
    PARAMS = PREQS;
}

params(PARAMS) ::= params_req(PRS) .
{
    log_trace("\n");
    log_trace(">>params ::= params_req");
    log_trace("  lhs: params(PARAMS)");
    log_trace("  rhs: params_req(PRS)");

    PARAMS = calloc(sizeof(struct node_s), 1);
    PARAMS->type = TK_DEF_PARAMS;
    /* PARAMS->line = PRS->line; */
    /* PARAMS->col  = PRS->col; */
    /* utarray_new(PARAMS->subnodes, &node_icd); */
    /* utarray_push_back(PARAMS, PRS); */
    PARAMS->subnodes = PRS;
    dump_nodes(PARAMS->subnodes);
}

params(PARAMS) ::= .
{
    log_trace("\n");
    log_trace(">>params ::= params_req");
    log_trace("  lhs: params(PARAMS)");

    PARAMS = calloc(sizeof(struct node_s), 1);
    PARAMS->type = TK_DEF_PARAMS;
    /* PARAMS->line = PRS->line; */
    /* PARAMS->col  = PRS->col; */
    /* utarray_new(PARAMS->subnodes, &node_icd); */
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* 'def' defines a function */
/* DefStmt = 'def' identifier '(' [Parameters [',']] ')' ':' Suite . */
def_stmt(STMT) ::= xDEF(XDEF) xID(XID) xLPAREN(XLP) params(Params) xRPAREN(XRP) xCOLON(XColon) .
{
    log_trace("\n");
    log_trace(">>def_stmt ::= xDEF xID xLPAREN params xRPAREN xCOLON xSUITE");
    log_trace("  lhs: def_stmt(STMT)");
    log_trace("  rhs: xDEF(XDEF)");
    log_trace("  rhs: xID(XID): %s", XID->s);
    log_trace("  rhs: xLPAREN(XLP)");
    log_trace("  rhs: params(Params)");
    log_trace("  rhs: xRPAREN(XRP)");
    log_trace("  rhs: xCOLOR(XColon)");
    /* /\* log_trace("  rhs: suite(S)"); *\/ */

    STMT = calloc(sizeof(struct node_s), 1);
    STMT->type = TK_DEF_STMT;
    STMT->line = XDEF->line;
    STMT->col  = XDEF->col;
    utarray_new(STMT->subnodes, &node_icd);

    utarray_push_back(STMT->subnodes, XDEF);
    utarray_push_back(STMT->subnodes, XID);
    utarray_push_back(STMT->subnodes, XLP);
    utarray_push_back(STMT->subnodes, Params);
    /* utarray_push_back(STMT->subnodes, XC); */
    /* utarray_concat(STMT->subnodes, LL); */
    utarray_push_back(STMT->subnodes, XRP);
    utarray_push_back(STMT->subnodes, XColon);
}

/* LoadStmt = 'load' '(' string {',' [identifier '='] string} [','] ')' . */
load_stmt(STMT) ::= xLOAD(XLD) xLPAREN(XLP) xSTRING(XS) xCOMMA(XC) load_list(LL) RPAREN(RP) . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>load_stmt ::= xLOAD xLPAREN xSTRING xCOMMA load_list xRPAREN");
    log_trace("  load_stmt (STMT)");
    log_trace("  xLOAD (XLD)");
    log_trace("  xLPAREN (XLP)");
    log_trace("  xCOMMA (XC)");
    log_trace("  xSTRING (XS): %s", XS->s);
    log_trace("  load_list (LL)");
    log_trace("  xRPAREN (RP)");
#endif
    STMT = calloc(sizeof(struct node_s), 1);
    STMT->type = TK_LOAD_STMT;
    STMT->line  = STMT->line;
    STMT->col   = STMT->col;
    utarray_new(STMT->subnodes, &node_icd);

    /* utarray_push_back(STMT->subnodes, LD); */
    utarray_push_back(STMT->subnodes, XLD);
    utarray_push_back(STMT->subnodes, XLP);
    utarray_push_back(STMT->subnodes, XS);
    utarray_push_back(STMT->subnodes, XC);
    utarray_concat(STMT->subnodes, LL);
    utarray_push_back(STMT->subnodes, RP);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
nodelist(NODES) ::= nodelist(PREVNODES) load_stmt(LOAD) . {
    log_trace(">>nodelist ::= nodelist load_stmt");
    log_trace("  nodelist lhs(NODES)");
    log_trace("  nodelist rhs(PREVNODES)");
    log_trace("  load_stmt (LOAD)");
    utarray_push_back(PREVNODES, LOAD);
    NODES = PREVNODES;
}

nodelist(NODES) ::= load_stmt(LOAD) . {
    log_trace(">>nodelist ::= load_stmt .");
    log_trace("  nodelist lhs load_stmt(LOAD)");
    utarray_new(NODES, &node_icd);
    utarray_push_back(NODES, LOAD);
}

nodelist(NODES) ::= nodelist(PREVNODES) def_stmt(LOAD) . {
    log_trace(">>nodelist ::= nodelist def_stmt");
    log_trace("  nodelist lhs(NODES)");
    log_trace("  nodelist rhs(PREVNODES)");
    log_trace("  def_stmt (LOAD)");
    utarray_push_back(PREVNODES, LOAD);
    NODES = PREVNODES;
}

nodelist(NODES) ::= def_stmt(DEF) . {
    log_trace(">>nodelist ::= def_stmt .");
    log_trace("  nodelist lhs def_stmt(DEF)");
    utarray_new(NODES, &node_icd);
    utarray_push_back(NODES, DEF);
}

nodelist(NODES) ::= COMMENT(C) . {
    log_trace(">>nodelist ::= COMMENT .");
    utarray_new(NODES, &node_icd);
    utarray_push_back(NODES, C);
}

nodelist(NODES) ::= nodelist(PREVNODES) COMMENT(C) . {
    log_trace(">>nodelist ::= nodelist COMMENT");
    log_trace("  nodelist lhs(NODES)");
    log_trace("  nodelist rhs(PREVNODES)");
    log_trace("  COMMENT (C)");
    utarray_push_back(PREVNODES, C);
    NODES = PREVNODES;
}
