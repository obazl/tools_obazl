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
#define YYCOVERAGE
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
%token BREAK .
/* BSTRING below */
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
%token SLASH2 .                 /* floored division binop */
%token SLASH2_EQ .
%token DIV_EQ .
%token DOT .
%token DQ .
%token ELIF .
%token ELSE .
%token EQ .
%token EQ2 .
%token ESC_BACKSLASH .
%token EXCEPT .
%token FINALLY .
%token FLOAT_LIT .
%token FLOAT .
%token FOR .
%token FROM .
%token GE .
%token GLOBAL .
%token ID .
/* ID_STAR - params & args */
/* ID_STAR2 - params & args */
%token IF .
%token IMPORT .
%token INT .
%token INT_DEC .
%token INT_HEX .
%token INT_OCT .
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
%token PCT .                    /* remainder binop */
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
%token STAR_ARGS .
%token STARSTAR .
%token STARSTAR_ARGS .
%token STAR_EQ .
%token STRING .
%token BSTRING .
%token BLANK .                  /* blank line */
%token BRSTRING .
%token RSTRING .
%token RBSTRING .
%token MLSTRING .
%token MLBSTRING .
%token MLRSTRING .
%token MLBRSTRING .
%token MLRBSTRING .
%token TILDE .
%token TRY .
%token VBAR .
%token VBAR_EQ .
%token WHILE .
%token WITH .
%token YIELD .
/* token constants used to tag non-terminals */
%token ALIAS .
%token Arg_List .
%token Arg_Named .
%token Bin_Expr .
%token Call_Expr .
%token Primary_Expr .
%token Unary_Expr .
%token Dot_Sfx .
%token Call_Sfx .
%token Slice_Sfx .
%token SYM .

%extra_argument { struct obazl_buildfile_s **ast}

/* Python op precedence: */
/* https://docs.python.org/3/reference/expressions.html#operator-precedence */
 /* https://www.mathcs.emory.edu/~valerie/courses/fall10/155/resources/op_precedence.html */
%right LAMBDA .
%nonassoc OR .
%nonassoc AND .
%right NOT .
/* membership: in, not in */
/* identity: is, is not */
/* comparison: <, <=,  >,  >=, !=, == ('<>' not supported)*/
/* NB: 'not in', 'is not' are non-terminals */
%nonassoc IN IS LT LE GT GE BANG_EQ EQ2 .
%nonassoc VBAR .                /* bitwise OR binop */
%nonassoc CARET .               /* bitwise XOR binop */
%nonassoc AMP .                 /* bitwise AND binop */
%nonassoc LLANGLE RRANGLE .     /* bitwise shift binops */
%left PLUS MINUS .
%nonassoc STAR SLASH SLASH2 PCT .
/* %right PLUS .                   /\* positive, e.g. +3 *\/ */
/* %right MINUS .                  /\* negative, e.g. -3 *\/ */
%right TILDE .                  /* bitwise NOT (unary) */
%right STAR2 .                  /* exponentiation */
%nonassoc IF ELSE . /* may go both ways: foo IF bar ELSE baz v. IF foo ELSE bar */
/* %right ELSE . */
/* binops: all nonassoc, mult ops higher precedence than add ops */
/* unary ops: +, -, not, ~ */

/* **************** */
%token_type { struct node_s* }

%type buildfile { struct obazl_buildfile_s* }
%destructor node {
    log_trace("freeing obazl_buildfile");
    free($$);
}

%type binop { struct node_s* }
%destructor binop {
    log_trace("freeing binop");
    free($$);
}

%type expr { struct node_s* }
%destructor expr {
    log_trace("freeing expr");
    free($$);
}

%type primary_expr { struct node_s* }
%destructor primary_expr {
    log_trace("freeing primary_expr");
    free($$);
}

%type unary_expr { struct node_s* }
%destructor unary_expr {
    log_trace("freeing unary_expr");
    free($$);
}

%type binary_expr { struct node_s* }
%destructor binary_expr {
    log_trace("freeing binary_expr");
    free($$);
}

%type arg_list { struct node_s* }
%destructor arg_list {
    log_trace("freeing arg_list");
    /* utarray_free($$->list); */
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
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

%type str_assign { struct node_s* } /* subnodes: ID cmt* EQ cmt* STRING  */
%destructor str_assign {
    log_trace("freeing STR_ASSIGN");
    /* utarray_free($$->list); */
}

/* optional def param: x=val */
%type param_opt { struct node_s* }
%destructor param_opt {
    log_trace("freeing param_opt");
}

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

%type primx { struct node_s* }
%destructor primx {
    log_trace("freeing primx");
}

%type params { struct node_s* }
%destructor params {
    log_trace("freeing params");
}

%type params_opt { UT_array* }
%destructor params_opt {
    log_trace("freeing params_opt");
}

%type params_req { UT_array* }
%destructor params_req {
    log_trace("freeing params_req");
}

%type params_star { UT_array* }
%destructor params_star {
    log_trace("freeing params_star");
}

/* suite :: approximately 'body' (of a fn defn, for example) */
/* Suite = [newline indent {Statement} outdent] | SimpleStmt . */
/* we rename to 'indent_block' */
%type indent_block { struct node_s* }
%destructor indent_block {
    log_trace("freeing indent_block");
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

%start_symbol buildfile
/* File = {Statement | newline} eof . */
buildfile(File) ::= statement_list(SS) .
{
    log_trace("\n");
    log_trace(">>buildfile ::= statement_list .");
    log_trace("  lhs file(File)");
    log_trace("  rhs statement_list(SS)");
}

%ifdef TEST
buildfile(File) ::= primary_expr(X) . {
    log_trace(">>buildfile(File) ::= primary_expr(X) .");
    log_debug("START dump");
    dump_node(X);
    log_debug("/START dump");
}

buildfile(File) ::= unary_expr(X) . {
    log_trace(">>buildfile(File) ::= unary_expr(X) .");
    log_debug("START dump");
    dump_node(X);
    log_debug("/START dump");
}

buildfile(File) ::= binary_expr(X) . {
    log_trace(">>buildfile(File) ::= binary_expr(X) .");
    log_debug("START dump");
    dump_node(X);
    log_debug("/START dump");
}
%endif

%ifdef STRINGS || ALL
%type string_list { UT_array* }
%destructor string_list {
    log_trace("freeing string_list");
    /* utarray_free($$->list); */
}

/* buildfile(File) ::= string_list(NODES) . { */
/* #if DEBUG_TRACE */
/*     log_trace("\n"); */
/*     log_trace(">>buildfile(File) ::= string_list ."); */
/*     log_trace("  rhs string_list (NODES)"); */
/* #endif */
/*     log_debug("START dump"); */
/*     dump_nodes(NODES); */
/*     log_debug("/START dump"); */
/* } */

/* string_list(NODES) ::= STRING(S) . { */
/*     log_trace(">>string_list ::= STRING ."); */
/*     log_trace("  rhs STRING(S)[%d]: %s", S->type, S->s); */
/*     utarray_new(NODES, &node_icd); */
/*     utarray_push_back(NODES, S); */
/* } */

/* string_list(NODES) ::= string_list(PREVNODES) STRING(S) . { */
/*     log_trace(">>string_list ::= nodelist STRING"); */
/*     log_trace("  string_list lhs(NODES)"); */
/*     log_trace("  string_list rhs(PREVNODES)"); */
/*     log_trace("  STRING (S)"); */
/*     utarray_push_back(PREVNODES, S); */
/*     NODES = PREVNODES; */
/* } */
%endif

%if NODES
nodes_test ::= nodelist(NODES) . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>nodes_test ::= nodelist .");
    log_trace("  rhs nodelist (NODES)");
#endif
    dump_nodes(NODES);
}
%endif

/* **************************************************************** */
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

%if LOAD_STMT
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* str_assign is for load stmts */
str_assign(ALIAS) ::= ID(Id) EQ(Eq) STRING(S). {
    log_trace(">>str_assign ::= ID EQ STRING");
    log_trace("  rhs ID(Id)");
    log_trace("  rhs EQ(Eq)");
    log_trace("  rhs STRING(S)");
    ALIAS = calloc(sizeof(struct node_s), 1);
    ALIAS->type = TK_ALIAS;
    ALIAS->line  = Id->line;
    ALIAS->col   = Id->col;
    utarray_new(ALIAS->subnodes, &node_icd);
    utarray_push_back(ALIAS->subnodes, Id);
    utarray_push_back(ALIAS->subnodes, Eq);
    utarray_push_back(ALIAS->subnodes, S);
}

/* param_opt is only for optional def params, i.e. name=expr */
/* param_opt(PARAM) ::= xID(IDENT) xEQ(XEQ) expr(S). { */
/*     log_trace(">>str_assign ::= xID xEQ xSTRING"); */
/*     log_trace("  rhs xID(IDENT)"); */
/*     log_trace("  rhs xEQ(XEQ)"); */
/*     log_trace("  rhs xSTRING(S)"); */
/*     ALIAS = calloc(sizeof(struct node_s), 1); */
/*     ALIAS->type = TK_ALIAS; */
/*     ALIAS->line  = IDENT->line; */
/*     ALIAS->col   = IDENT->col; */
/*     utarray_new(ALIAS->subnodes, &node_icd); */
/*     utarray_push_back(ALIAS->subnodes, IDENT); */
/*     utarray_push_back(ALIAS->subnodes, XEQ); */
/*     utarray_push_back(ALIAS->subnodes, S); */
/* } */

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
load_list(LOAD_LIST) ::= str_assign(A) . {
    log_trace(">>load_list ::= load_list str_assign");
    utarray_new(LOAD_LIST, &node_icd);
    utarray_push_back(LOAD_LIST, A);
}

load_list(LOAD_LIST) ::= STRING(S) . {
    log_trace(">>load_list ::= STRING");
    utarray_new(LOAD_LIST, &node_icd);
    utarray_push_back(LOAD_LIST, S);
}

load_list(LOAD_LIST) ::= load_list(LL) COMMA(Comma) STRING(S) . {
    log_trace(">>load_list ::= load_list COMMA STRING");
    utarray_push_back(LL, Comma);
    utarray_push_back(LL, S);
    LOAD_LIST = LL;
}

load_list(LOAD_LIST) ::= load_list(LL) COMMA(Comma) str_assign(A) . {
    log_trace(">>load_list ::= load_list COMMA str_assign");
    utarray_push_back(LL, Comma);
    utarray_push_back(LL, A);
    LOAD_LIST = LL;
}

/* optional trailing comma */
load_list(LOAD_LIST) ::= load_list(LL) COMMA(Comma) . {
    log_trace(">>load_list ::= load_list COMMA");
    utarray_push_back(LL, Comma);
    LOAD_LIST = LL;
}

/* cmt_list(COMMENTS) ::= cmt_list(CL) COMMENT(C) . { */
/*     log_trace(">>cmt_list ::= cmt_list COMMENT"); */
/*     utarray_push_back(CL, C); */
/*     COMMENTS = CL; */
/* } */

/* cmt_list(COMMENTS) ::= COMMENT(C) . { */
/*     log_trace(">>cmt_list ::= COMMENT"); */
/*     utarray_new(COMMENTS, &node_icd); */
/*     utarray_push_back(COMMENTS, C); */
/* } */

%endif

%if PARAMS || ALL
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* PARAMETERS and ARGS */

/* Parameters = Parameter {',' Parameter}. */
param_list(Params) ::= param(Param) . {
    log_trace(">>param_list(Params) ::= param(Param)"); }

param_list(Params) ::= param_list(Params_rhs) COMMA param(Param) . {
    log_trace(">>param_list(Params) ::= param_list COMMA param");
    log_trace("  rhs param_list(Params_rhs)");
    log_trace("  rhs param(Param)");
}

/* Parameter = identifier | identifier '=' Test | '*' identifier | '**' identifier . */

param(PARAM) ::= ID(Id) .
{ log_trace(">>param(PARAM) ::= ID(Id)"); }

param(PARAM) ::= param_named(P) . {
    log_trace(">>param(PARAM) ::= param_named(P)"); }
param_named(PARAM) ::= ID(Id) EQ expr(Expr). {
    log_trace(">>param(PARAM) ::= ID(Id) EQ expr(Expr)"); }

param(PARAM) ::= param_star(P) . {
    log_trace(">>param(PARAM) ::= param_star(P)"); }
param_star(PARAM) ::= STAR ID(Id) . {
    log_trace(">>param(PARAM) ::= STAR ID(Id)"); }

param(PARAM) ::= param_starstar(P) . {
    log_trace(">>param(PARAM) ::= param_starstar(P)"); }
param_starstar(PARAM) ::= STARSTAR ID(Id) . {
    log_trace(">>param(PARAM) ::= STARSTAR ID(Id)"); }

%endif

%if STATEMENTS || ALL
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
// STATEMENTS
/* Statement = DefStmt | IfStmt | ForStmt | SimpleStmt . */
// MB: we use 'indent_block' instead of 'suite'
/* DefStmt = 'def' identifier '(' [Parameters [',']] ')' ':' Suite . */
/*IfStmt = 'if' Test ':' Suite {'elif' Test ':' Suite} ['else' ':' Suite].*/
/* ForStmt = 'for' LoopVariables 'in' Expression ':' Suite . */
/* SimpleStmt = SmallStmt {';' SmallStmt} [';'] '\n' . */
/* # NOTE: '\n' optional at EOF */
/* SmallStmt = ReturnStmt */
/*           | BreakStmt | ContinueStmt | PassStmt */
/*           | AssignStmt */
/*           | ExprStmt */
/*           | LoadStmt */
/*           . */

/* AssignStmt   = Expression ('=' | '+=' | '-=' | '*=' | '/=' | '//=' | '%=' | '&=' | '|=' | '^=' | '<<=' | '>>=') Expression . */

statement_list(STMTS) ::= statement(STMT) .
{
    log_trace("\n");
    log_trace(">>statement_list ::= statement");
    log_trace("  lhs statement_list(STMTS)");
    log_trace("  rhs statement(STMT)");
}

statement_list(STMTS_lhs) ::= statement_list(STMTS_rhs) statement(STMT) .
{
    log_trace("\n");
    log_trace(">>statement_list ::= statement_list statement");
    log_trace("  lhs statement_list(STMTS_lhs)");
    log_trace("  rhs statement_list(STMTS_rhs)");
    log_trace("  rhs statement(STMT)");
}

statement(STMT) ::= def_stmt(DefSTMT) .
{
    log_trace("\n");
    log_trace(">>statement ::= def_stmt(DefSTMT)");
    log_trace("  lhs statement(STMT)");
    log_trace("  rhs def_stmt(DefSTMT)");
}

/* statement(STMT) ::= if_stmt(IfSTMT) . */
/* { */
/*     log_trace("\n"); */
/*     log_trace(">>statement ::= if_stmt(IfSTMT)"); */
/*     log_trace("  lhs statement(STMT)"); */
/*     log_trace("  rhs if_stmt(IfSTMT)"); */
/* } */

/* Suite = [newline indent {Statement} outdent] | SimpleStmt . */
indent_block(Block) ::= simple_stmt(SimpleSTMT) . {
    log_trace(">>indent_block(Block) ::= simple_stmt(SimpleSTMT)"); }

/* indent_block(Block) ::= statement_list(STMTS) . { */
/*     log_trace(">>indent_block(Block) ::= statement_list(STMTS)"); } */


/* IfStmt = 'if' Test ':' Suite {'elif' Test ':' Suite} ['else' ':' Suite] . */

/* if_stmt(IfSTMT) ::= IF expr(Expr1) COLON simple_stmt(SimpleSTMT) . /\* FIXME *\/ */
/* { */
/*     log_trace("\n"); */
/*     log_trace(">>if_stmt ::= IF expr COLON ..."); */
/*     log_trace("  lhs if_stmt(IfSTMT)"); */
/*     log_trace("  rhs expr(Expr1)"); */
/*     log_trace("  rhs simple_stmt(SimpleSTMT)"); */
/* } */

statement(STMT) ::= for_stmt(ForSTMT) .
{
    log_trace("\n");
    log_trace(">>statement ::= for_stmt(ForSTMT)");
    log_trace("  lhs statement(STMT)");
    log_trace("  rhs for_stmtm(ForSTMT)");
}
/* ForStmt = 'for' LoopVariables 'in' Expression ':' Suite . */
for_stmt(ForSTMT) ::= FOR loop_vars(LoopVars) IN expr(Expr) COLON indent_block(Suite) .
{
    log_trace("\n");
    log_trace(">>for_stmt ::= FOR loop_vars IN expr COLOR indent_block");
    log_trace("  lhs for_stmt(ForSTMT)");
    log_trace("  rhs loop_vars(LoopVars)");
    log_trace("  rhs expr(Expr)");
    log_trace("  rhs indent_block(Suite)");
}
/* LoopVariables = PrimaryExpr {',' PrimaryExpr} . */
loop_vars(LoopVars) ::= primary_expr(Primx) . {
    log_trace(">>loop_vars(LoopVars) ::= primary_expr(Primx)");
}
loop_vars(LoopVars) ::= loop_vars(LoopVars_rhs) COMMA primary_expr(Primx) . {
    log_trace(">>loop_vars ::= loop_vars COMMA primary_expr");
    log_trace("  lhs loop_vars(LoopVars)");
    log_trace("  rhs loop_vars(LoopVars_rhs)");
    log_trace("  rhs primary_expr(Primx)");
}

statement(STMT) ::= simple_stmt(SimpleSTMT) .
{
    log_trace("\n");
    log_trace(">>statement ::= simple_stmt(SimpleSTMT)");
    log_trace("  lhs statement(STMT)");
    log_trace("  lhs simple_stmt(SimpleSTMT)");
}

/* 'def' defines a function */
/* DefStmt = 'def' identifier '(' [Parameters [',']] ')' ':' Suite . */
def_stmt(STMT) ::= DEF(Def) ID(Id) LPAREN(Lparen) param_list(Params) RPAREN(Rparen) COLON(Colon) indent_block(Block).
{
    log_trace("\n");
    log_trace(">>def_stmt ::= DEF ID LPAREN params RPAREN COLON indent_block");
    log_trace("  lhs: def_stmt(STMT)");
    log_trace("  rhs: DEF(Def)");
    log_trace("  rhs: ID(Id): %s", Id->s);
    log_trace("  rhs: LPAREN(Lparen)");
    log_trace("  rhs: param_list(Params)");
    log_trace("  rhs: RPAREN(Rparen)");
    log_trace("  rhs: COLON(Colon)");
    log_trace("  rhs: indent_block(Block)");

    STMT = calloc(sizeof(struct node_s), 1);
    STMT->type = TK_DEF_STMT;
    STMT->line = Def->line;
    STMT->col  = Def->col;
    utarray_new(STMT->subnodes, &node_icd);

    utarray_push_back(STMT->subnodes, Def);
    utarray_push_back(STMT->subnodes, Id);
    utarray_push_back(STMT->subnodes, Lparen);
    utarray_push_back(STMT->subnodes, Params);
    /* utarray_push_back(STMT->subnodes, Colon); */
    utarray_push_back(STMT->subnodes, Rparen);
    utarray_push_back(STMT->subnodes, Colon);
}

/* LoadStmt = 'load' '(' string {',' [identifier '='] string} [','] ')' . */
load_stmt(STMT) ::= LOAD(Load) LPAREN(Lparen) STRING(S) COMMA(Comma) load_list(LL) RPAREN(Rparen) . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>load_stmt ::= LOAD LPAREN STRING COMMA load_list RPAREN");
    log_trace("  load_stmt (STMT)");
    log_trace("  LOAD (Load)");
    log_trace("  LPAREN (Lparen)");
    log_trace("  COMMA (Comma)");
    log_trace("  STRING (S): %s", S->s);
    log_trace("  load_list (LL)");
    log_trace("  RPAREN (Rparen)");
#endif
    STMT = calloc(sizeof(struct node_s), 1);
    STMT->type = TK_LOAD_STMT;
    STMT->line  = STMT->line;
    STMT->col   = STMT->col;
    utarray_new(STMT->subnodes, &node_icd);

    /* utarray_push_back(STMT->subnodes, LD); */
    utarray_push_back(STMT->subnodes, Load);
    utarray_push_back(STMT->subnodes, Lparen);
    utarray_push_back(STMT->subnodes, S);
    utarray_push_back(STMT->subnodes, Comma);
    utarray_concat(STMT->subnodes, LL);
    utarray_push_back(STMT->subnodes, Rparen);
}

/* SimpleStmt = SmallStmt {';' SmallStmt} [';'] '\n' . */
simple_stmt(SimpSTMT) ::= small_stmt(SmallSTMT) . {
#if DEBUG_TRACE
    log_trace("\n");
    log_trace(">>simple_stmt ::= small_stmt");
    log_trace("  lhs simple_stmt (SimpSTMT)");
    log_trace("  rhs small_stmt(SmallSTMT)");
#endif
    SmallSTMT = calloc(sizeof(struct node_s), 1);
    /* SmallSTMT->type = TK_SMALL_STMT; */ // FIXME
    SmallSTMT->line  = SmallSTMT->line;
    SmallSTMT->col   = SmallSTMT->col;
    /* utarray_new(SmallSTMT->subnodes, &node_icd); */
    /* utarray_concat(SimpSTMT->subnodes, SmallSTMT); */
}

/* # NOTE: '\n' optional at EOF */
/* SmallStmt = ReturnStmt */
/*           | BreakStmt | ContinueStmt | PassStmt */
/*           | AssignStmt */
/*           | ExprStmt */
/*           | LoadStmt */
/*           . */
small_stmt(SmallSTMT) ::= RETURN .
{ log_trace(">>small_stmt(SmallSTMT) ::= RETURN"); }

small_stmt(SmallSTMT) ::= BREAK .
{ log_trace(">>small_stmt(SmallSTMT) ::= BREAK"); }

small_stmt(SmallSTMT) ::= CONTINUE .
{ log_trace(">>small_stmt(SmallSTMT) ::= CONTINUE"); }

small_stmt(SmallSTMT) ::= PASS .
{ log_trace(">>small_stmt(SmallSTMT) ::= PASS"); }

small_stmt(SmallSTMT) ::= load_stmt(STMT) . {
    log_trace(">>small_stmt ::= load_stmt");
    log_trace("  lhs small_stmt(SmallSTMT)");
    log_trace("  rhs load_stmt(STMT)");
}

%endif

%ifdef FOO
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

/* nodelist(NODES) ::= string_list(S) . { */
/*     log_trace(">>nodelist ::= string_list ."); */
/*     log_trace("  rhs string_list(S)"); */
/*     /\* utarray_new(NODES, &node_icd); *\/ */
/*     /\* utarray_push_back(NODES, S); *\/ */
/* } */

nodelist(NODES) ::= nodelist(PREVNODES) string_list(S) . {
    log_trace(">>nodelist ::= nodelist STRING");
    log_trace("  nodelist lhs(NODES)");
    log_trace("  nodelist rhs(PREVNODES)");
    log_trace("  STRING (S)");
    /* utarray_push_back(PREVNODES, S); */
    NODES = PREVNODES;
}

nodelist(NODES) ::= primx(OP) . {
    log_trace(">>nodelist ::= nodelist primx");
    log_trace("  nodelist lhs(NODES)");
    log_trace("  primx (OP)");
    utarray_new(NODES, &node_icd);
    utarray_push_back(NODES, OP);
}

nodelist(NODES) ::= nodelist(PREVNODES) primx(OP) . {
    log_trace(">>nodelist ::= nodelist primx");
    log_trace("  nodelist lhs(NODES)");
    log_trace("  nodelist rhs(PREVNODES)");
    log_trace("  primx (OP)");
    /* utarray_push_back(PREVNODES, OP); */
    NODES = PREVNODES;
}

%endif

%ifdef EXPRESSIONS || ALL
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* EXPRESSIONS */

/* WARNING: We rename these:  'Test' => expr, 'Expression' => expr_list */
/* Test = IfExpr | PrimaryExpr | UnaryExpr | BinaryExpr | LambdaExpr . */
/* Expression = Test {',' Test} . */

/* maybe_expr(Expr) ::= expr(Expr_rhs) . */
/* { log_trace(">>maybe_expr(Expr) ::= expr(Expr_rhs)"); } */
/* maybe_expr(Expr) ::= . */
/* { log_trace(">>maybe_expr(Expr) ::= . "); } */

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* IF EXPRESSIONS */
expr(Expr) ::= if_expr(IfExpr) .
{ log_trace(">>expr(Expr) ::= if_expr(IfExpr"); }

/* IfExpr = Test 'if' Test 'else' Test . */
/* not to be confused with if_stmt! */

if_expr(IfExpr) ::= expr(Expr1) IF expr(Expr2) ELSE expr(Expr3) .
{
 log_trace(">>if_expr ::= expr IF expr ELSE expr");
 log_trace("  lhs if_expr(IfExpr)");
 log_trace("  rhs expr(Expr1)");
 log_trace("  rhs expr(Expr2)");
 log_trace("  rhs expr(Expr3)");
 }

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* PRIMARY EXPRESSIONS */
expr(X) ::= primary_expr(PrimX) .
{
    log_trace(">>expr(X) ::= primary_expr(PrimX");
    X = PrimX;
}

/* WARNING: according to this the following should be legal:
   1.a (or: 1 .a), 1(x), 1[:], etc.
   i.e. suffixes should combine with Operand (int, float, etc.)
 */
/* PrimaryExpr = Primx // => 'Operand' in the original */
/*             | PrimaryExpr DotSuffix */
/*             | PrimaryExpr CallSuffix */
/*             | PrimaryExpr SliceSuffix */

/* %%%% Primx (Operand) */
primary_expr(Primx) ::= primx(Primx_rhs) .
{
    log_trace(">>primary_expr(Primx) ::= primx(Primx_rhs) .");
    Primx = Primx_rhs;
}
/* Terminology: 'operand' => 'primx' (PRimaryEXPRession) */
/* Primx = identifier */
/*         | int | float | string | bytes */
/*         | ListExpr | ListComp */
/*         | DictExpr | DictComp */
/*         | '(' [Expression [',']] ')' */
/*         . */
primx ::= ID(Ident)   . {
    log_trace(">>primx ::= ID(Ident) .");
}
primx(X) ::= INT(Int) . {
    log_trace(">>primx(X) ::= INT(Int) .");
    X = Int;
}
primx ::= FLOAT(Float) . {
    log_trace(">>primx ::= FLOAT(Float) .");
}
primx ::= STRING(S) . {       /* includes rawstrings */
    log_trace(">>primx ::= STRING(S) .");
}
primx ::= BSTRING(B) . {      /* bytes */
    log_trace(">>primx ::= BSTRING(B) .");
}

/* %%%% DotSuffix primary expr */
primary_expr(PrimX) ::= primary_expr(PrimX_rhs) dot_suffix(DotSfx) .
{
    log_trace(">>primary_expr(PrimX) ::= primary_expr(PrimX_rhx) dot_suffix(DotSfx");
    PrimX = calloc(sizeof(struct node_s), 1);
    PrimX->type = TK_Primary_Expr;
    PrimX->line  = PrimX_rhs->line;
    PrimX->col   = PrimX_rhs->col;
    utarray_new(PrimX->subnodes, &node_icd);
    utarray_push_back(PrimX->subnodes, PrimX_rhs);
    utarray_push_back(PrimX->subnodes, DotSfx);
}

dot_suffix(DotSfx) ::= DOT ID(Id) .
{
    log_trace(">>dot_suffix(DotSfx) ::= DOT ID(Id)");
    DotSfx = Id;
    DotSfx->type = TK_Dot_Sfx;
}

/* %%%% CallSuffix primary expr - type TK_Call_Expr */
primary_expr(PrimX) ::= primary_expr(PrimX_rhs) call_suffix(CallSfx) .
{
    log_trace(">>primary_expr(PrimX) ::= primary_expr(PrimX_rhs) call_suffix(CallSfx)");
    PrimX = calloc(sizeof(struct node_s), 1);
    PrimX->type = TK_Call_Expr; // TK_Primary_Expr;
    PrimX->line  = PrimX_rhs->line;
    PrimX->col   = PrimX_rhs->col;
    utarray_new(PrimX->subnodes, &node_icd);
    utarray_push_back(PrimX->subnodes, PrimX_rhs);
    utarray_push_back(PrimX->subnodes, CallSfx);
}

/* CallSuffix  = '(' [Arguments [',']] ')' . */
call_suffix(CallSfx) ::= LPAREN(LParen) arg_list(Args) RPAREN .
{
    log_trace(">>call_suffix(CallSfx) ::= LPAREN arg_list(Args) RPAREN");
    log_debug("  arg_list ct: %d", utarray_len(Args->subnodes));
    CallSfx = calloc(sizeof(struct node_s), 1);
    CallSfx->type = TK_Arg_List;
    CallSfx->line  = LParen->line;
    CallSfx->col   = LParen->col;
    utarray_new(CallSfx->subnodes, &node_icd);
    /* CallSfx->subnodes = Args; */
    /* utarray_push_back(CallSfx->subnodes, Args); */
    utarray_concat(CallSfx->subnodes, Args->subnodes);
    utarray_free(Args->subnodes);
    free(Args);
}

/* Arguments = Argument {',' Argument} . (Optional) */
arg_list(Args) ::= . {
    log_trace(">>arg_list(Args) ::= .");
    Args = calloc(sizeof(struct node_s), 1);
    Args->type = TK_Arg_List;
    Args->subnodes = NULL;      /* Or: empty list? */
}

arg_list(Args) ::= arg(Arg) . {
    log_trace(">>arg_list(Args) ::= arg(Arg)");
    Args = calloc(sizeof(struct node_s), 1);
    Args->type = TK_Arg_List;
    Args->line  = Arg->line;
    Args->col   = Arg->col;
    utarray_new(Args->subnodes, &node_icd);
    utarray_push_back(Args->subnodes, Arg);
}

arg_list(Args) ::= arg_list(Args_rhs) COMMA arg(Arg) . {
    log_trace(">>arg_list(Args) ::= arg_list(Args_rhs COMMA arg(Arg)");
    utarray_push_back(Args_rhs->subnodes, Arg);
    Args = Args_rhs;
}

/* Argument  = Test | identifier '=' Test | '*' Test | '**' Test . */
arg(Arg) ::= expr(X) . {
    log_trace(">>arg(Arg) ::= expr(Expr)");
    Arg = X;
}

arg(Arg) ::= arg_named(Arg_rhs) . {
    log_trace(">>arg(Arg) ::= arg_named(Arg_rhs)");
}
arg_named(Arg) ::= ID(Id) EQ expr(X) . {
    log_trace(">>arg_named(Arg) ::= ID(Id) EQ expr(X)");
    Arg = calloc(sizeof(struct node_s), 1);
    Arg->type = TK_Arg_Named;
    Arg->line  = Id->line;
    Arg->col   = Id->col;
    utarray_new(Arg->subnodes, &node_icd);
    utarray_push_back(Arg->subnodes, Id);
    utarray_push_back(Arg->subnodes, X);
}

/* arg(Arg) ::= arg_star(Arg_rhs) . { */
/*     log_trace(">>arg(Arg) ::= arg_star(Arg_rhs)"); } */
/* arg_star(Arg) ::= STAR expr(Expr) . { */
/*     log_trace(">>arg_star(Arg) ::= STAR expr(Expr)"); } */

/* arg(Arg) ::= arg_starstar(Arg_rhs) . { */
/*     log_trace(">>arg(Arg) ::= arg_starstar(Arg_rhs)"); } */
/* arg_starstar(Arg) ::= STARSTAR expr(Expr) . { */
/*     log_trace(">>arg_starstar(Arg) ::= STARSTAR expr(Expr)"); } */

/* primary_expr(PrimX) ::= primary_expr(PrimX_rhs) slice_suffix(SliceSfx) . */
/* { */
/*     log_trace(">>primary_expr(PrimX) ::= primary_expr slice_suffix"); */
/*     log_trace("  lhs primary_expr(PrimX)"); */
/*     log_trace("  rhs primary_expr(PrimX_rhs)"); */
/*     log_trace("  rhs slice_suffix(SliceSfx)"); */
/* } */
/* SliceSuffix = '[' [Expression] ':' [Test] [':' [Test]] ']' */
/*             | '[' Expression ']' */
/*             . */
 /* foo[bar], foo[:], foo[a:], foo[a:b], foo[a:b:c] */
/* with Expression (=Test list):  foo[a,b,c:d] */
/* expr_list may be empty; ditto for expr? */
/* for optional expr we need a 'maybe_expr'? */
/* slice_suffix(SliceSfx) ::= LBRACK expr_list COLON */
/*                            maybe_expr COLON maybe_expr RBRACK . */
/* { */
/*     log_trace(">>slice_sfx(SliceSfx) ::= LBrack expr_list COLON maybe_expr COLON maybe_expr RBRACK"); */
/* } */
/* slice_suffix(SliceSfx) ::= LBRACK expr_list COLON expr RBRACK . */
/* { */
/* } */
/* slice_suffix(SliceSfx) ::= LBRACK expr_list COLON COLON expr RBRACK . */
/* { */
/* } */
/* slice_suffix(SliceSfx) ::= LBRACK expr_list COLON COLON RBRACK . */
/* { */
/* } */


/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* UNARY EXPRESSIONS */
expr(X) ::= unary_expr(X_rhs) .
{
    log_trace(">>expr(X) ::= unary_expr(X_rhs)");
    X = X_rhs;
}
/* UnaryExpr = '+' Test */
/*           | '-' Test */
/*           | '~' Test */
/*           | 'not' Test */
/*           . */
unary_expr(X_lhs) ::= PLUS(Op) expr(X_rhs) . {
    log_trace(">>unary_expr(X_lhs) ::= PLUS expr(X_rhs)");
    X_lhs = calloc(sizeof(struct node_s), 1);
    X_lhs->type = TK_Unary_Expr;
    X_lhs->line  = X_rhs->line;
    X_lhs->col   = X_rhs->col;
    utarray_new(X_lhs->subnodes, &node_icd);
    utarray_push_back(X_lhs->subnodes, Op);
    utarray_push_back(X_lhs->subnodes, X_rhs);
}
unary_expr(X_lhs) ::= MINUS(Op) expr(X_rhs) . {
    log_trace(">>unary_expr(X_lhs) ::= MINUS expr(X_rhs)");
    X_lhs = calloc(sizeof(struct node_s), 1);
    X_lhs->type = TK_Unary_Expr;
    X_lhs->line  = X_rhs->line;
    X_lhs->col   = X_rhs->col;
    utarray_new(X_lhs->subnodes, &node_icd);
    utarray_push_back(X_lhs->subnodes, Op);
    utarray_push_back(X_lhs->subnodes, X_rhs);
}
unary_expr(X_lhs) ::= TILDE(Op) expr(X_rhs) . {
    log_trace(">>unary_expr(X_lhs) ::= TILDE expr(X_rhs)");
    X_lhs = calloc(sizeof(struct node_s), 1);
    X_lhs->type = TK_Unary_Expr;
    X_lhs->line  = X_rhs->line;
    X_lhs->col   = X_rhs->col;
    utarray_new(X_lhs->subnodes, &node_icd);
    utarray_push_back(X_lhs->subnodes, Op);
    utarray_push_back(X_lhs->subnodes, X_rhs);
}
unary_expr(X_lhs) ::= NOT(Op) expr(X_rhs) . {
    log_trace(">>unary_expr(X_lhs) ::= NOT expr(X_rhs)");
    X_lhs = calloc(sizeof(struct node_s), 1);
    X_lhs->type = TK_Unary_Expr;
    X_lhs->line  = X_rhs->line;
    X_lhs->col   = X_rhs->col;
    utarray_new(X_lhs->subnodes, &node_icd);
    utarray_push_back(X_lhs->subnodes, Op);
    utarray_push_back(X_lhs->subnodes, X_rhs);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* BINARY EXPRESSIONS */
expr(X) ::= binary_expr(X_rhs) .
{
    log_trace(">>expr(X) ::= binary_expr(X_rhs)");
    log_trace("  rhs expr subnode ct: %d", utarray_len(X_rhs->subnodes));
    X = X_rhs;
}

/* BinaryExpr = Test {Binop Test} . */
/* WARNING: {} means zero or more, not possible here. */
/* [PLUS] sets precedence of this rule eq to that of PLUS. This makes
.g. x + y + z parse as (x + y) + z */
binary_expr(BinExpr) ::= expr(BinExpr_rhs) binop(BinOp) expr(X_rhs) . [PLUS]
{
    log_trace(">>binary_expr ::= binary_expr binop expr");
    log_trace("  lhs binary_expr(BinExpr)");
    log_trace("  rhs binary_expr(BinExpr_rhs)");
    if (BinExpr_rhs->subnodes)
        log_trace("  rhs binary_expr subnode ct: %d",
                  utarray_len(BinExpr_rhs->subnodes));
    log_trace("  rhs binop(BinOp)");
    log_trace("  rhs expr(X_rhs)");
    /* log_trace("  rhs expr subnode ct: %d", */
    /*           utarray_len(X_rhs->subnodes)); */

    BinExpr = calloc(sizeof(struct node_s), 1);
    BinExpr->type = TK_Bin_Expr;
    BinExpr->s = NULL;
    BinExpr->line  = BinExpr_rhs->line;
    BinExpr->col   = BinExpr_rhs->col;
    utarray_new(BinExpr->subnodes, &node_icd);
    utarray_push_back(BinExpr->subnodes, BinExpr_rhs);
    utarray_push_back(BinExpr->subnodes, BinOp);
    utarray_push_back(BinExpr->subnodes, X_rhs);
    log_debug("DUMPING BINARY_EXPR");
    dump_node(BinExpr);
    log_debug("/DUMPING BINARY_EXPR");
}

/* Binop = 'or' */
/*       | 'and' */
/*       | '==' | '!=' | '<' | '>' | '<=' | '>=' | 'in' | 'not' 'in' */
/*       | '|' */
/*       | '^' */
/*       | '&' */
/*       | '<<' | '>>' */
/*       | '-' | '+' */
/*       | '*' | '%' | '/' | '//' */

binop ::= OR . { log_trace(">>binop ::= OR"); }
binop ::= AND . { log_trace(">>binop ::= AND"); }
binop ::= EQ2 . { log_trace(">>binop ::= EQ2"); }
binop ::= BANG_EQ . { log_trace(">>binop ::= BANG_EQ"); }
binop ::= LT . { log_trace(">>binop ::= LT"); }
binop ::= LE . { log_trace(">>binop ::= LE"); }
binop ::= GT . { log_trace(">>binop ::= GT"); }
binop ::= GE . { log_trace(">>binop ::= GE"); }
binop ::= IN . { log_trace(">>binop ::= IN"); }
binop ::= NOT IN . [IN] {
    log_trace(">>binop ::= NOT IN"); }
binop ::= VBAR . { log_trace(">>binop ::= VBAR"); }
binop ::= CARET . { log_trace(">>binop ::= CARET"); }
binop ::= AMP . { log_trace(">>binop ::= AMP"); }
binop ::= LLANGLE . { log_trace(">>binop ::= LLANGLE"); }
binop ::= RRANGLE . { log_trace(">>binop ::= RRANGLE"); }
binop ::= MINUS . { log_trace(">>binop ::= MINUS"); }
binop ::= PLUS . { log_trace(">>binop ::= PLUS"); }
binop ::= STAR . { log_trace(">>binop ::= STAR"); }
binop ::= PCT . { log_trace(">>binop ::= PCT"); }
binop ::= SLASH . { log_trace(">>binop ::= SLASH"); }
binop ::= SLASH2 . { log_trace(">>binop ::= SLASH2"); }

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* LAMBDA EXPRESSIONS */
expr(Expr) ::= lambda_expr(LambdaExpr) .
{ log_trace(">>expr(Expr) ::= lambda_expr(LambdaExpr"); }

/* expr_list(ExprList) ::= expr(Expr) . */
/* { log_trace(">>expr_list(ExprList) ::= expr(Expr"); } */

/* expr_list(ExprList) ::= expr_list(ExprList_rhs) expr(Expr) . */
/* { log_trace(">>expr_list(ExprList) ::= expr_list(ExprList_rhs) expr(Expr"); } */


/*         | ListExpr | ListComp */
/*         | DictExpr | DictComp */
/* ListExpr = '[' [Expression [',']] ']' . */
/* ListComp = '[' Test {CompClause} ']'. */

/* DictExpr = '{' [Entries [',']] '}' . */
/* DictComp = '{' Entry {CompClause} '}' . */
/* Entries  = Entry {',' Entry} . */
/* Entry    = Test ':' Test . */

/* CompClause = 'for' LoopVariables 'in' Test | 'if' Test . */


/* LambdaExpr = 'lambda' [Parameters] ':' Test . */
lambda_expr(LExpr) ::= LAMBDA param_list(Params) COLON . // expr(Expr) .
{
    log_trace(">>lambda_expr ::= LAMBDA params COLOR expr");
    log_trace("  lhs lambda_expr(LExpr)");
    log_trace("  rhs params(Params)");
    log_trace("  rhs expr(Expr)");
}

%endif // EXPRESSIONS

