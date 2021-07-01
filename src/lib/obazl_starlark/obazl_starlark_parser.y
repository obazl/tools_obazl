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

%extra_argument { struct node_s **root}
/* %extra_argument { struct obazl_buildfile_s *ast} */

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
%token STAR2 .
%token STAR2_ARGS .
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
%token Arg_Star .
%token Arg_Star2 .
%token Bin_Expr .
%token Call_Expr .
%token Call_Sfx .
%token Comp_Clause .
%token Dict_Comp .
%token Dict_Entry .
%token Dict_Entry_List .
%token Dict_Expr .
%token Dot_Expr .
%token Dot_Sfx .
%token If_Expr .
%token Lambda_Expr .
%token List_Comp .
%token List_Expr .
%token Loop_Vars .
%token Param_List .
%token Param_Named .
%token Param_Star .
%token Param_Star2 .
%token Paren_Expr .
%token Slice_Expr .
%token Slice_Sfx .
%token Primary_Expr .
%token Unary_Expr .
%token SYM .

%token_prefix TK_

/* Python op precedence: */
/* https://docs.python.org/3/reference/expressions.html#operator-precedence */
 /* https://www.mathcs.emory.edu/~valerie/courses/fall10/155/resources/op_precedence.html */
%right LAMBDA .
%nonassoc COMMA .
%nonassoc OR .
%nonassoc AND .
%right NOT .
/* membership: in, not in */
/* identity: is, is not */
/* comparison: <, <=,  >,  >=, !=, == ('<>' not supported)*/
/* NB: 'not in', 'is not' are non-terminals */
%nonassoc IN IS LANGLE LE RANGLE GE BANG_EQ EQ2 .
%nonassoc VBAR .                /* bitwise OR binop */
%nonassoc CARET .               /* bitwise XOR binop */
%nonassoc AMP .                 /* bitwise AND binop */
%nonassoc LLANGLE RRANGLE .     /* bitwise shift binops */
%right LBRACK LBRACE LPAREN .   /* prevent ambiguities */
%left PLUS MINUS .
%nonassoc STAR SLASH SLASH2 PCT .
/* %right PLUS .                   /\* positive, e.g. +3 *\/ */
/* %right MINUS .                  /\* negative, e.g. -3 *\/ */
%right TILDE .                  /* bitwise NOT (unary) */
%right STAR2 .                  /* exponentiation */
%nonassoc IF ELSE . /* may go both ways: foo IF bar ELSE baz v. IF foo ELSE bar */
%right FOR .
/* %right ELSE . */
/* binops: all nonassoc, mult ops higher precedence than add ops */
/* unary ops: +, -, not, ~ */

/* **************** */
%token_type { struct node_s* }  /* terminals */
%default_type { struct node_s* } /* non-terminals */
%default_destructor {
    log_trace("freeing non-terminal, type %d", $$->type);
    free($$);
}

/* %type nodelist { UT_array* } */
/* %destructor nodelist { */
/*     log_trace("freeing nodelist"); */
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
/* %type def_stmt { struct node_s* } */
/* %destructor def_stmt { */
/*     log_trace("freeing def_stmt"); */
/*     /\* utarray_free($$->list); *\/ */
/* } */

/* %type load_stmt { struct node_s* } */
/* %destructor load_stmt { */
/*     log_trace("freeing load_stmt"); */
/*     /\* utarray_free($$->list); *\/ */
/* } */

/* %type primx { struct node_s* } */
/* %destructor primx { */
/*     log_trace("freeing primx"); */
/* } */

/* %type params { struct node_s* } */
/* %destructor params { */
/*     log_trace("freeing params"); */
/* } */

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
/* %type indent_block { struct node_s* } */
/* %destructor indent_block { */
/*     log_trace("freeing indent_block"); */
/*     /\* utarray_free($$->list); *\/ */
/* } */

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
    *root = SS;
}

%ifdef TEST
buildfile(File) ::= expr(X) . {
    log_trace(">>buildfile(File) ::= expr(X) .");
    *root = X;
}

/* buildfile(File) ::= primary_expr(X) . { */
/*     log_trace(">>buildfile(File) ::= primary_expr(X) ."); */
/*     *root = X; */
/* } */

/* buildfile(File) ::= list_expr(X) . [NOT] { */
/*     log_trace(">>buildfile(File) ::= list_expr(X) ."); */
/*     log_debug("START dump"); */
/*     dump_node(X); */
/*     log_debug("/START dump"); */
/*     *root = X; */
/* } */

/* buildfile(File) ::= unary_expr(X) . { */
/*     log_trace(">>buildfile(File) ::= unary_expr(X) ."); */
/*     log_debug("START dump"); */
/*     dump_node(X); */
/*     log_debug("/START dump"); */
/*     *root = X; */
/* } */

/* buildfile(File) ::= binary_expr(X) . { */
/*     log_trace(">>buildfile(File) ::= binary_expr(X) ."); */
/*     /\* log_debug("START dump"); *\/ */
/*     /\* dump_node(X); *\/ */
/*     /\* log_debug("/START dump"); *\/ */
/*     *root = X; */
/* } */
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

/* %if NODES */
/* nodes_test ::= nodelist(NODES) . { */
/* #if DEBUG_TRACE */
/*     log_trace("\n"); */
/*     log_trace(">>nodes_test ::= nodelist ."); */
/*     log_trace("  rhs nodelist (NODES)"); */
/* #endif */
/*     dump_nodes(NODES); */
/* } */
/* %endif */

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
/* PARAMETERS */

/* Parameters = Parameter {',' Parameter}. */
param_list(Params) ::= param(Param) . {
    log_trace(">>param_list(Params) ::= param(Param)");
    Params = calloc(sizeof(struct node_s), 1);
    Params->type = TK_Param_List;
    Params->line  = Param->line;
    Params->col   = Param->col;
    utarray_new(Params->subnodes, &node_icd);
    utarray_push_back(Params->subnodes, Param);
}

param_list(Params) ::= param_list(Params_rhs) COMMA(Comma) param(Param) . {
    log_trace(">>param_list(Params) ::= param_list COMMA param");
    utarray_push_back(Params_rhs->subnodes, Comma);
    utarray_push_back(Params_rhs->subnodes, Param);
    Params = Params_rhs;
}

/* Parameter = identifier | identifier '=' Test | '*' identifier | '**' identifier . */

param(Param) ::= ID(Id) . {
    log_trace(">>param(PARAM) ::= ID(Id)");
    Param = Id;
}

param(Param) ::= param_named(P) . {
    log_trace(">>param(Param) ::= param_named(P)");
    Param = P;
}
param_named(Param) ::= ID(Id) EQ(Eq) expr(X). {
    log_trace(">>param(Param) ::= ID(Id) EQ(Eq) expr(X)");
    Param = calloc(sizeof(struct node_s), 1);
    Param->type = TK_Param_Named;
    Param->line = Id->line;
    Param->col  = Id->col;
    utarray_new(Param->subnodes, &node_icd);
    utarray_push_back(Param->subnodes, Id);
    utarray_push_back(Param->subnodes, Eq);
    utarray_push_back(Param->subnodes, X);
}

param(Param) ::= param_star(P) . {
    log_trace(">>param(Param) ::= param_star(P)");
    Param = P;
}
param_star(Param) ::= STAR(Star) ID(Id) . {
    log_trace(">>param(Param) ::= STAR ID(Id)");
    Param = calloc(sizeof(struct node_s), 1);
    Param->type = TK_Param_Star;
    Param->line = Star->line;
    Param->col  = Star->col;
    utarray_new(Param->subnodes, &node_icd);
    utarray_push_back(Param->subnodes, Star);
    utarray_push_back(Param->subnodes, Id);
}

param(Param) ::= param_star2(P) . {
    log_trace(">>param(Param) ::= param_star2(P)");
    Param = P;
}
param_star2(Param) ::= STAR2(Star2) ID(Id) . {
    log_trace(">>param(Param) ::= STAR2 ID(Id)");
    Param = calloc(sizeof(struct node_s), 1);
    Param->type = TK_Param_Star2;
    Param->line = Star2->line;
    Param->col  = Star2->col;
    utarray_new(Param->subnodes, &node_icd);
    utarray_push_back(Param->subnodes, Star2);
    utarray_push_back(Param->subnodes, Id);
}

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
    LoopVars = Primx;
}

loop_vars(LoopVars) ::= loop_vars(LoopVars_rhs) COMMA(Comma) primary_expr(X) . {
    log_trace(">>loop_vars ::= loop_vars COMMA primary_expr");
    log_trace("  lhs loop_vars(LoopVars)");
    log_trace("  rhs loop_vars(LoopVars_rhs)");
    log_trace("  rhs primary_expr(Primx)");
    LoopVars = calloc(sizeof(struct node_s), 1);
    LoopVars->type = TK_Loop_Vars;
    LoopVars->line  = LoopVars_rhs->line;
    LoopVars->col   = LoopVars_rhs->col;
    utarray_new(LoopVars->subnodes, &node_icd);
    utarray_push_back(LoopVars->subnodes, LoopVars_rhs);
    utarray_push_back(LoopVars->subnodes, Comma);
    utarray_push_back(LoopVars->subnodes, X);
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
/* nodelist(NODES) ::= nodelist(PREVNODES) load_stmt(LOAD) . { */
/*     log_trace(">>nodelist ::= nodelist load_stmt"); */
/*     log_trace("  nodelist lhs(NODES)"); */
/*     log_trace("  nodelist rhs(PREVNODES)"); */
/*     log_trace("  load_stmt (LOAD)"); */
/*     utarray_push_back(PREVNODES, LOAD); */
/*     NODES = PREVNODES; */
/* } */

/* nodelist(NODES) ::= load_stmt(LOAD) . { */
/*     log_trace(">>nodelist ::= load_stmt ."); */
/*     log_trace("  nodelist lhs load_stmt(LOAD)"); */
/*     utarray_new(NODES, &node_icd); */
/*     utarray_push_back(NODES, LOAD); */
/* } */

/* nodelist(NODES) ::= nodelist(PREVNODES) def_stmt(LOAD) . { */
/*     log_trace(">>nodelist ::= nodelist def_stmt"); */
/*     log_trace("  nodelist lhs(NODES)"); */
/*     log_trace("  nodelist rhs(PREVNODES)"); */
/*     log_trace("  def_stmt (LOAD)"); */
/*     utarray_push_back(PREVNODES, LOAD); */
/*     NODES = PREVNODES; */
/* } */

/* nodelist(NODES) ::= def_stmt(DEF) . { */
/*     log_trace(">>nodelist ::= def_stmt ."); */
/*     log_trace("  nodelist lhs def_stmt(DEF)"); */
/*     utarray_new(NODES, &node_icd); */
/*     utarray_push_back(NODES, DEF); */
/* } */

/* nodelist(NODES) ::= COMMENT(C) . { */
/*     log_trace(">>nodelist ::= COMMENT ."); */
/*     utarray_new(NODES, &node_icd); */
/*     utarray_push_back(NODES, C); */
/* } */

/* nodelist(NODES) ::= nodelist(PREVNODES) COMMENT(C) . { */
/*     log_trace(">>nodelist ::= nodelist COMMENT"); */
/*     log_trace("  nodelist lhs(NODES)"); */
/*     log_trace("  nodelist rhs(PREVNODES)"); */
/*     log_trace("  COMMENT (C)"); */
/*     utarray_push_back(PREVNODES, C); */
/*     NODES = PREVNODES; */
/* } */

/* /\* nodelist(NODES) ::= string_list(S) . { *\/ */
/* /\*     log_trace(">>nodelist ::= string_list ."); *\/ */
/* /\*     log_trace("  rhs string_list(S)"); *\/ */
/* /\*     /\\* utarray_new(NODES, &node_icd); *\\/ *\/ */
/* /\*     /\\* utarray_push_back(NODES, S); *\\/ *\/ */
/* /\* } *\/ */

/* nodelist(NODES) ::= nodelist(PREVNODES) string_list(S) . { */
/*     log_trace(">>nodelist ::= nodelist STRING"); */
/*     log_trace("  nodelist lhs(NODES)"); */
/*     log_trace("  nodelist rhs(PREVNODES)"); */
/*     log_trace("  STRING (S)"); */
/*     /\* utarray_push_back(PREVNODES, S); *\/ */
/*     NODES = PREVNODES; */
/* } */

/* nodelist(NODES) ::= primx(OP) . { */
/*     log_trace(">>nodelist ::= nodelist primx"); */
/*     log_trace("  nodelist lhs(NODES)"); */
/*     log_trace("  primx (OP)"); */
/*     utarray_new(NODES, &node_icd); */
/*     utarray_push_back(NODES, OP); */
/* } */

/* nodelist(NODES) ::= nodelist(PREVNODES) primx(OP) . { */
/*     log_trace(">>nodelist ::= nodelist primx"); */
/*     log_trace("  nodelist lhs(NODES)"); */
/*     log_trace("  nodelist rhs(PREVNODES)"); */
/*     log_trace("  primx (OP)"); */
/*     /\* utarray_push_back(PREVNODES, OP); *\/ */
/*     NODES = PREVNODES; */
/* } */

%endif

%ifdef EXPRESSIONS || ALL
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* EXPRESSIONS */

/* WARNING: We rename these:  'Test' => expr, 'Expression' => expr_list */
/* Test = IfExpr | PrimaryExpr | UnaryExpr | BinaryExpr | LambdaExpr . */
/* Expression = Test {',' Test} . */
expr(X) ::= if_expr(IfX) . [LAMBDA]
{
    log_trace(">>expr(Expr) ::= if_expr(IfExpr");
    X = IfX;
}
expr(X) ::= primary_expr(PrimX) . [IF]
{
    log_trace(">>expr(X) ::= primary_expr(PrimX");
    /* log_trace("s: %s", X->s); */
    X = PrimX;
}
expr(X) ::= unary_expr(X_rhs) .
{
    log_trace(">>expr(X) ::= unary_expr(X_rhs)");
    X = X_rhs;
}
expr(X) ::= binary_expr(X_rhs) .
{
    log_trace(">>expr(X) ::= binary_expr(X_rhs)");
    log_trace("  rhs expr subnode ct: %d", utarray_len(X_rhs->subnodes));
    X = X_rhs;
}
expr(X) ::= lambda_expr(X_rhs) .
{
    log_trace(">>expr(X) ::= lambda_expr(X_rhs)");
    X = X_rhs;
}


/* maybe_expr(Expr) ::= expr(Expr_rhs) . */
/* { log_trace(">>maybe_expr(Expr) ::= expr(Expr_rhs)"); } */
/* maybe_expr(Expr) ::= . */

/* { log_trace(">>maybe_expr(Expr) ::= . "); } */

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* IF EXPRESSIONS */
/* IfExpr = Test 'if' Test 'else' Test . */
/* not to be confused with if_stmt! */

if_expr(IfX) ::= expr(X1) IF(If) expr(X2) ELSE(Else) expr(X3) . [LAMBDA]
{
    log_trace(">>if_expr(IfX) ::= expr IF expr ELSE expr");
    IfX = calloc(sizeof(struct node_s), 1);
    IfX->type = TK_If_Expr;
    IfX->line  = X1->line;
    IfX->col   = X1->col;
    utarray_new(IfX->subnodes, &node_icd);
    utarray_push_back(IfX->subnodes, X1);
    utarray_push_back(IfX->subnodes, If);
    utarray_push_back(IfX->subnodes, X2);
    utarray_push_back(IfX->subnodes, Else);
    utarray_push_back(IfX->subnodes, X3);
 }

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* PRIMARY EXPRESSIONS */
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
primx(PrimX) ::= ID(Id)   . {
    log_trace(">>primx ::= ID(Id) :]%s[:", Id->s);
    PrimX = Id;
}
primx(PrimX) ::= INT(Int) . {
    log_trace(">>primx(X) ::= INT(Int) .");
    PrimX = Int;
}
primx(PrimX) ::= FLOAT(Float) . {
    log_trace(">>primx ::= FLOAT(Float) .");
    PrimX = Float;
}
primx(PrimX) ::= STRING(S) . {       /* includes rawstrings */
    log_trace(">>primx ::= STRING(S) .");
    PrimX = S;
}
primx(PrimX) ::= BSTRING(B) . {      /* bytes */
    log_trace(">>primx ::= BSTRING(B) .");
    PrimX = B;
}
primx(PrimX) ::= list_expr(ListX) . [COMMA] {
    log_trace(">>primx ::= list_expr(ListX) .");
    PrimX = ListX;
}
primx(PrimX) ::= list_comp(LComp) . [COMMA] {
    log_trace(">>primx ::= list_comp(LComp) .");
    PrimX = LComp;
}
primx(PrimX) ::= dict_expr(DictX) . [COMMA] {
    log_trace(">>primx ::= dict_expr(DictX) .");
    PrimX = DictX;
}
primx(PrimX) ::= dict_comp(DictX) . [COMMA] {
    log_trace(">>primx ::= dict_comp(DictX) .");
    PrimX = DictX;
}
primx(PrimX) ::= paren_expr(ParenX) . [COMMA] {
    log_trace(">>primx ::= paren_comp .");
    PrimX = ParenX;
}

/* %%%% DotSuffix primary expr - type TK_Dot_Expr */
primary_expr(PrimX) ::= primary_expr(PrimX_rhs) dot_suffix(DotSfx) .
{
    log_trace(">>primary_expr(PrimX) ::= primary_expr(PrimX_rhx) dot_suffix(DotSfx");
    PrimX = calloc(sizeof(struct node_s), 1);
    PrimX->type = TK_Dot_Expr; // TK_Primary_Expr;
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
call_suffix(CallSfx) ::= LPAREN(LParen) arg_list(Args) RPAREN(RParen) .
{
    log_trace(">>call_suffix(CallSfx) ::= LPAREN arg_list(Args) RPAREN");
    if (Args->subnodes)
        log_debug("  arg_list ct: %d", utarray_len(Args->subnodes));
    CallSfx = calloc(sizeof(struct node_s), 1);
    CallSfx->type = TK_Arg_List;
    CallSfx->line  = LParen->line;
    CallSfx->col   = LParen->col;
    utarray_new(CallSfx->subnodes, &node_icd);
    /* Need to retain parens, for comments and whitespace */
    utarray_push_back(CallSfx->subnodes, LParen);
    utarray_push_back(CallSfx->subnodes, Args);
    /* if (Args->subnodes) { */
    /*     log_debug("XXXXXXXXXXXXXXXX"); */
    /*     utarray_concat(CallSfx->subnodes, Args->subnodes); */
    /* } */
    utarray_push_back(CallSfx->subnodes, RParen);
    /* if (Args->subnodes) */
    /*     utarray_free(Args->subnodes); */
    /* free(Args); */
}

call_suffix(CallSfx) ::= LPAREN(LParen) arg_list_comma(Args) RPAREN(RParen) .
{
    log_trace(">>call_suffix(CallSfx) ::= LPAREN arg_list(Args) RPAREN");
    if (Args->subnodes)
        log_debug("  arg_list ct: %d", utarray_len(Args->subnodes));
    CallSfx = calloc(sizeof(struct node_s), 1);
    CallSfx->type = TK_Arg_List;
    CallSfx->line  = LParen->line;
    CallSfx->col   = LParen->col;
    utarray_new(CallSfx->subnodes, &node_icd);
    /* Need to retain parens, for comments and whitespace */
    utarray_push_back(CallSfx->subnodes, LParen);
    utarray_push_back(CallSfx->subnodes, Args);
    /* if (Args->subnodes) { */
    /*     log_debug("XXXXXXXXXXXXXXXX"); */
    /*     utarray_concat(CallSfx->subnodes, Args->subnodes); */
    /* } */
    utarray_push_back(CallSfx->subnodes, RParen);
    /* if (Args->subnodes) */
    /*     utarray_free(Args->subnodes); */
    /* free(Args); */
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

/* optional trailing comma */
arg_list_comma(Args) ::= arg_list(Args_rhs) COMMA(Comma) . {
    log_trace(">>arg_list(Args) ::= arg_list(Args_rhs COMMA arg(Arg)");
    /* retain the comma, for comments and ws */
    utarray_push_back(Args_rhs->subnodes, Comma);
    Args = Args_rhs;
}

arg_list(Args) ::= arg_list(Args_rhs) COMMA(Comma) arg(Arg) . {
    log_trace(">>arg_list(Args) ::= arg_list(Args_rhs COMMA arg(Arg)");
    /* retain the comma, for comments and ws */
    utarray_push_back(Args_rhs->subnodes, Comma);
    utarray_push_back(Args_rhs->subnodes, Arg);
    Args = Args_rhs;
}

/* WARNING: we do not currently validate ordering of arg types */
/* Argument  = Test | identifier '=' Test | '*' Test | '**' Test . */
arg(Arg) ::= expr(X) . {
    log_trace(">>arg(Arg) ::= expr(Expr)");
    /* log_trace("s: %s", X->s); */
    Arg = X;
}

/* arg(Arg) ::= arg_named(Arg_rhs) . { */
/*     log_trace(">>arg(Arg) ::= arg_named(Arg_rhs)"); */
/* } */
arg(Arg) ::= ID(Id) EQ(Eq) expr(X) . {
    log_trace(">>arg(Arg) ::= ID(Id) EQ expr(X)");
    Arg = calloc(sizeof(struct node_s), 1);
    Arg->type = TK_Arg_Named;
    Arg->line  = Id->line;
    Arg->col   = Id->col;
    utarray_new(Arg->subnodes, &node_icd);
    utarray_push_back(Arg->subnodes, Id);
    utarray_push_back(Arg->subnodes, Eq);
    utarray_push_back(Arg->subnodes, X);
}

/* Argument  = ... | '*' Test | ... */
arg(Arg) ::= STAR(Star) expr(X) . {
    log_trace(">>arg(Arg) ::= STAR expr(X)");
    Arg = calloc(sizeof(struct node_s), 1);
    Arg->type = TK_Arg_Star;
    Arg->line  = Star->line;
    Arg->col   = Star->col;
    utarray_new(Arg->subnodes, &node_icd);
    utarray_push_back(Arg->subnodes, Star);
    utarray_push_back(Arg->subnodes, X);
}

/* Argument  = ... | '**' Test . */
arg(Arg) ::= STAR2(Star2) expr(X) . {
    log_trace(">>arg(Arg) ::= STAR expr(X)");
    Arg = calloc(sizeof(struct node_s), 1);
    Arg->type = TK_Arg_Star2;
    Arg->line  = Star2->line;
    Arg->col   = Star2->col;
    utarray_new(Arg->subnodes, &node_icd);
    utarray_push_back(Arg->subnodes, Star2);
    utarray_push_back(Arg->subnodes, X);
}


/* %%%% SliceSuffix primary expr - type TK_Slice_Expr */
/* Expression = Test {',' Test} => expr {',' expr} */
/* i.e. Expression = expr_list */
/* foo[bar], foo[:], foo[a:], foo[a:b], foo[a:b:c] */
/* with Expression (=Test list):  foo[a,b,c:d] */
/* expr_list may be empty; ditto for expr? */

primary_expr(PrimX) ::= primary_expr(PrimX_rhs) slice_suffix(SliceSfx) .
{
    log_trace(">>primary_expr(PrimX) ::= primary_expr slice_suffix");
    log_trace("  lhs primary_expr(PrimX)");
    log_trace("  rhs primary_expr(PrimX_rhs)");
    log_trace("  rhs slice_suffix(SliceSfx)");
    PrimX = calloc(sizeof(struct node_s), 1);
    PrimX->type = TK_Slice_Expr;
    PrimX->line  = PrimX_rhs->line;
    PrimX->col   = PrimX_rhs->col;
    utarray_new(PrimX->subnodes, &node_icd);
    utarray_push_back(PrimX->subnodes, PrimX_rhs);
    utarray_push_back(PrimX->subnodes, SliceSfx);
}

/* SliceSuffix = '[' [Expression] ':' [Test] [':' [Test]] ']' */
/*             | '[' Expression ']' */
/* What does e.g. foo[a,b:] mean? */
/* two colons: extended slicing. see https://docs.python.org/release/2.3.5/whatsnew/section-slices.html */
/* last arg is stride/step, e.g. foo[start:end:stride] */
/* we'll do this the hard way, instead of trying to use empty exprs */
/* combinations:
 foo[], foo[:], foo[::],
 foo[a], foo[a:], foo[a::], foo[a:b], foo[a:b:], foo[a::c], foo[:a:b:c]
 foo[:b], foo[:b:], foo[:b:c]
 foo[::c]
 */

/* foo[] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBRACK RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[:] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) COLON(Colon) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBrack COLON RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Colon);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[::] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) COLON(Colon1) COLON(Colon2) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBrack COLON COLON RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}

 /* foo[a], foo[a:], foo[a::], foo[a:b], foo[a:b:], foo[a::c], foo[a:b:c] */
/* foo[a] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBRACK expr_list RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[a:] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) COLON(Colon) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBrack expr_list COLON RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, Colon);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[a::] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) COLON(Colon1) COLON(Colon2) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBrack expr_list COLON COLON RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[a:b] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) COLON(Colon) expr(X) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBrack expr_list COLON expr RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, Colon);
    utarray_push_back(SliceSfx->subnodes, X);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[a:b:] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) COLON(Colon1) expr(X) COLON(Colon2) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBrack expr_list COLON expr COLON RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, X);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[a::c] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) COLON(Colon1) COLON(Colon2) expr(X) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBrack expr_list COLON COLON expr RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, X);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[a:b:c] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) expr_list(Xs) COLON(Colon1) expr(X1) COLON(Colon2) expr(X2) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBrack expr_list COLON expr COLON expr RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Xs);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, X1);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, X2);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
 /* foo[:b], foo[:b:], foo[:b:c] */
 /* foo[::c] */

/* foo[:b] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) COLON(Colon) expr(X) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBrack COLON expr RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Colon);
    utarray_push_back(SliceSfx->subnodes, X);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[:b:] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) COLON(Colon1) expr(X) COLON(Colon2) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBrack COLON expr COLON RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, X);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[:b:c] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) COLON(Colon1) expr(X1) COLON(Colon2) expr(X2) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBrack COLON expr COLON expr RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, X1);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, X2);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}
/* foo[::c] */
slice_suffix(SliceSfx) ::= LBRACK(LBrack) COLON(Colon1) COLON(Colon2) expr(X) RBRACK(RBrack) .
{
    log_trace(">>slice_sfx(SliceSfx) ::= LBrack COLON COLON expr RBRACK");
    SliceSfx = calloc(sizeof(struct node_s), 1);
    SliceSfx->type = TK_Slice_Sfx;
    SliceSfx->line  = LBrack->line;
    SliceSfx->col   = LBrack->col;
    utarray_new(SliceSfx->subnodes, &node_icd);
    utarray_push_back(SliceSfx->subnodes, LBrack);
    utarray_push_back(SliceSfx->subnodes, Colon1);
    utarray_push_back(SliceSfx->subnodes, Colon2);
    utarray_push_back(SliceSfx->subnodes, X);
    utarray_push_back(SliceSfx->subnodes, RBrack);
}

/* %%%%%%%%%%%%%%%% */
/* precedence: must be higher than PLUS, MINUS, NOT */
expr_list(XList) ::= expr(X) . [IF]
{
    log_trace(">>expr_list(XList) ::= expr(X");
    XList = X;
}

expr_list(XList) ::= expr_list(XList_rhs) COMMA(Comma) expr(X) . [IF]
{
    log_trace(">>expr_list(XList) ::= expr_list(XList_rhs) COMMA(Comma) expr(X)");
    XList = calloc(sizeof(struct node_s), 1);
    XList->type = TK_List_Expr;
    XList->line  = XList_rhs->line;
    XList->col   = XList_rhs->col;
    utarray_new(XList->subnodes, &node_icd);
    utarray_push_back(XList->subnodes, XList_rhs);
    utarray_push_back(XList->subnodes, Comma);
    utarray_push_back(XList->subnodes, X);
}

expr_list_comma(XList) ::= expr_list(XList_rhs) COMMA(Comma) . [LAMBDA]
{
    log_trace(">>expr_list_comma(XList) ::= expr_list(XList_rhs) COMMA(Comma)");
    if (XList_rhs->type != TK_List_Expr) {
        struct node_s *Xs = calloc(sizeof(struct node_s), 1);
        Xs->type = TK_List_Expr;
        Xs->line  = XList_rhs->line;
        Xs->col   = XList_rhs->col;
        utarray_new(Xs->subnodes, &node_icd);
        utarray_push_back(Xs->subnodes, XList_rhs);
        utarray_push_back(Xs->subnodes, Comma);
        XList = Xs;
    } else {
        utarray_push_back(XList_rhs->subnodes, Comma);
        XList = XList_rhs;
    }
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* LIST EXPRESSIONS */
/* PrimX (Operand) = ... | ListExpr | ListComp | ... */
/* ListExpr = '[' [Expression [',']] ']' . */
/* ListComp = '[' Test {CompClause} ']'. */
list_expr(ListX) ::= LBRACK(LBrack) expr_list(Xs) RBRACK(RBrack) .
{
    log_trace(">>list_expr(List) ::= LBRACK expr_list RBRACK");
    ListX = calloc(sizeof(struct node_s), 1);
    ListX->type = TK_List_Expr;
    ListX->line  = LBrack->line;
    ListX->col   = LBrack->col;
    utarray_new(ListX->subnodes, &node_icd);
    utarray_push_back(ListX->subnodes, LBrack);
    if (Xs->type != TK_List_Expr) {
        struct node_s *XList = calloc(sizeof(struct node_s), 1);
        XList->type = TK_List_Expr;
        utarray_new(XList->subnodes, &node_icd);
        utarray_push_back(XList->subnodes, Xs);
        utarray_push_back(ListX->subnodes, XList);
    } else {
        utarray_push_back(ListX->subnodes, Xs);
    }
    utarray_push_back(ListX->subnodes, RBrack);
}

list_expr(ListX) ::= LBRACK(LBrack) expr_list_comma(Xs) RBRACK(RBrack) .
{
    log_trace(">>list_expr(List) ::= LBRACK expr_list_comma RBRACK");
    ListX = calloc(sizeof(struct node_s), 1);
    ListX->type = TK_List_Expr;
    ListX->line  = LBrack->line;
    ListX->col   = LBrack->col;
    utarray_new(ListX->subnodes, &node_icd);
    utarray_push_back(ListX->subnodes, LBrack);
    if (Xs->type != TK_List_Expr) {
        struct node_s *XList = calloc(sizeof(struct node_s), 1);
        XList->type = TK_List_Expr;
        utarray_new(XList->subnodes, &node_icd);
        utarray_push_back(XList->subnodes, Xs);
        utarray_push_back(ListX->subnodes, XList);
    } else {
        utarray_push_back(ListX->subnodes, Xs);
    }
    utarray_push_back(ListX->subnodes, RBrack);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* LIST COMPREHENSIONS */
/* PrimX (Operand) = ... | ListExpr | ListComp | ... */
/* ListComp = '[' Test {CompClause} ']'. */
/* CompClause = 'for' LoopVariables 'in' Test | 'if' Test . */
/* LoopVariables = PrimaryExpr {',' PrimaryExpr} . */

list_comp(LComp) ::= LBRACK(LBrack) expr(X) comp_clause(Comp) RBRACK(RBrack) .
{
    log_trace(">>list_comp(LComp) ::= LBRACK expr_list comp_clause RBRACK");
    LComp = calloc(sizeof(struct node_s), 1);
    LComp->type = TK_List_Comp;
    LComp->line  = LBrack->line;
    LComp->col   = LBrack->col;
    utarray_new(LComp->subnodes, &node_icd);
    utarray_push_back(LComp->subnodes, LBrack);
    utarray_push_back(LComp->subnodes, X);
    utarray_push_back(LComp->subnodes, Comp);
    utarray_push_back(LComp->subnodes, RBrack);
}

comp_clause(Comp) ::= comp_for_clause(CompFor) .
{
    log_trace(">>comp_clause(Comp) ::= comp_for_clause");
    Comp = CompFor;
}

comp_clause(Comp) ::= comp_clause(Comp_rhs) comp_if_clause(CompIf) .
{
    log_trace(">>comp_clause(Comp) ::= comp_clause comp_if_clause");
    Comp = calloc(sizeof(struct node_s), 1);
    Comp->type = TK_Comp_Clause;
    Comp->line  = Comp_rhs->line;
    Comp->col   = Comp_rhs->col;
    utarray_new(Comp->subnodes, &node_icd);
    utarray_push_back(Comp->subnodes, Comp_rhs);
    utarray_push_back(Comp->subnodes, CompIf);
}

comp_clause(Comp) ::= comp_clause(Comp_rhs) comp_for_clause(CompFor) . [IF]
{
    log_trace(">>comp_clause(Comp) ::= comp_clause comp_for_clause");
    Comp = calloc(sizeof(struct node_s), 1);
    Comp->type = TK_Comp_Clause;
    Comp->line  = Comp_rhs->line;
    Comp->col   = Comp_rhs->col;
    utarray_new(Comp->subnodes, &node_icd);
    utarray_push_back(Comp->subnodes, Comp_rhs);
    utarray_push_back(Comp->subnodes, CompFor);
}

/* high precedence to disambiguate e.g.
[a for b in c if x] since 'c if x' matches if_expr
 */
comp_for_clause(Comp) ::= FOR(For) loop_vars(LoopVars) IN(In) expr(X) .
{
    log_trace(">>comp_clause(Comp) ::= For loop_vars IN expr");
    Comp = calloc(sizeof(struct node_s), 1);
    Comp->type = TK_Comp_Clause;
    Comp->line  = For->line;
    Comp->col   = For->col;
    utarray_new(Comp->subnodes, &node_icd);
    utarray_push_back(Comp->subnodes, For);
    /* abstract singleton loop var */
    if (LoopVars->type != TK_Loop_Vars) {
        struct node_s *LVs = calloc(sizeof(struct node_s), 1);
        LVs->type = TK_Loop_Vars;
        utarray_new(LVs->subnodes, &node_icd);
        utarray_push_back(LVs->subnodes, LoopVars);
        utarray_push_back(Comp->subnodes, LVs);
    } else {
        utarray_push_back(Comp->subnodes, LoopVars);
    }
    utarray_push_back(Comp->subnodes, In);
    utarray_push_back(Comp->subnodes, X);
}

comp_if_clause(Comp) ::= IF(If) expr(X) .
{
    log_trace(">>comp_if_clause(Comp) ::= IF expr");
    Comp = calloc(sizeof(struct node_s), 1);
    Comp->type = TK_Comp_Clause;
    Comp->line  = If->line;
    Comp->col   = If->col;
    utarray_new(Comp->subnodes, &node_icd);
    utarray_push_back(Comp->subnodes, If);
    utarray_push_back(Comp->subnodes, X);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* DICTIONARIES */
/* Operand (PrimX) = ... | DictExpr | DictComp | ... */
/* DictExpr = '{' [Entries [',']] '}' . */
/* DictComp = '{' Entry {CompClause} '}' . */
/* Entries  = Entry {',' Entry} . */
/* Entry    = Test ':' Test . */

entry_list(EList) ::= entry(Entry) . [FOR]
{
    log_trace(">>entry_list(EList) ::= entry(Entry)");
    EList = calloc(sizeof(struct node_s), 1);
    EList->type = TK_Dict_Entry_List;
    EList->line  = Entry->line;
    EList->col   = Entry->col;
    utarray_new(EList->subnodes, &node_icd);
    utarray_push_back(EList->subnodes, Entry);
}

entry_list(EList) ::= entry_list(EList_rhs) COMMA(Comma) entry(Entry) .
{
    log_trace(">>entry_list(EList) ::= entry_list(EList_rhs) entry(Entry)");
    utarray_push_back(EList_rhs->subnodes, Comma);
    utarray_push_back(EList_rhs->subnodes, Entry);
    EList = EList_rhs;
}

entry(Entry) ::= expr(X1) COLON(Colon) expr(X2) . [FOR] //highest prec
{
    log_trace(">>entry(Entry) ::= expr(X1) COLON expr(X2)");
    Entry = calloc(sizeof(struct node_s), 1);
    Entry->type = TK_Dict_Entry;
    Entry->line  = X1->line;
    Entry->col   = X1->col;
    utarray_new(Entry->subnodes, &node_icd);
    utarray_push_back(Entry->subnodes, X1);
    utarray_push_back(Entry->subnodes, Colon);
    utarray_push_back(Entry->subnodes, X2);
}

dict_expr(DictX) ::= LBRACE(LBrace) entry_list(EList) RBRACE(RBrace) .
{
    log_trace(">>dict_expr(DictX) ::= LBRACE entry_list RBRACE");
    DictX = calloc(sizeof(struct node_s), 1);
    DictX->type = TK_Dict_Expr;
    DictX->line  = LBrace->line;
    DictX->col   = LBrace->col;
    utarray_new(DictX->subnodes, &node_icd);
    utarray_push_back(DictX->subnodes, LBrace);
    /* abstract singleton entry */
    if (EList->type != TK_Dict_Entry_List) {
        log_debug("2 xxxxxxxxxxxxxxxx");
        struct node_s *Es = calloc(sizeof(struct node_s), 1);
        Es->type = TK_Dict_Entry_List;
        utarray_new(Es->subnodes, &node_icd);
        utarray_push_back(Es->subnodes, EList);
        utarray_push_back(DictX->subnodes, Es);
    } else {
        utarray_push_back(DictX->subnodes, EList);
    }
    utarray_push_back(DictX->subnodes, RBrace);
}

dict_comp(DictX) ::= LBRACE(LBrace) entry(Entry) comp_clause(DClause) RBRACE(RBrace) .
{
    log_trace(">>dict_comp(DictX) ::= LBRACE entry comp_clause RBRACE");
    DictX = calloc(sizeof(struct node_s), 1);
    DictX->type = TK_Dict_Comp;
    DictX->line  = LBrace->line;
    DictX->col   = LBrace->col;
    utarray_new(DictX->subnodes, &node_icd);
    utarray_push_back(DictX->subnodes, LBrace);
    utarray_push_back(DictX->subnodes, Entry);
    utarray_push_back(DictX->subnodes, DClause);
    utarray_push_back(DictX->subnodes, RBrace);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* PAREN EXPRESSIONS */
/* Operand (PrimX) = ... | '(' [Expression [',']] ')' */

paren_expr(ParenX) ::= LPAREN(LParen) expr_list(Xs) RPAREN(RParen) .
{
    log_trace(">>list_expr(Paren) ::= LPAREN expr_list RPAREN");
    ParenX = calloc(sizeof(struct node_s), 1);
    ParenX->type  = TK_Paren_Expr;
    ParenX->line  = LParen->line;
    ParenX->col   = LParen->col;
    utarray_new(ParenX->subnodes, &node_icd);
    utarray_push_back(ParenX->subnodes, LParen);
    utarray_push_back(ParenX->subnodes, Xs);
    utarray_push_back(ParenX->subnodes, RParen);
}

paren_expr(ParenX) ::= LPAREN(LParen) expr_list_comma(Xs) RPAREN(RParen) .
{
    log_trace(">>list_expr(Paren) ::= LPAREN expr_list_comma RPAREN");
    ParenX = calloc(sizeof(struct node_s), 1);
    ParenX->type = TK_Paren_Expr;
    ParenX->line  = LParen->line;
    ParenX->col   = LParen->col;
    utarray_new(ParenX->subnodes, &node_icd);
    utarray_push_back(ParenX->subnodes, LParen);
    utarray_push_back(ParenX->subnodes, Xs);
    utarray_push_back(ParenX->subnodes, RParen);
}

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* UNARY EXPRESSIONS */
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
binop ::= LANGLE . { log_trace(">>binop ::= LANGLE"); }
binop ::= LE . { log_trace(">>binop ::= LE"); }
binop ::= RANGLE . { log_trace(">>binop ::= RANGLE"); }
binop ::= GE . { log_trace(">>binop ::= GE"); }
binop ::= IN . { log_trace(">>binop ::= IN"); }
binop ::= NOT IN . [IN] { log_trace(">>binop ::= NOT IN"); }
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
/* LambdaExpr = 'lambda' [Parameters] ':' Test . */

lambda_expr(LambdaX) ::= LAMBDA(Lambda) COLON(Colon) expr(X) .
{
    log_trace(">>lambda_expr ::= LAMBDA COLON expr");
    LambdaX = calloc(sizeof(struct node_s), 1);
    LambdaX->type = TK_Lambda_Expr;
    LambdaX->line  = Lambda->line;
    LambdaX->col   = Lambda->col;
    utarray_new(LambdaX->subnodes, &node_icd);
    utarray_push_back(LambdaX->subnodes, Lambda);
    utarray_push_back(LambdaX->subnodes, Colon);
    utarray_push_back(LambdaX->subnodes, X);
}

lambda_expr(LambdaX) ::= LAMBDA(Lambda) param_list(Params) COLON(Colon) expr(X) .
{
    log_trace(">>lambda_expr ::= LAMBDA params COLON expr");
    LambdaX = calloc(sizeof(struct node_s), 1);
    LambdaX->type = TK_Lambda_Expr;
    LambdaX->line  = Lambda->line;
    LambdaX->col   = Lambda->col;
    utarray_new(LambdaX->subnodes, &node_icd);
    utarray_push_back(LambdaX->subnodes, Lambda);
    utarray_push_back(LambdaX->subnodes, Params);
    utarray_push_back(LambdaX->subnodes, Colon);
    utarray_push_back(LambdaX->subnodes, X);
}

%endif // EXPRESSIONS

