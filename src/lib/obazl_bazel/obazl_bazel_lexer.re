// Bazel BUILDfile lexer
// re2c $INPUT -o $OUTPUT -i

// starlark grammar: https://github.com/bazelbuild/starlark/blob/master/spec.md#grammar-reference
// gazelle lexer:  https://github.com/bazelbuild/buildtools/blob/master/build/lex.go

#include <assert.h>
#include <errno.h>
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/errno.h>

#include "utarray.h"
#include "log.h"
/* #include "tokens.h" */
#include "obazl_bazel_lexer.h"

// for mtags:
static const int ROOT = -1;

#if EXPORT_INTERFACE
#define BUFSIZE 1024
#ifndef MAX_DEPS
#define MAX_DEPS 64
#endif
#endif

/* const char *deps[MAX_DEPS] = {}; */
int curr_tag = 0;

/*!types:re2c */

/* struct load_syms_s { */
/*     int idx; */
/*     const char *alias; */
/*     const char *sym; */
/* }; */

UT_icd loadsyms_icd = {sizeof(char**), NULL, NULL, NULL};

#if EXPORT_INTERFACE
struct bzl_token_s
{
    int line, col;
    /* type STRING_LIT */
    char *s;                    /* raw string for other types */

    union {
        /* type LOAD: raw string, label (file), load syms, aliases */
        struct {
            char *label;
            int sym_ct;
            UT_array *syms; /* array of char**, not strings (char*) */
        } load;
    };
};

struct position_s {
    int line; // line in input (starting at 1)
    int line_rune; // rune in line (starting at 1)
    int col; // col in input (starting at 0)
};

struct bf_lexer
{
    const char *filename;
    /* from gazelle */
    struct position_s pos;  // current input position
    //lineComments   []Comment // accumulated line comments
    //suffixComments []Comment // accumulated suffix comments
    int depth;        // nesting of [ ] { } ( )
    bool clean_line;   // true if the current line only contains whitespace before the current position
    int indent;       // current line indentation in spaces
    int indents[12];    //    []int     // stack of indentation levels in spaces

    /* re2c */
    /* On entry, YYCURSOR is assumed to point to the first character
       of the current token. On exit, YYCURSOR will point to the first
       character of the following token. */
    const char *cursor;         /* YYCURSOR */
    const char *tok;            /* YYCURSOR on entry, i.e. start of tok */
    const char *sol;            /* start of line ptr */
    const char *limit;
    const char *marker;
    int mode;                   /* i.e. start condition */

    /* stags - re2c will insert const char *yyt1, yyt2, etc.  These are ptrs to the recognized tags. */
    /*!stags:re2c format = 'const char *@@;'; */

    /* mtags - re2c will insert 'int yyt3', 'int yyt4', etc. These are subexpr indices, not ptrs to the tokens recognized. */
    /*!mtags:re2c format = 'int @@;'; */
};

#endif

int return_token(int tok)
{
    return tok;
}


/* typedef std::vector<Mtag> MtagTree; */

/* loadsym_push will be called twice per token, once for start mtag, once for end mtag */
/* static void loadsym_push(int *idx, const char *t, struct node_s **mtok) */
/* { */
/*     static int ct = 0; */
/*     log_debug("loadsym_push: %d, ct: %d, s: %s", *idx, ct, t); */
/*     /\*  if ( (ct % 2) == 0) { *\/ */
/*     /\*     log_debug("pushing new load sym w/alias"); *\/ */
/*     /\* } *\/ */
/*     /\* we push the ptr, not a string *\/ */
/*     utarray_push_back((*mtok)->load.syms, &t); */
/*     (*mtok)->load.sym_ct++; */
/*     (*idx)++; */
/*     ct++; */
/*     log_debug("subexpr ct: %d", utarray_len((*mtok)->load.syms)); */

/*     /\* Mtag m = {*idx, t}; *\/ */
/*     /\* *idx = (int)tree->size(); *\/ */
/*     /\* tree->push_back(m); *\/ */
/* } */

/* called for non-matching optional subexprs (i.e. soid, eoid) */
/* static void loadsym_null(int *idx, struct node_s **tok) */
/* { */
/*     static int ct; */
/*     log_debug("loadsym_null: %d, ct: %d", *idx, ct); */
/*    if ( (ct % 2) == 0) { */
/*        log_debug("pushing new load sym w/o alias"); */
/*    } */
/*     (*idx)++; */
/*     ct++; */
/* } */

int get_next_token(struct bf_lexer *lexer, struct node_s **mtok)
{
    log_debug("");
    //int c = yycinit;
    /* lexer->mode = yycinit; */
    int countNL = 0;
    /* const char *YYMARKER; */
    // stags - to be used as @s1, @s2 in regexps.
    const char *s1, *s2;
    // not used: local defs /*!stags:re2c format = "const char *@@;"; */
    // see struct bf_lexer_s above

    // mtags - to be used as #sosym, #eosym, #soid, #eoid in regexpr.
    // not used: local defns: /*!mtags:re2c format = "int @@ = 0;"; */
    // defs are in struct bf_lexer_s
    int sosym, eosym;           /* after lexing, will contain count of syms found */
    int soid, eoid;

loop:
    /* log_debug("loop lexmode: %d", lexer->mode); */
    curr_tag = 0;
    lexer->tok = lexer->cursor;
    /*!re2c
      re2c:api:style = free-form;
      re2c:define:YYCTYPE = char;
      re2c:define:YYCURSOR = lexer->cursor;
      re2c:define:YYLIMIT = lexer->limit;
      re2c:define:YYMARKER = lexer->marker;
      re2c:define:YYGETCONDITION = "lexer->mode";
      re2c:define:YYSETCONDITION = "lexer->mode = @@;";

      // The way tag variables are accessed from the lexer (not needed if tag variables are defined as local variables).
      re2c:tags:expression = "lexer->@@";
      // for the following, '@@' will be replaced by 'lexer->@@' (re2c:tags:expression)
      /* re2c:define:YYMTAGP = "loadsym_push(&@@, lexer->cursor, mtok);"; // , mtag_list);"; */
      /* re2c:define:YYMTAGN = "loadsym_push(&@@, NULL, mtok);"; // , NULL, mtag_list);"; */

      re2c:flags:tags = 1;
      re2c:yyfill:enable  = 0;

      end    = "\x00";
      eol    = "\n";
      ws     = [ \t]*;
      wsnl   = [ \t\n]*;

    <!init> {
        // lexer->pos.col = lexer->tok - lexer->sol;
        (*mtok)->line = lexer->pos.line;
        (*mtok)->col  = lexer->tok - lexer->sol;
        // productions using tags must explicitly set mtok line and col
    }

    <*> " " {
        lexer->pos.col++;
        if (lexer->clean_line) {
            lexer->indent++;
        }
        goto loop;
    }

    <*> eol {
        lexer->sol = lexer->cursor;
        lexer->pos.line++;
        lexer->pos.col = 0;
        lexer->indent = 0;
        lexer->clean_line = true;
        if (lexer->depth == 0) {
            // Not in a statememt. Tell parser about top-level blank line.
            /* in.startToken(val); */
            /* in.readRune(); */
            /* in.endToken(val); */
            // return TK_NEWLINE;
        }
        countNL++;
        goto loop;
    }

    sqstring_lit = [^']+;
    <sqstr> @s1 sqstring_lit @s2 "'" => init {
            log_debug("<sqstr>, mode: %d", lexer->mode);
            (*mtok)->line = lexer->pos.line;
            (*mtok)->col  = lexer->tok - lexer->sol;
            (*mtok)->s = strndup(lexer->tok, lexer->cursor - lexer->tok);
            return TK_STRING;
      }
    <init> "\'" :=> sqstr

    dqstring_lit = [^"]+;  // ([a-zA-Z0-9=/_-:.])+;
    <dqstr> @s1 dqstring_lit @s2 "\"" => init {
            (*mtok)->s = strndup(lexer->tok, lexer->cursor - lexer->tok);
            log_debug("<dqstr>: %s", (*mtok)->s);
            /* lexer->clean_line = false; */
            /* lexer->pos.col = lexer->tok - lexer->sol; */
            (*mtok)->line = lexer->pos.line;
            (*mtok)->col  = lexer->tok - lexer->sol;
            /* (*mtok)->s = strndup(s1, (size_t)(s2 - s1)); */
            return TK_STRING;
      }
    <init> "\"" :=> dqstr

      // IDENTIFIERS "an identifier is a sequence of
      // Unicode letters, decimal digits, and underscores (_), not
      // starting with a digit.

      // from https://re2c.org/manual/manual_c.html#encoding-support
      id_start = [a-zA-Z_];
      id_continue = id_start | [0-9];
      // id_start    = L | Nl | '_'; // Letters, Number letters
      // id_continue = id_start | Mn | Mc | Nd | Pc | [\u200D\u05F3];
      identifier  = id_start id_continue*;

      // PUNCTUATION
/*
      +    -    *    //   %    **
      ~    &    |    ^    <<   >>
      .    ,    =    ;    :
      (    )    [    ]    {    }
      <    >    >=   <=   ==   !=
      +=   -=   *=   //=  %=
      &=   |=   ^=   <<=  >>=
    */

    <init> "**" { return return_token(TK_STARSTAR); }
    <init> "->" { return TK_ARROW; }
    <init> "<=" { return TK_LE; }
    <init> ">=" { return TK_GE; }

    <init> "." { return TK_DOT; }
    <init, load> "," { return TK_COMMA; }
    <init> ";" { return TK_SEMI; }
    <init> ":" { return TK_COLON; }
    <init> "!=" { return TK_BANG_EQ; }
    <init> "!"  { return TK_BANG; }
    <init> "+=" { return TK_PLUS_EQ; }
    <init> "+" { return TK_PLUS; }
    <init> "-=" { return TK_MINUS_EQ; }
    <init> "-" { return TK_MINUS; }
    <init> "*=" { return TK_STAR_EQ; }
    <init> "*" { return TK_STAR; }
    <init> "//=" { return TK_DIVDIV_EQ; }
    <init> "//" { return TK_DIVDIV; }
    <init> "/=" { return TK_DIV_EQ; }
    <init> "/" { return TK_SLASH; }
    <init> "%=" { return TK_PCT_EQ; }
    <init> "%" { return TK_PCT; }
    <init> "&=" { return TK_AMP_EQ; }
    <init> "&" { return TK_AMP; }
/*    <init> "|=" { return TK_VBAR_EQ; } */
    <init> "|" { return TK_VBAR; }
    <init> "^=" { return TK_CARET_EQ; }
    <init> "^" { return TK_CARET; }
    <init> "~" { return TK_TILDE; }
    <init> "[" { return TK_LBRACK; }
    <init> "]" { return TK_RBRACK; }
    <init> "{" { return TK_LBRACE; }
    <init> "}" { return TK_RBRACE; }
    <init> "(" {
        /* log_debug("<init> LPAREN"); */
            lexer->pos.col = lexer->tok - lexer->sol;
            /* (*mtok)->s = NULL; */
            return TK_LPAREN;
        }
    /* <load> "(" { */
    /*         log_debug("<load> LPAREN, mode: %d", lexer->mode); */
    /*         lexer->pos.col = lexer->tok - lexer->sol; */
    /*         /\* (*mtok)->s = NULL; *\/ */
    /*         return TK_LPAREN; */
    /*     } */
    <init> ")"  {
            log_debug("<init> RPAREN, mode: %d", lexer->mode);
            /* (*mtok)->pos.line = lexer->pos.line; */
            lexer->pos.col = lexer->tok - lexer->sol;
            /* (*mtok)->s = NULL; */
            return TK_RPAREN;
        }
    /* <load> ")" => init { */
    /*         log_debug("<load> RPAREN, mode: %d", lexer->mode); */
    /*         /\* (*mtok)->pos.line = lexer->pos.line; *\/ */
    /*         lexer->pos.col = lexer->tok - lexer->sol; */
    /*         /\* (*mtok)->s = NULL; *\/ */
    /*         return TK_RPAREN; */
    /*     } */
    <init> "<<=" { return TK_LLANGLE_EQ; }
    <init> "<<"  { return TK_LLANGLE; }
    <init> "<"   { return TK_LANGLE; }
    <init> ">>=" { return TK_RRANGLE_EQ; }
    <init> ">>"  { return TK_RRANGLE; }
    <init> ">"   { return TK_RANGLE; }
    <init> "=="  { return TK_EQEQ; }
    <init> "="   { return TK_EQ; }

    /* LITERALS: integer, floating point, string, byte  */

    <init> "\"\"\"" { return TK_SQ3; } /* triple quoted string delim */
    <init> "r'" { return TK_RSQ; } /* raw single-quoted string */
    <init> "r\"" { return TK_RDQ; }        /* raw single-quoted string */
    <init> "b'"  { return TK_BSQ; }         /* single-quoted bytestring, e.g. b'hello' */
    <init> "b\"" { return TK_BDQ; }        /* double-quoted bytestring, e.g. b"hello" */
    <init> "b'''" { return TK_BSQ3; } /* triple single-quoted bytestring */
    <init> "b\"\"\"" { return TK_BDQ3; } /* triple single-quoted bytestring */
    <init> "rb'" { return TK_RBSQ; }        /* raw bytes literal, rb'hello' */
    <init> "rb\"" { return TK_RBDQ; }        /* raw bytes literal, rb"hello" */

        /* string escapes */
        /* \a   \x07 alert or bell */
        /* \b   \x08 backspace */
        /* \f   \x0C form feed */
        /* \n   \x0A line feed */
        /* \r   \x0D carriage return */
        /* \t   \x09 horizontal tab */
        /* \v   \x0B vertical tab */
    <init> "\\" { return TK_ESC_BACKSLASH; }

        /* octals */
        /* '\0'			# "\x00"  a string containing a single NUL element */
        /* '\12'			# "\n"    octal 12 = decimal 10 */
        /* '\101-\132'		# "A-Z" */
        /* '\119'			# "\t9"   = "\11" + "9" */

        /* '\x00'			# "\x00"  a string containing a single NUL element */
        /* '\0A'			# "\n"    hexadecimal A = decimal 10 */
        /* "\x41-\x5A"             # "A-Z" */

        /* hex */
        /* '\u0041'		# "A", an ASCII letter (U+0041) */
        /* '\u0414' 		# "Д", a Cyrillic capital letter (U+0414) */
        /* '\u754c                 # "界", a Chinese character (U+754C) */
        /* '\U0001F600'            # "😀", an Emoji (U+1F600) */

        /* KEYWORDS */
    <init> "and" { return TK_AND; }
    <init> "else" { return TK_ELSE; }

    /* LoadStmt = 'load' '(' string {',' [identifier '='] string} [','] ')' . */
    <init> "load" {
        log_debug("<init> LOAD, mode: %d", lexer->mode);
        return TK_LOAD;
    }
    <init> "break" { return TK_BREAK; }
    <init> "for" { return TK_FOR; }
    <init> "not" { return TK_NOT; }
    <init> "continue" { return TK_CONTINUE; }
    <init> "if" { return TK_IF; }
    <init> "or" { return TK_OR; }
    <init> "def" { return TK_DEF; }
    <init> "in" { return TK_IN; }
    <init> "pass" { return TK_PASS; }
    <init> "elif" { return TK_ELIF; }
    <init> "lambda" { return TK_LAMBDA; }
    <init> "return" { return TK_RETURN; }

        /* RESERVED */
        /* "The tokens below also may not be used as identifiers although they do not appear in the grammar; they are reserved as possible future keywords:"
         */
    <init> "as" { return TK_AS; }
    <init> "import" { return TK_IMPORT; }
    <init> "assert" { return TK_ASSERT; }
    <init> "is" { return TK_IS; }
    <init> "class" { return TK_CLASS; }
    <init> "nonlocal" { return TK_NONLOCAL; }
    <init> "del" { return TK_DEL; }
    <init> "raise" { return TK_RAISE; }
    <init> "except" { return TK_EXCEPT; }
    <init> "try" { return TK_TRY; }
    <init> "finally" { return TK_FINALLY; }
    <init> "while" { return TK_WHILE; }
    <init> "from" { return TK_FROM; }
    <init> "with" { return TK_WITH; }
    <init> "global" { return TK_GLOBAL; }
    <init> "yield" { return TK_YIELD; }

    <init> identifier {
        (*mtok)->s = strndup(lexer->tok, lexer->cursor - lexer->tok);
        return TK_ID;
    }

    <init> [0-9] { return TK_NUMBER; }

    inline_cmt = [^\n]+;
    <inline_cmt> inline_cmt => init {
            (*mtok)->line = lexer->pos.line;
            /* start of comment is stag 's1', not lexer->tok */
            /* end of comment is cursor, not @s2! */
            (*mtok)->col  = s1 - lexer->sol;
            (*mtok)->s = strndup(s1, (size_t)(lexer->cursor - s1));
            return TK_COMMENT;
        }
    <init> ws* @s1 "#" @s2 :=> inline_cmt

    <init> end       {
        printf("<init> ending\n");
        return 0;
        }

    <*> *         {
            fprintf(stderr, "ERROR lexing: %s\n", lexer->tok);
            exit(-1);
        }

    */
}

void lexer_init(struct bf_lexer *lexer,
                const char *sob /* start of buffer */ )
{
    lexer->cursor = sob;
    // lexer->pos.line = 1;
    lexer->sol = sob;
}
