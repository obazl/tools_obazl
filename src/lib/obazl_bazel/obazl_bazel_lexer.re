// Bazel BUILDfile lexer
// re2c $INPUT -o $OUTPUT -i

// /*!include:re2c "unicode_categories.h" */

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

#include "obazl_bazel_lexer.h"

#if EXPORT_INTERFACE
#define BUFSIZE 1024
#ifndef MAX_DEPS
#define MAX_DEPS 64
#endif
#endif

const char *deps[MAX_DEPS] = {};
int curr_tag = 0;

#if EXPORT_INTERFACE
union meta_token
{
    char *s;
};

struct meta_lexer
{
    const char *tok;
    const char *cursor;
    const char *limit;
    const char *marker;
};
#endif

/* static void mtag(const char *t) */
static void mtag(int t)
{
    /* fprintf(stderr, "mtag ctor idx: %d, s: %.22s\n", curr_tag, t); */
    /* deps[curr_tag++] = t; */
}


/* static void print_tags() { */
/*     /\* fprintf(stderr, "printing %d tags:\n", curr_tag/2); *\/ */
/*     for (int i=0; i < curr_tag/2 ; i++) { */
/*         fprintf(stderr, "\tVALTOK: '%*s'\n", (int)(deps[i*2+1] - deps[i*2]), deps[i*2]); */
/*     } */
/*     /\* fprintf(stderr, "done\n"); *\/ */
/* } */

#define YYMTAGP(t) mtag(YYCURSOR)
#define YYMTAGN(t) mtag(NULL)

int get_next_token(struct meta_lexer *lexer, union meta_token *mtok)
{
    /* const char *YYMARKER; */

    /* stags */
    const char *s1, *s2;        /* dq strings */

    /* mtags */
    int f1, f2;                 /* flags */
    /*!stags:re2c format = "const char *@@;"; */
    /*!mtags:re2c format = "int @@;"; */
loop:
    curr_tag = 0;
    lexer->tok = lexer->cursor;
    /*!mtags:re2c format = "@@ = -1;"; */
    /*!re2c
      re2c:define:YYCTYPE = char;
      re2c:define:YYCURSOR = lexer->cursor;
      re2c:define:YYLIMIT = lexer->limit;
      re2c:define:YYMARKER = lexer->marker;
      re2c:define:YYMTAGP = mtag;
      re2c:define:YYMTAGN = mtag;
      re2c:yyfill:enable  = 0;

      re2c:flags:tags = 1;

      end    = "\x00";
      eol    = "\n";
      ws     = [ \t]*;
      listws = [ \t\n,]*;
      wsnl   = [ \t\n]*;

      // FIXME: indent, outdent

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

    STAR2   = "**";
        ARROW   = "->";
        LE      = "<=";
        GE      = ">=";
        LE      = "<=";

        EQ      = "=";

        DOT     = ".";
        COMMA   = ",";
        SEMI    = ";";
        COLON   = ":";

        "!=" { return BANG_EQ; }
        BANG    = "!";
        "+=" { return PLUS_EQ; }
        PLUS    = "+";
        "-=" { return MINUS_EQ; }
        MINUS   = "-";
        "*=" { return STAR_EQ; }
        STAR    = "*";
        "//=" { return DIVDIV_EQ; }
        "/=" { return DIV_EQ; }
        SLASH   = "/";
        "%=" { return PCT_EQ; }
        PCT     = "%";
        "&=" { return AMP_EQ; }
        AMP     = "&";
        "|=" { return VBAR_EQ; }
        VBAR    = "|";
        "^=" { return CARET_EQ; }
        CARET   = "^"

        TILDE   = "~";

        LBRACK  = "[";
        RBRACK  = "]";
        LBRACE  = "{";
        RBRACE  = "}";
        LPAREN  = "(";
        RPAREN  = ")";

        "<<=" { return LLANGLE_EQ; }
        "<<"  { return LLANGLE; }
        "<"   { return LANGLE; }
        ">>=" { return RRANGLE_EQ; }
        ">>"  { return RRANGLE; }
        ">"   { return RANGLE; }

        "==" { return EQ; }
        "="  { return EQEQ; }

        /* LITERALS: integer, floating point, string, byte  */

        SQ3     = "\"\"\"";     /* triple quoted string delim */
        RSQ     = "r'";         /* raw single-quoted string */
        RDQ     = "r\"";        /* raw single-quoted string */

        BSQ     = "b'";         /* single-quoted bytestring, e.g. b'hello' */
        BDQ     = "b\"";        /* double-quoted bytestring, e.g. b"hello" */
        BSQ3    = "b'''";       /* triple single-quoted bytestring, e.g. b'''hello''' */
        BDQ3    = "b"""";       /* triple single-quoted bytestring, e.g. b"""hello""" */
        RBSQ    = "rb'";        /* raw bytes literal, rb'hello' */
        RBDQ    = "rb\"";        /* raw bytes literal, rb"hello" */

        SQ      = "'";
        DQ      = "\"";

        /* string escapes */
        /* \a   \x07 alert or bell */
        /* \b   \x08 backspace */
        /* \f   \x0C form feed */
        /* \n   \x0A line feed */
        /* \r   \x0D carriage return */
        /* \t   \x09 horizontal tab */
        /* \v   \x0B vertical tab */
        ESC_BACKSLASH = "\\";

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
        /*
and            else           load
break          for            not
continue       if             or
def            in             pass
elif           lambda         return
        */
        ws "for" ws { return FOR; }
        ws "if" ws { return IF; }
        ws "or" ws { return OR; }
        ws "and" ws { return AND; }
        ws "not" ws { return NOT; }
        ws "in" ws { return IN; }
        // etc.

        /* RESERVED */
        /* "The tokens below also may not be used as identifiers although they do not appear in the grammar; they are reserved as possible future keywords:"

as             import
assert         is
class          nonlocal
del            raise
except         try
finally        while
from           with
global         yield
        */

      // IDENTIFIERS "an identifier is a sequence of
      // Unicode letters, decimal digits, and underscores (_), not
      // starting with a digit.

        // from https://re2c.org/manual/manual_c.html#encoding-support
        id_start    = L | Nl | '_'; // Letters, Number letters
        id_continue = id_start | Mn | Mc | Nd | Pc | [\u200D\u05F3];
        identifier  = id_start id_continue*;

        "#" .* eol { //FIXME: excluding newline
            /* return COMMENT; */
            goto loop;          /* skip comments */
        }

        wsnl @s1 "requires" @s2 wsnl { return REQUIRES; }

        wsnl "(" @s1 (listws #f1 FLAG #f2 listws)* @s2 ")" wsnl {
        /* we leave it to the parser to tokenize, now that we know each flag is syntactically correct */
            mtok->s = strndup(s1, (size_t)(s2 - s1));
            return FLAGS;

        }

        *         {
            fprintf(stderr, "ERROR lexing: %s\n", lexer->cursor);
            exit(-1);
        }
        end       {
        /* printf("ending\n"); */
        return 0;
        }
        ws | eol {
        // printf("looping\n");
        goto loop;
        }

    */
}

void lexer_init(struct meta_lexer *lexer, const char *input)
{
    lexer->cursor = input;
}
