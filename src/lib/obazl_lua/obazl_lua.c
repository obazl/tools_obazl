#include <stdarg.h>

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "utstring.h"

#include "obazl_lua.h"

lua_State *L;
UT_string *lua_file;
char *lua_file_name = "obazl.lua";

void lerror (lua_State *L, const char *fmt, ...) {
    va_list argp;
    va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
    va_end(argp);
    lua_close(L);
    exit(EXIT_FAILURE);
}
