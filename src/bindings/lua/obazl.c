/*
** lua lib for Obazl Dune Parser
*/

#define lomplib_c
#define LUA_LIB

#include <errno.h>
#include <limits.h>
#include <stdlib.h>

#include "lua.h"

#include "lauxlib.h"
#include "lualib.h"

#include "obazl.h"
#include "obazl_dir.h"
#include "obazl_dune.h"

/* void error (lua_State *L, const char *fmt, ...) */
/* { */
/*     va_list argp; */
/*     va_start(argp, fmt); */
/*     vfprintf(stderr, argp); */
/*     va_end(argp); */
/*     lua_close(L); */
/*     exit(EXIT_FAILURE); */
/* } */

// FIXME: rename meta_parse_file, or move to obazl_meta.c
static int parse_file (lua_State *L) {
    size_t l;
    const char *fname = luaL_checklstring(L, 1, &l);
    obzl_meta_package *pkg = obzl_meta_parse_file((char*)fname);
    if (pkg == NULL) {
        return luaL_error(L, "%s: %s", fname, strerror(errno));
    } else {
        printf("pkg name: %s\n", obzl_meta_package_name(pkg));
    }
    /* push pkg userdata??? */
    obzl_meta_package **bp = lua_newuserdata(L, sizeof(pkg));
    *bp = pkg;
    return 1;
}

static int version (lua_State *L) {
    char *v = obzl_meta_version();
    lua_pushstring(L, v);
    return 1;
}

static const luaL_Reg lobazl[] = {
  {"parse_file" , parse_file},
  {"dune"       , luaopen_obazl_dune},
  {"dir"        , luaopen_obazl_dir},
  {NULL         , NULL}
};

/*
** Open obazl library - e.g. require 'obazl'
*/
LUAMOD_API int luaopen_obazl (lua_State *L) {
  luaL_newlib(L, lobazl);
  /* add more stuff to the table created by luaL_newlib ... */
  lua_pushstring(L, "0.2.0");
  lua_setfield(L, -2, "version");
  return 1;
}
