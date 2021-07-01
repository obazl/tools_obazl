/*
** lua lib for Obazl Dune Parser
*/

#define obazl_dune_c
#define LUA_LIB

#include <errno.h>
#include <limits.h>
#include <stdlib.h>

#include "lua.h"

#include "lauxlib.h"
#include "lualib.h"

#include "log.h"
#include "utarray.h"
#include "uthash.h"
#include "utstring.h"

#include "obazl.h"
#include "obazl_dune.h"

/* **************************************************************** */
static int parse_file (lua_State *L) {
    size_t l;
    const char *fname = luaL_checklstring(L, 1, &l);
    /* obazl_meta_package *pkg = obazl_meta_parse_file((char*)fname); */

    struct obazl_dune_package_s *parsed = obazl_dune_parse_file((char*)fname);
    if (parsed == NULL) {
        return luaL_error(L, "%s: %s", fname, strerror(errno));
    } else {
        log_debug("parsed dune file: %s\n", fname);
        log_debug("stanza ct: %d", utarray_len(parsed->stanzas));
        log_debug("2 stanza ct: %d", utarray_len((parsed)->stanzas));

    }

    lua_newtable(L);
    lua_pushstring(L, "path");
    lua_pushstring(L, parsed->path);
    lua_settable(L, -3);

    lua_pushstring(L, "stanzas");
    stanzas_table(L, parsed);
    lua_settable(L, -3);

    return 1;
}

static int version (lua_State *L) {
    char *v = obazl_dune_version();
    lua_pushstring(L, v);
    return 1;
}

static const luaL_Reg obazl_dune[] = {
  {"parse_file" , parse_file},
  {NULL         , NULL}
};

/*
** Open obazl library - e.g. require 'obazl'
*/
EXPORT LUAMOD_API int luaopen_obazl_dune (lua_State *L) {
    log_set_level(LOG_TRACE);
    log_set_quiet(false);

    /* log_info("luaopen_obazl_dir"); */

    /* metatables */
    /* luaL_newmetatable(L, "OBazl.dune.pkg"); */
    /* lua_pushcfunction(L, dune_pkg_gc); */
    /* lua_setfield(L, -2, "__gc"); */
    /* lua_pushcfunction(L, dune_pkg_index); */
    /* lua_setfield(L, -2, "__index"); */
    /* lua_pushcfunction(L, dune_pkg_pairs); */
    /* lua_setfield(L, -2, "__pairs"); */

    /* create the library */

    luaL_newlib(L, obazl_dune);
    /* add more stuff to the table created by luaL_newlib ... */
    lua_pushstring(L, "0.2.0");
    lua_setfield(L, -2, "version");
    return 1;
}
