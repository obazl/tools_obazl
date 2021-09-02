#include <stdarg.h>
#include <pthread.h>
#include <unistd.h>

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "log.h"
#include "utstring.h"

#include "obazl_lua.h"

lua_State *L;

UT_string *default_lua_dir;
UT_string *default_lua_file;
char *default_lua_file_name = "obazl.lua";
UT_string *user_lua_file;

void lerror (lua_State *L, const char *fmt, ...) {
    va_list argp;
    va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
    va_end(argp);
    lua_close(L);
    exit(EXIT_FAILURE);
}

/**
   to be run after codept init; calls user-provided lua fn 'init'
 */
void l_init (void)
{
    log_debug("l_init, state: %p", L);

    int rc;

    /* for iterating dirty_feps array */
    char **fdep = NULL;
    char *result;

    /* get global 'bazel' table created by obazl_config_lua */
    /* lua_getglobal(L, "bazel");  /\* bazel table *\/ */

    /* l_install_packages(); */

    /* l_install_modules(); */


    /* 2. call user-provided lua fn  */
    /* push functions and arguments */
    if (utstring_len(user_lua_file) > 0) {
        lua_getglobal(L, "init");   /* user-provided */
    } else {
        lua_getglobal(L, "bazel");
        lua_getfield(L, -1, "init"); /* tos: bazel.packages (table) */
    }

    /* do the call (0 arguments, 0 results) */
    if (lua_pcall(L, 0, 0, 0) != 0) {
        log_error("error running lua fn: init");
        lerror(L, "error running function `init': %s", lua_tostring(L, -1));
    }

    /* retrieve result */
    /* if (!lua_isstring(L, -1)) { */
    /*     log_error("function `init' must return a string"); */
    /*     lerror(L, "function `init' must return a string"); */
    /* } */
    /* const char *msg = lua_tostring(L, -1); */
    /* lua_pop(L, 1);  /\* pop returned value *\/ */

    log_debug("lua user-provided 'init' returned");

    return;
}

/**
   lua search path always contains:
       <proj_root>/.obazl.d/
       if run by `bazel run`:
           <exec_root>/<runfiles_dir>/  -- contains default `obazl.lua`
       else:
           ???
    for runtime files: see https://github.com/bazelbuild/bazel/issues/10022
        https://github.com/laszlocsomor/bazel/commit/21989926c1a002709ec3eba9ee7a992506f2d50a
 */
void _config_lua_search_path(void) // (UT_string *proj_root, UT_string *lua_srcdir)
{
    log_debug("_config_lua_search_path");
    UT_string *lua_path;
    utstring_new(lua_path);

    /* char *_proj_root = getenv("BUILD_WORKSPACE_DIRECTORY"); */
    utstring_printf(lua_path, "%s/?.lua", utstring_body(obazl_d));
    log_debug("lua_path: %s", utstring_body(lua_path));

    size_t sz;
    utstring_printf(lua_path, ";%s/%s/?.lua",
                    utstring_body(runfiles_root),
                    "external/obazl/src/lua");
                    //getcwd(NULL, sz), "external/obazl/src/lua");
    log_debug("lua_path: %s", utstring_body(lua_path));

    lua_getglobal(L, "package");
    lua_pushstring(L, utstring_body(lua_path));
    lua_setfield(L, -2, "path");
    lua_pop(L, 1);

    utstring_free(lua_path);
}

/**
   create lua state, global pkgs var, load user lua file with onchange callback
*/
void obazl_config_lua(char *_default_lua_file)
{
    log_debug("obazl_config_lua, pid: %d, tid: %d", getpid(), pthread_self());
    log_debug("default_lua_file: %s", _default_lua_file);

    utstring_new(default_lua_dir);
    utstring_printf(default_lua_dir,
                    "%s/%s",
                    utstring_body(runfiles_root),
                    "external/obazl/src/lua");
    log_debug("default_lua_dir: %s", utstring_body(default_lua_dir));

    utstring_new(default_lua_file);
    utstring_printf(default_lua_file,
                    "%s",
                    /* utstring_body(default_lua_dir), */
                    _default_lua_file);
    log_debug("default_lua_file: %s", utstring_body(default_lua_file));

    char buff[256];
    L = luaL_newstate();        /* set global lua state var */
    luaL_openlibs(L);

    lua_newtable(L);
    lua_pushstring(L, "packages"); /* key */
    lua_newtable(L);
    lua_settable(L, -3);
    lua_pushstring(L, "modules"); /* key */
    lua_newtable(L);
    lua_settable(L, -3);
    lua_setglobal(L, "bazel");

    _config_lua_search_path();
}

/**
   default: <runfiles_dir>/
*/
void obazl_init_lua(void) // char *_lua_file) // (UT_string *_lua_file)
{
    log_debug("obazl_init_lua"); // : %s", _lua_file);

    utstring_new(user_lua_file);
    utstring_printf(user_lua_file, "%s/%s", utstring_body(default_lua_dir), utstring_body(default_lua_file));
    log_debug("loading lua file %s", utstring_body(user_lua_file));
    if (luaL_loadfile(L, utstring_body(user_lua_file)) || lua_pcall(L, 0, 0, 0))
            lerror(L, "cannot run configuration file: %s\n",
                   lua_tostring(L, -1));
    log_debug("loaded default lua file: %s", utstring_body(user_lua_file));

    utstring_clear(user_lua_file);
    utstring_printf(user_lua_file, "%s/%s", utstring_body(obazl_d), utstring_body(default_lua_file));
    int rc = access(utstring_body(user_lua_file), R_OK);
    if (!rc) { /* found */
        log_debug("loading user lua_file: %s", utstring_body(user_lua_file));
        if (luaL_loadfile(L, utstring_body(user_lua_file)) || lua_pcall(L, 0, 0, 0))
            lerror(L, "cannot run configuration file: %s\n",
                   lua_tostring(L, -1));
        log_debug("loaded lua file: %s", utstring_body(user_lua_file));
    } else {
        log_debug("no user lua file found: %s", utstring_body(user_lua_file));
        utstring_clear(user_lua_file);
    }
}
