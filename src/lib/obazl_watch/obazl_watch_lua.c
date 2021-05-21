#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include <libfswatch/c/libfswatch.h>

#include "utarray.h"
#include "utstring.h"
#include "log.h"

#include "obazl_watch_lua.h"

/**
   called by my_callback
 */
void l_on_change (fsw_cevent *event)
{
    log_debug("l_on_change, state: %p", L);

    log_debug("event path: %s", event->path);

    return;
}

void foo_on_change()
{
    /* for iterating dirty_feps array */
    char **fdep = NULL;
    char *result;

    /* 1. update packages table */
    lua_getglobal(L, "bazel");  /* bazel table */
    lua_getfield(L, -1, "packages"); /* tos: bazel.packages (table) */
    while ( (fdep=(char**)utarray_next(dirty_fdeps, fdep))) {
        log_debug("pushing dirty %s to lua",*fdep);

        char *pkg_name = fname_to_pkgname(*fdep);
        char *pkg_path = fname_to_pkgpath(*fdep);
        char *module_name = fname_to_mname(*fdep);

        /* stack-effect notation:  (stack_before_op -- stack_after_op); '$' on rhs: stack_before_op */
        /* implicit: initial state of stack not affected */
        /* lua_pushstring: ( -- s) op takes no args, leaves s on stack */
        /* use '$' to indicate stack state before op */
        /* so ( -- s) == ($ -- $ s); meaning no change to initial state, just s added */
        /* we can expand '$' to make state explicit for bookkeeping */
        /* alternative: [initial stack state](input_args -- outputs) */

        lua_pushstring(L, pkg_name); /* (tbzl tpkgs -- $ pkg_name) */
        lua_newtable(L);             /* (tbzl tpkgs pkg_name -- $ tpkg) */

        lua_pushstring(L, "name");   /* (tbzl tpkgs pkg_name tpkg -- $ "name") */
        lua_pushstring(L, pkg_name); /* (tbzl tpkgs pkg_name tpkg "name" -- $ pkg_name) */
        lua_settable(L, -3); /* (tbl key val -- tbl) */
        /* expanded:
           (tbzl tpkgs pkg_nm pgk_tbl "name" pkg_name -- tbzl tpkgs pkg_name tpkg)
        */

        lua_pushstring(L, "path");   /* [tbzl tpkgs pkg_name tpkg]( -- "path") */
        lua_pushstring(L, pkg_path); /* [tbzl tpkgs pkg_name tpkg "path"]( -- pkg_path) */
        lua_settable(L, -3);         /* [tbzl tpkgs pkg_name](tpkg "path" pkg_path -- tpkg) */

        /* modules subtable */
        lua_pushstring(L, "modules"); /* [tbzl tpkgs pkg_name tpkg]( -- "modules") */
        lua_newtable(L);              /* [tbzl tpkgs pkg_name tpkg "modules"]( -- tbl1) */

        /* module entry */
        lua_pushstring(L, module_name); /* [tbzl tpkgs pkg_name tpkg "modules" tbl1]( -- module_name) */
        lua_newtable(L);                /* [tbzl tpkgs pkg_name tpkg "modules" tbl1 module_name]( -- tbl2) */

        lua_pushstring(L, "structfile"); /* [tbzl tpkgs pkg_name tpkg "modules" tbl1 module_name tbl2]
                                            ( -- "structfile") */
        lua_pushstring(L, *fdep); /* [tbzl tpkgs pkg_name tpkg "modules" tbl1 module_name tbl2 "structfile"]
                                     ( -- *fdep) */
        lua_settable(L, -3);      /* [tbzl tpkgs pkg_name tpkg "modules" tbl1 module_name]
                                     (tbl2 "structfile" *fdep -- tbl2) */

        lua_settable(L, -3);      /* [tbzl tpkgs pkg_name tpkg "modules"](tbl1 module_name tbl2 -- tbl1) */

        lua_settable(L, -3);      /* [tbzl tpkgs pkg_name](tpkg "modules" tbl1 -- tpkg) */

        /* fdep subtable completed */
        lua_settable(L, -3);      /* [tbzl](tpkgs pkg_name tpkg -- tpkgs) */
        /* result: [tbzl tpkgs] */

        // FIXME: pop the stack? we've updated in place, and it's in a global var, so we clear the stack?
    }

    /* 2. call user-provided lua fn  */
    /* push functions and arguments */
    /* lua_getglobal(L, "emit_build_lua");  /\* function to be called *\/ */

    /* fdep = NULL; */
    /* while ( (fdep=(char**)utarray_next(dirty_fdeps, fdep))) { */
    /*     log_debug("dirty: %s", *fdep); */

    /*     /\* result = (char*)emit_build_lua(*p); *\/ */
    /*     lua_pushstring(L, *fdep);   /\* push 1st argument *\/ */

    /*     /\* do the call (1 arguments, 1 result) *\/ */
    /*     if (lua_pcall(L, 1, 1, 0) != 0) { */
    /*         log_error("error running lua fn"); */
    /*         lerror(L, "error running function `f': %s", lua_tostring(L, -1)); */
    /*     } */

    /*     /\* retrieve result *\/ */
    /*     if (!lua_isstring(L, -1)) */
    /*         lerror(L, "function `emit_build_lua' must return a string"); */
    /*     const char *msg = lua_tostring(L, -1); */
    /*     lua_pop(L, 1);  /\* pop returned value *\/ */

    /*     log_debug("emit_build_lua returned: %s", msg); */
    /* } */
    return;
}

/**
   adds 'packages' entry to global 'bazel' table
   LStack on entry: [tblz]
   stack effect: (tblz -- tbzl) // updates tbzl
 */
int l_install_packages(void)
{
    log_debug("l_install_packages entry");

    lua_pushstring(L, "packages"); /* [tbzl]( -- "packages") */
    lua_newtable(L);               /* [tbzl "packages"]( -- tpkgs) */
    /* set packages tbl, so we can search it. (is this necessary?) */
    lua_settable(L, -3);           /* (tbzl "packages" tpkgs -- tbzl) */

    struct module_s *module, *tmp;
    int ftype;                  /* lua field type */

    char *pkg_name;

    HASH_ITER(hh, codept_modules, module, tmp) {
        /* log_debug(""); */
        /* log_debug("module->name: %s", module->name); */
        /* log_debug("module->type: %d (%s)", */
        /*           module->type, */
        /*           (module->type == 0)? "LOCAL" */
        /*           :(module->type == 1)? "EXTERNAL" */
        /*           :(module->type == 2)? "UKNOWN" */
        /*           : "UNRECOGNIZED MODULE TYPE"); */
        if (module->type == 0) { // M_LOCAL) { friggin makeheaders doesn't get M_LOCAL
            /* log_debug("registering LOCAL module %s", module->name); */
            /* log_debug("module->structfile: %s", module->structfile); */
            /* log_debug("module->sigfile: %s", module->sigfile); */

            if (module->structfile) {
                pkg_name = fname_to_pkgname(module->structfile);
            } else {
                pkg_name = fname_to_pkgname(module->sigfile);
            }
            log_debug("PKG NAME: %s", pkg_name);

            /* stack: [bzl tpkgs] */
            /* 1. is pkg already in tpkgs? */
            log_debug("stacktop %d", lua_gettop(L));
            ftype = lua_getfield(L, -1, pkg_name);
            if (ftype == LUA_TNIL) {
                log_debug("pkg not found in table; creating");
                /* getfield left a nil val on the stack; pop it */
                lua_pop(L, 1);
                lua_pushstring(L, pkg_name);
                lua_newtable(L);
                lua_settable(L, -3);
                log_debug("stacktop %d", lua_gettop(L));

                /* tpkgs now contains entry for pkg, so try again, to confirm and open it */
                ftype = lua_getfield(L, -1, pkg_name);
                if (ftype == LUA_TNIL) {
                    log_error("not found after creating");
                } else {
                    log_info("created");
                    /* lua_pop(L,1); */
                    /* stack: [tbzl tpkgs modpkg */
                }
            } else {
                log_debug("found in table");
                /* lua_pop(L,1); */
                /* type should be LUA_TTABLE */
                /* stack: [bzl tpkgs modpkg] */
            }
            log_debug("after stacktop %d", lua_gettop(L));

            log_debug("tos istable? %d", lua_istable(L, -1));

            /* ok we got the pkg. now search for the module in the pkg's list of modules . */
            /* stack: [tbzl tpkgs tpkg] */
            ftype = lua_getfield(L, -1, "modules");
            if (ftype == LUA_TNIL) {
                log_debug("fld 'modules' not found in pkg table; creating");
                lua_pop(L, 1); /* getfield left a nil val on the stack; pop it */
                lua_pushstring(L, "modules");
                lua_newtable(L);
                lua_settable(L, -3);
                log_debug("stacktop %d", lua_gettop(L));

                /* tpkg now contains 'modules' entry, so try again, to confirm and open it */
                ftype = lua_getfield(L, -1, "modules");
                if (ftype == LUA_TNIL) {
                    log_error("not found after creating");
                } else {
                    log_info("created fld 'modules'");
                    lua_pop(L,1);
                    /* stack: [tbzl tpkgs modpkg */
                }
            } else {
                log_debug("found fld 'modules' in table for pkg %s", pkg_name);
                lua_pop(L,1);
                /* type should be LUA_TTABLE */
                /* stack: [bzl tpkgs modpkg] */
            }

            log_debug("nowthen, do module '%s'", module->name);
            log_debug("module->structfile: %s", module->structfile);
            log_debug("module->sigfile: %s", module->sigfile);

            /* ftype = lua_getfield(L, -1, module->name); */
            /* if (ftype == LUA_TNIL) { */
            /*     log_debug("module %s not found in pkg table; creating", module->name); */
            /*     lua_pop(L, 1); /\* getfield left a nil val on the stack; pop it *\/ */
            /*     lua_pushstring(L, module->name); */
            /*     lua_newtable(L); */
            /*     lua_settable(L, -3); */
            /*     log_debug("stacktop %d", lua_gettop(L)); */

            /*     /\* tpkgs now contains entry for pkg, so try again, to confirm and open it *\/ */
            /*     ftype = lua_getfield(L, -1, pkg_name); */
            /*     if (ftype == LUA_TNIL) { */
            /*         log_error("not found after creating"); */
            /*     } else { */
            /*         log_info("created"); */
            /*         /\* lua_pop(L,1); *\/ */
            /*         /\* stack: [tbzl tpkgs modpkg *\/ */
            /*     } */
            /* } else { */
            /*     log_debug("found in table"); */
            /*     /\* lua_pop(L,1); *\/ */
            /*     /\* type should be LUA_TTABLE *\/ */
            /*     /\* stack: [bzl tpkgs modpkg] *\/ */
            /* } */


            /* finally, update ml, mli, deps */

            /* for dev, pop */
            lua_pop(L,1);

            /* we've got the module table, now update the 'ml' and 'mli' fiels */
            /* lua_pushstring(L, module->name); /\* [tbzl "modules" tmods]( -- snm) *\/ */
            /* lua_newtable(L);             /\* [tbzl "modules" tmods snm]( -- tpkg) *\/ */

            /* if (module->structfile) { */
            /*     lua_pushstring(L, "ml");     /\* [tbzl "modules" tmods snm tpkg]( -- "ml") *\/ */
            /*     lua_pushstring(L, module->structfile); /\* [tbzl "modules" tmods snm tpkg "ml"]( -- mlfname) *\/ */
            /*     lua_settable(L, -3); /\* [tbzl "modules" tmods snm](tpkg "ml" mlfname -- tpkg) *\/ */
            /* } */
            /* if (module->sigfile) { */
            /*     lua_pushstring(L, "mli");     /\* [tbzl "modules" tmods snm tpkg]( -- "mli") *\/ */
            /*     lua_pushstring(L, module->sigfile); /\* [tbzl "modules" tmods snm tpkg "mli"]( -- mlifname) *\/ */
            /*     lua_settable(L, -3); /\* [tbzl "modules" tmods snm](tpkg "mli" mlifname -- tpkg) *\/ */
            /* } */
            /* lua_settable(L, -3); /\* [tbzl "modules"](tmods snm tpkg -- tmods) *\/ */
        } else {
            if (module->type == 1) { // M_EXTERNAL) {
                /* log_debug("(skipping) module->lib: %s", module->lib); */
            /* } else { */
            /*     if (module->type == 2) { // M_UNKNOWN) { */
            /*         log_debug("module UKNOWN"); */
            /*     } else { */
            /*         log_fatal("Unrecognized module type"); */
            /*     } */
            }
        }
    }
    /*         log_debug("3 xxxxxxxxxxxxxxxx"); */
    /* lua_settable(L, -3); /\* (tbzl "modules" tmods -- tbzl) *\/ */
    log_debug("");
}

/**
   adds 'modules' entry to global 'bazel' table
   LStack on entry: [tblz]
   stack effect: (tblz -- tbzl) // updates tbzl
 */
int l_install_modules(void)
{
    log_debug("l_install_modules entry");

    lua_pushstring(L, "modules"); /* [tbzl]( -- "modules") */
    lua_newtable(L);              /* [tbzl "modules"]( -- tmods) */

    struct module_s *module, *tmp;

    HASH_ITER(hh, codept_modules, module, tmp) {
        log_debug("");
        log_debug("module->name: %s", module->name);
        log_debug("module->type: %d (%s)",
                  module->type,
                  (module->type == 0)? "LOCAL"
                  :(module->type == 1)? "EXTERNAL"
                  :(module->type == 2)? "UKNOWN"
                  : "UNRECOGNIZED MODULE TYPE");
        if (module->type == 0) { // M_LOCAL) { friggin makeheaders doesn't get M_LOCAL
            log_debug("module->structfile: %s", module->structfile);
            log_debug("module->sigfile: %s", module->sigfile);

            /* tos: modules table */
            lua_pushstring(L, module->name); /* [tbzl "modules" tmods]( -- snm) */
            lua_newtable(L);             /* [tbzl "modules" tmods snm]( -- tpkg) */

            if (module->structfile) {
                lua_pushstring(L, "ml");     /* [tbzl "modules" tmods snm tpkg]( -- "ml") */
                lua_pushstring(L, module->structfile); /* [tbzl "modules" tmods snm tpkg "ml"]( -- mlfname) */
                lua_settable(L, -3); /* [tbzl "modules" tmods snm](tpkg "ml" mlfname -- tpkg) */
            }
            if (module->sigfile) {
                lua_pushstring(L, "mli");     /* [tbzl "modules" tmods snm tpkg]( -- "mli") */
                lua_pushstring(L, module->sigfile); /* [tbzl "modules" tmods snm tpkg "mli"]( -- mlifname) */
                lua_settable(L, -3); /* [tbzl "modules" tmods snm](tpkg "mli" mlifname -- tpkg) */
            }
            lua_settable(L, -3); /* [tbzl "modules"](tmods snm tpkg -- tmods) */
        } else {
            if (module->type == 1) { // M_EXTERNAL) {
                log_debug("(skipping) module->lib: %s", module->lib);
            /* } else { */
            /*     if (module->type == 2) { // M_UNKNOWN) { */
            /*         log_debug("module UKNOWN"); */
            /*     } else { */
            /*         log_fatal("Unrecognized module type"); */
            /*     } */
            }
        }
    }
    lua_settable(L, -3); /* (tbzl "modules" tmods -- tbzl) */
    log_debug("");
}

/**
   to be run after codept init
 */
void l_init (void)
{
    log_debug("l_init, state: %p", L);

    int rc;

    /* for iterating dirty_feps array */
    char **fdep = NULL;
    char *result;

    /* get global 'bazel' table created by config_lua */
    /* lua_getglobal(L, "bazel");  /\* bazel table *\/ */

    /* l_install_packages(); */

    /* l_install_modules(); */


    /* 2. call user-provided lua fn  */
    /* push functions and arguments */
    lua_getglobal(L, "init");  /* function to be called */

    /* do the call (1 arguments, 1 result) */
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
   create lua state, global pkgs var, load user lua file with onchange callback
*/
void config_lua(void)
{
    log_debug("config_lua");
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

    UT_string *lua_path;
    utstring_new(lua_path);

    char *_proj_root = getenv("BUILD_WORKSPACE_DIRECTORY");
    utstring_printf(lua_path, "%s/.obazl.d/?.lua", _proj_root);

    size_t sz;
    utstring_printf(lua_path, ";%s/%s/?.lua", getcwd(NULL, sz), "external/obazl/src/lua");
    log_debug("lua_path: %s", utstring_body(lua_path));

    lua_getglobal(L, "package");
    lua_pushstring(L, utstring_body(lua_path));
    lua_setfield(L, -2, "path");
    lua_pop(L, 1);

    // FIXME: allow user to override default obazl.lua with `.obazl.d/obazl.lua`
    /* UT_string *override; */
    /* utstring_new(override); */
    /* utstring_printf(override, "%s/obazl.lua", utstring_body(obazl_d)); */

    /* char *lua_file; */
    log_debug("checkout for lua_file override: %s", utstring_body(lua_file));
    int rc = access(utstring_body(lua_file), R_OK);
    if (rc) {
        utstring_clear(lua_file);
        utstring_printf(lua_file, "external/obazl/src/lua/obazl.lua");
    }

    log_debug("loading lua file %s", utstring_body(lua_file));
    if (luaL_loadfile(L, utstring_body(lua_file)) || lua_pcall(L, 0, 0, 0))
            lerror(L, "cannot run configuration file: %s\n",
                   lua_tostring(L, -1));

    log_debug("loaded lua file: %s", utstring_body(lua_file));

    utstring_free(lua_path);
    utstring_free(lua_file);
}
