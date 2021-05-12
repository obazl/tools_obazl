#include <dirent.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "lua.h"
#include "lauxlib.h"

#include "log.h"

#include "obazl_dir.h"

/* **************** */
static int dirent_index (lua_State *L) {

    /* log_debug("dirent_index"); */

    struct dirent *d = *(struct dirent **)luaL_checkudata(L, 1, "OBazl.mt.dirent");

    /* struct dirent *d = *(struct dirent **)lua_touserdata(L, 1); */
    if (d == NULL) {
        log_error("NULL dirent userdata");
    }
    /* log_debug("dirent->d_name: %s", d->d_name); */
    // arg 1: table; arg 2: method name
    /* char *tbl = lua_tostring(L, -1); */

    const char *method = lua_tostring(L, 2);
    /* log_debug("__index arg 2: %s", method); */

    if (strncmp("name", method, 4) == 0) {
        lua_pushstring(L, d->d_name);
        return 1;
    }
    if (strncmp("type", method, 5) == 0) {
        switch(d->d_type) {
        case DT_DIR:
            lua_pushstring(L, "directory");
            return 1;
            break;
        case DT_REG:
            lua_pushstring(L, "regular");
            return 1;
            break;
        default:
            lua_pushstring(L, "other");
            return 1;
        }
    }
    return 1;
}

/* **************** */
static int dir_iter (lua_State *L) {
    /* log_trace("dir_iter"); */
    DIR *d = *(DIR **)lua_touserdata(L, lua_upvalueindex(1));
    errno = 0;
    struct dirent *entry = readdir(d);
    if (entry != NULL) {

        /* NOTE: readdir is not reentrant; will we run into trouble if
           user keeps a dirent around across multiple calls to
           readdir? FIX: push a copy of dirent as userdata? malloc a
           copy and push its pointer? */

        struct dirent **de = (struct dirent **)lua_newuserdata(L, sizeof(struct dirent *));
        *de = entry;

        /* set its metatable */
        luaL_getmetatable(L, "OBazl.mt.dirent");
        lua_setmetatable(L, -2);

        /* creates and returns the iterator function
           (its sole upvalue, the directory userdatum,
           is already on the stack top */
        /* lua_pushcclosure(L, dir_iter, 1); */

        /* return new table */
        return 1;
    } else {
        if (errno == 0)
            return 0;  /* no more values to return */
        else
            log_error("readdir error: %d", errno); /* FIXME */
    }
    return 0;
}

/* **************** */
static int dir_gc (lua_State *L) {
    log_debug("dir_gc");
    DIR *d = *(DIR **)lua_touserdata(L, 1);
    if (d) closedir(d);
    return 0;
}

/* **************** */
/* static int l_dirent (lua_State *L) { */
/*     const char *path = luaL_checkstring(L, 1); */

/*     /\* create a userdatum to store a DIR address *\/ */
/*     struct dirent **de = (DIR **)lua_newuserdata(L, sizeof(struct dirent *)); */

/*     /\* pre-initialize *\/ */
/*     *d = NULL; */

/*     /\* set its metatable *\/ */
/*     luaL_getmetatable(L, "OBazl.dirent"); */
/*     lua_setmetatable(L, -2); */

/*     /\* try to open the given directory *\/ */
/*     /\* *d = opendir(path); *\/ */
/*     /\* if (*d == NULL)  /\\* error opening the directory? *\\/ *\/ */
/*     /\*     luaL_error(L, "cannot open %s: %s", path, strerror(errno)); *\/ */

/*     /\* creates and returns the iterator function */
/*        (its sole upvalue, the directory userdatum, */
/*        is already on the stack top *\/ */
/*     lua_pushcclosure(L, dir_iter, 1); */
/*     return 1; */
/* } */

/* **************** */
static int l_opendir (lua_State *L) {
    const char *path = luaL_checkstring(L, 1);

    /* create a userdatum to store a DIR address */
    DIR **d = (DIR **)lua_newuserdata(L, sizeof(DIR *));

    /* pre-initialize */
    *d = NULL;

    /* set its metatable */
    luaL_getmetatable(L, "OBazl.mt.dir");
    lua_setmetatable(L, -2);

    /* try to open the given directory */
    *d = opendir(path);
    if (*d == NULL)  /* error opening the directory? */
        luaL_error(L, "cannot open %s: %s", path, strerror(errno));

    /* creates and returns the iterator function
       (its sole upvalue, the directory userdatum,
       is already on the stack top */
    lua_pushcclosure(L, dir_iter, 1);
    return 1;
}

static const struct luaL_Reg dirlib [] = {
    {"open", l_opendir},
    {NULL, NULL}
};

int luaopen_obazl_dir (lua_State *L) {

    log_set_level(LOG_TRACE);
    log_set_quiet(false);

    /* log_info("luaopen_obazl_dir"); */

    // metatable for direntry
    luaL_newmetatable(L, "OBazl.mt.dirent");
    /* set its __index field, will handle 'name' and 'type' lookups */
    lua_pushcfunction(L, dirent_index);
    lua_setfield(L, -2, "__index");

    // metatable for dir
    luaL_newmetatable(L, "OBazl.mt.dir");
    /* set its __gc field */
    lua_pushcfunction(L, dir_gc);
    lua_setfield(L, -2, "__gc");

    /* create the library */
    luaL_newlib(L, dirlib);
    return 1;
}
