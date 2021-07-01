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

/* void error (lua_State *L, const char *fmt, ...) */
/* { */
/*     va_list argp; */
/*     va_start(argp, fmt); */
/*     vfprintf(stderr, argp); */
/*     va_end(argp); */
/*     lua_close(L); */
/*     exit(EXIT_FAILURE); */
/* } */

/* **************************************************************** */
static int dune_pkg_gc (lua_State *L) {
    log_debug("dune_pkg_gc");
    /* DIR *d = *(DIR **)lua_touserdata(L, 1); */
    /* if (d) closedir(d); */
    return 0;
}

static int dune_pkg_index (lua_State *L) {
    /* metatable: OBazl.dune.pkg */
    log_debug("dune_pkg_index");

    struct obazl_dune_package_s *pkg = *(struct obazl_dune_package_s **)lua_touserdata(L, lua_upvalueindex(1));
    const char *key = luaL_checkstring(L, 2);
    log_debug("index key: %s", key);
    if (strncmp(key, "path", 4) == 0) {
        lua_pushstring(L, pkg->path);
    } else {
        if (strncmp(key, "stanzas", 7) == 0) {
            log_debug("returning stanzas list");
            lua_remove(L, -1);
            UT_array **sud = lua_newuserdata(L, sizeof(UT_array*)); /* array of struct stanza_s */
            *sud = pkg->stanzas;
            luaL_getmetatable(L, "OBazl.dune.stanzas");
            lua_setmetatable(L, -2);
            return 1;
        } else {
            log_warn("invalid key: %s", key);
            return 0;
        }
    }
    return 1;
}

static int dune_pkg_enumerator (lua_State *L)
{
    log_debug("dune_pkg_enumerator");

    /* first the 1 upvalue */
    /* int stanza_count = lua_tointeger(L, lua_upvalueindex(1)); */
    /* /\* log_debug("stanza_count: %d", stanza_count); *\/ */

    int upcounter = lua_tointeger(L, lua_upvalueindex(2));
    /* log_debug("upcounter: %d", upcounter); */

    /* now the args */
    /* arg 1: the invariant */
    struct obazl_dune_package_s *pkg = *(struct obazl_dune_package_s **)lua_touserdata(L, 1);
    /* log_debug("invariant (pkg): %p", pkg); */

    /* arg 2: value of control var */
    /* we don't need this? */
    /* int arg = lua_tointeger(L, 2); */
    /* log_debug("ctl var value:: %d", arg); */

    char *result;
    switch(upcounter) {
    case 0:
        result = pkg->path;
        break;
    case 1:
        //FIXME:
        result = "stanzas";
        break;
    case 2:
        //FIXME:
        result = "files";
        break;
    default:
        return 0;
    }

    /* prep for the next iteration */
    lua_pushinteger(L, ++upcounter);
    lua_copy(L, -1, lua_upvalueindex(2));

    /* now push the k,v results */
    lua_pushinteger(L, upcounter);

    /* struct stanza_s **next_item = lua_newuserdata(L, sizeof(struct stanza_s*)); */
    /* next_item = curritem; */
    /* lua_pushlightuserdata(L, next_item); /\* upval 3: ptr to next item *\/ */
    /* lua_copy(L, -1, lua_upvalueindex(3)); */
    /* lua_remove(L, -1);          /\* remove the lightuserdata, leaving userdata *\/ */

    /* luaL_getmetatable(L, "OBazl.dune.stanza"); */
    /* lua_setmetatable(L, -2); */
    lua_pushstring(L, result);
    return 2;
}

static int dune_pkg_pairs (lua_State *L) {
    log_debug("dune_pkg_pairs");

    struct obazl_dune_package_s *pkg = *(struct obazl_dune_package_s **)lua_touserdata(L, 1);

    /*  result 1: "generator" closure with upvalues */
    /* char **curritem = NULL;           /\* we use this to keep track of where we are in list *\/ */
    /* int len = 3; */
    lua_pushinteger(L, 0);      /* upval 2: counter */
    lua_pushcclosure(L, dune_pkg_enumerator, 2);

    /* result 2: invariant (stanza list) */
    obazl_dune_package_s **new_pkg = lua_newuserdata(L, sizeof(struct obazl_dune_package_s*));
    *new_pkg = pkg;
    luaL_getmetatable(L, "OBazl.dune.pkg");
    lua_setmetatable(L, -2);

    /* result 3: initial val for control var */
    lua_pushinteger(L, 0);

    return 3;
}

/* **************************************************************** */
static int dune_stanzas_gc (lua_State *L) {
    log_debug("dune_stanzas_gc");
    /* DIR *d = *(DIR **)lua_touserdata(L, 1); */
    /* if (d) closedir(d); */
    return 0;
}

static int dune_stanzas_index (lua_State *L) {
    /* metatable: OBazl.dune.stanza */
    log_debug("dune_stanzas_index");
    UT_array *stanzas = *(UT_array **)lua_touserdata(L, 1); /* array of struct stanza_s* */

    if (lua_isinteger(L, 2)) {
        int i = lua_tointeger(L, 2);
        log_debug("indexing: %d", i);
        int len = utarray_len(stanzas);

        if (i > 0 && i <= len) {
            /* create new user datum */
            struct stanza_s **new_stanza = lua_newuserdata(L, sizeof(struct stanza_s*));
            struct stanza_s **the_stanza = utarray_eltptr(stanzas, i-1);
            *new_stanza = *the_stanza;
            luaL_getmetatable(L, "OBazl.dune.stanza");
            lua_setmetatable(L, -2);
            return 1;
        } else {
            return 0;
        }
    } else {
        const char *s = lua_tostring(L, 2);
        log_debug("indexing: %s", s);
        return 0;
    }
}

static int dune_stanzas_len (lua_State *L) {
    log_debug("dune_stanzas_len");
    UT_array *pkg = *(UT_array **)lua_touserdata(L, 1);
    int len = utarray_len((UT_array*)pkg);
    log_debug("dune_stanzas_len: %d", len);
    lua_pushinteger(L, len);
    return 1;
}

static int dune_stanzas_enumerator (lua_State *L)
{
    log_debug("dune_stanzas_enumerator");

    /* first the 3 upvalues */
    int stanza_count = lua_tointeger(L, lua_upvalueindex(1));
    log_debug("stanza_count: %d", stanza_count);

    int upcounter = lua_tointeger(L, lua_upvalueindex(2));
    log_debug("upcounter: %d", upcounter);

    struct stanza_s **curritem = (struct stanza_s**)lua_touserdata(L, lua_upvalueindex(3));
    /* log_debug("curritem ptr: %p", curritem); */

    /* now the args */
    /* arg 1: the invariant (list of struct stanza_s) */
    UT_array *stanzas = *(UT_array **)lua_touserdata(L, 1);
    /* log_debug("invariant (stanzas): %p", stanzas); */

    /* arg 2: value of control var */
    /* int arg = lua_tointeger(L, 2); */
    /* log_debug("ctl var value:: %d", arg); */

    curritem=(struct stanza_s**)utarray_next(stanzas, curritem);
    if (curritem == NULL) return 0;
    if (upcounter >= stanza_count) return 0;

    /* prep for the next iteration */
    lua_pushinteger(L, ++upcounter);
    lua_copy(L, -1, lua_upvalueindex(2));

    /* now push the k,v results */
    lua_pushinteger(L, upcounter);

    struct stanza_s **next_item = lua_newuserdata(L, sizeof(struct stanza_s*));
    next_item = curritem;
    lua_pushlightuserdata(L, next_item); /* upval 3: ptr to next item */
    lua_copy(L, -1, lua_upvalueindex(3));
    lua_remove(L, -1);          /* remove the lightuserdata, leaving userdata */

    luaL_getmetatable(L, "OBazl.dune.stanza");
    lua_setmetatable(L, -2);
    return 2;
}

static int dune_stanzas_pairs (lua_State *L)
{
    log_debug("dune_stanzas_pairs");
    UT_array *stanzas = *(UT_array **)lua_touserdata(L, 1);

    /*  result 1: "generator" closure with upvalues */
    char **curritem = NULL;           /* we use this to keep track of where we are in list */
    int len = utarray_len(stanzas);
    log_debug("stanzas ct: %d", len);
    lua_pushinteger(L, len);    /* upval 1: stanza ct */
    lua_pushinteger(L, 0);      /* upval 2: counter */
    lua_pushlightuserdata(L, curritem); /* upval 3: ptr to initial item in list */
    lua_pushcclosure(L, dune_stanzas_enumerator, 3);

    /* result 2: invariant (stanza list) */
    UT_array **new_stanzas = lua_newuserdata(L, sizeof(UT_array*));
    *new_stanzas = stanzas;
    luaL_getmetatable(L, "OBazl.dune.stanzas");
    lua_setmetatable(L, -2);

    /* result 3: initial val for control var */
    lua_pushinteger(L, 0);

    return 3;
}

/* **************************************************************** */
static int dune_stanza_gc (lua_State *L) {
    log_debug("dune_stanza_gc");
    /* DIR *d = *(DIR **)lua_touserdata(L, 1); */
    /* if (d) closedir(d); */
    return 0;
}

static int dune_stanza_index (lua_State *L) {
    /* metatable: OBazl.dune.stanza */
    log_debug("dune_stanza_index");
    struct stanza_s *stanza = *(struct stanza_s**)lua_touserdata(L, 1);

    const char *key = luaL_checkstring(L, 2);
    log_debug("indexing stanza at: %s", key);
    if (strncmp(key, "type", 4) == 0) {
        const char *name = stanza_name[stanza->type];
        lua_pushstring(L, name);
    } else {
        if (strncmp(key, "fields", 6) == 0) {
            struct stanza_field_s **fields = lua_newuserdata(L, sizeof(struct stanza_field_s*));
            *fields = stanza->fields;
            luaL_getmetatable(L, "OBazl.dune.fields");
            lua_setmetatable(L, -2);
            log_debug("returning fields: %p", *fields);
        } else {
            return 0;
        }
    }
    return 1;
}

static int dune_stanza_len (lua_State *L) {
    log_debug("dune_stanza_len");
    struct stanza_s *stanza = *(struct stanza_s **)lua_touserdata(L, 1);
    int len = HASH_CNT(hh, stanza->fields);
    lua_pushinteger(L, len);
    return 1;
}

static int dune_stanza_enumerator (lua_State *L) {
    log_debug("dune_stanza_enumerator");

    /* upvalue 1: current index */
    int upcounter = lua_tointeger(L, lua_upvalueindex(1));
    log_debug("upcounter: %d", upcounter);

    /* arg 1: the invariant (one struct stanza_s) */
    struct stanza_s *stanza = *(struct stanza_s **)lua_touserdata(L, 1);

    if (upcounter > 1) return 0;

    /* result 1: k */
    if (upcounter == 0) {
        lua_pushstring(L, "type");
    } else {
        if (upcounter == 1) {
            lua_pushstring(L, "fields");
        }
    }

    /* result 2: v */
    if (upcounter == 0) {
        const char *type = stanza_name[stanza->type];
        lua_pushstring(L, type);
    } else {
        if (upcounter == 1) {
            struct stanza_field_s **sud = lua_newuserdata(L, sizeof(struct stanza_field_s*));
            *sud = stanza->fields;
            luaL_getmetatable(L, "OBazl.dune.fields");
            lua_setmetatable(L, -2);
        }
    }
    /* prep for the next iteration */
    lua_pushinteger(L, ++upcounter);
    lua_copy(L, -1, lua_upvalueindex(1));
    lua_remove(L, -1);

    return 2;
}

static int dune_stanza_pairs (lua_State *L) {
    log_debug("dune_stanza_pairs");
    struct stanza_s *stanza = *(struct stanza_s **)lua_touserdata(L, 1);

    /* return 1: iterator */
    lua_pushinteger(L, 0);      /* upval 2: counter */
    lua_pushcclosure(L, dune_stanza_enumerator, 1);

    /* invariant state (struct stanza_s) */
    struct stanza_s **the_stanza = lua_newuserdata(L, sizeof(struct stanza_s*));
    *the_stanza = stanza;
    luaL_getmetatable(L, "OBazl.dune.stanza");
    lua_setmetatable(L, -2);

    /* initial val for control var */
    lua_pushinteger(L, 0);

    return 3;
}

/* **************************************************************** */
static int dune_fields_gc (lua_State *L) {
    log_debug("dune_fields_gc");
    /* DIR *d = *(DIR **)lua_touserdata(L, 1); */
    /* if (d) closedir(d); */
    return 0;
}

static int dune_fields_index (lua_State *L) {
    log_debug("dune_fields_index");
    /* metatable: OBazl.dune.fields */
    struct stanza_field_s *fields = *(struct stanza_field_s **)lua_touserdata(L, 1);

    if (lua_isinteger(L, 2)) {
        int i = lua_tointeger(L, 2);
        log_debug("indexing flds at: %d", i);
        int len = HASH_CNT(hh, fields);
        log_debug("flds cnt: %d", len);

        i--;                    /* convert index from lua to c */

        if (i >= 0 && i < len) {
            /* iterate hashmap */
            struct stanza_field_s *search_fld;
            int j = 0;
            for (search_fld = fields; search_fld != NULL; search_fld = search_fld->hh.next) {
                if (j == i) {
                    break;
                } else {
                    j++;
                }
            }
            if (search_fld == NULL) {
                return 0;
            } else {
                /* create new user datum */
                /* struct stanza_field_s **new_fld = lua_newuserdata(L, sizeof(struct stanza_field_s*)); */
                /* *new_fld = search_fld; */

                lua_createtable(L, 0, 1);
                char *fldname = field_name[search_fld->type];
                lua_pushstring(L, fldname); /* key */
                switch(search_fld->type) {
                case FIELD_NAME:
                    lua_pushstring(L, search_fld->name);
                    /* return 1; */
                    break;
                case FIELD_PUBLIC_NAME:
                    lua_pushstring(L, search_fld->name);
                    /* return 1; */
                    break;
                case FIELD_LIBRARIES: {
                    /* data is UT_array ptr, but userdata is ptr ptr */
                    //FIXME: create a table
                    UT_array **libs = lua_newuserdata(L, sizeof(UT_array*));
                    *libs = search_fld->libraries; /* array of strings */
                    luaL_getmetatable(L, "OBazl.dune.wordlist");
                    lua_setmetatable(L, -2);
                    /* return 1; */
                    break;
                }
                case FIELD_MODULES: {
                    //FIXME: create a table
                    struct modules_s **modules = lua_newuserdata(L, sizeof(struct modules_s*));
                    *modules = search_fld->modules;
                    luaL_getmetatable(L, "OBazl.dune.modules");
                    lua_setmetatable(L, -2);
                    /* return 1; */
                    break;
                }
                default:
                    /* log_warn("Unexpected key: %d", key); */
                    return 0;
                }
                lua_settable(L, -3);

                /* luaL_getmetatable(L, "OBazl.dune.field"); */
                /* lua_setmetatable(L, -2); */
                return 1;
            }
        } else {
            return 0;
        }
    } else {
        const char *s = lua_tostring(L, 2);
        log_debug("indexing fields at: %s", s);
        lua_pushstring(L, "testfld");
        return 1;
    }
}

static int dune_fields_len (lua_State *L) {
    log_debug("dune_fields_len");
    struct stanza_field_s *fields = *(struct stanza_field_s **)lua_touserdata(L, 1);
    int len = HASH_CNT(hh, fields);
    lua_pushinteger(L, len);
    return 1;
}

static int dune_fields_enumerator (lua_State *L) {
    log_debug("dune_fields_enumerator");

    int uplen = lua_tointeger(L, lua_upvalueindex(1));
    log_debug("uplen: %d", uplen);

    int upcounter = lua_tointeger(L, lua_upvalueindex(2));
    log_debug("flds upcounter: %d", upcounter);

    /* ptr (lightuserdata) to current fld */
    struct stanza_field_s *current_fld = (struct stanza_field_s*)lua_touserdata(L, lua_upvalueindex(3));
    log_debug("upfield: %p", current_fld);

    /* ignore args? */
    /* arg 1: invariant (fields list) */
    struct stanza_field_s *field = (struct stanza_field_s*)lua_touserdata(L, 1);
    log_debug("arg1: %p", field);
    int arg = lua_tointeger(L, 2);
    log_debug("ctl var value:: %d", arg);

    if (upcounter > uplen) return 0;
    if (current_fld == NULL) return 0;

    /* prep for the next iteration */
    lua_pushinteger(L, ++upcounter);
    lua_copy(L, -1, lua_upvalueindex(2));
    lua_remove(L, -1);
    struct stanza_field_s *next_field = current_fld->hh.next;
    lua_pushlightuserdata(L, next_field);
    lua_copy(L, -1, lua_upvalueindex(3));
    lua_remove(L, -1);

    /* now push the k, v, rawstring results */
    /* result 1: k */
    /* REMEMBER! initial index is 1, not 0 */
    lua_pushinteger(L, upcounter);

    /* result 2: v (= kth field) */
    /* struct stanza_field_s **kth_field = lua_newuserdata(L, sizeof(struct stanza_field_s*)); */
    /* *kth_field = current_fld; */
    lua_createtable(L, 0, 1);
    char *fldname = field_name[current_fld->type];
    lua_pushstring(L, fldname); /* key */
    switch(current_fld->type) {
    case FIELD_NAME:
        lua_pushstring(L, current_fld->name);
        /* return 1; */
        break;
    case FIELD_PUBLIC_NAME:
        lua_pushstring(L, current_fld->name);
        /* return 1; */
        break;
    case FIELD_LIBRARIES: {
        /* data is UT_array ptr, but userdata is ptr ptr */
        //FIXME: create a table
        UT_array **libs = lua_newuserdata(L, sizeof(UT_array*));
        *libs = current_fld->libraries; /* array of strings */
        luaL_getmetatable(L, "OBazl.dune.wordlist");
        lua_setmetatable(L, -2);
        /* return 1; */
        break;
    }
    case FIELD_MODULES: {
        //FIXME: create a table
        struct modules_s **modules = lua_newuserdata(L, sizeof(struct modules_s*));
        *modules = current_fld->modules;
        luaL_getmetatable(L, "OBazl.dune.modules");
        lua_setmetatable(L, -2);
        /* return 1; */
        break;
    }
    default:
        /* log_warn("Unexpected key: %d", key); */
        return 0;
    }
    lua_settable(L, -3);
    /* luaL_getmetatable(L, "OBazl.dune.field"); */
    /* lua_setmetatable(L, -2); */
    /* log_debug("returning kth fld: %p", *kth_field); */

    return 2;
}

static int dune_fields_pairs (lua_State *L) {
    log_debug("dune_fields_pairs");
    struct stanza_field_s *fields = *(struct stanza_field_s **)lua_touserdata(L, 1);

    /* return 1: iterator */
    int len = HASH_CNT(hh, fields);
    log_debug("fields ct: %d", len);
    lua_pushinteger(L, len);    /* upval 1: fld ct */
    lua_pushinteger(L, 0);      /* upval 2: counter */
    lua_pushlightuserdata(L, fields); /* upval 3: ptr to initial item */
    lua_pushcclosure(L, dune_fields_enumerator, 3);

    /* return 2: invariant state (fields list) */
    struct stanza_field_s **new_fields = lua_newuserdata(L, sizeof(struct stanza_field_s*));
    *new_fields = fields;
    luaL_getmetatable(L, "OBazl.dune.fields");
    lua_setmetatable(L, -2);

    /* return 3: initial value for control var: ptr to initial field*/
    /* struct stanza_field_s **initial_field = lua_newuserdata(L, sizeof(struct stanza_field_s*)); */
    /* *initial_field = fields; */
    lua_pushlightuserdata(L, fields); /* deref to get ptr to initial fld */
    luaL_getmetatable(L, "OBazl.dune.fields");
    lua_setmetatable(L, -2);

    return 3;
}

/* **************************************************************** */
static int dune_field_gc (lua_State *L) {
    log_debug("dune_field_gc");
    /* DIR *d = *(DIR **)lua_touserdata(L, 1); */
    /* if (d) closedir(d); */
    return 0;
}

static int dune_field_index (lua_State *L) {
    log_debug("dune_field_index");
    struct stanza_field_s *fld = *(struct stanza_field_s **)lua_touserdata(L, 1);

    const char *key = luaL_checkstring(L, 2);
    log_debug("indexing fld at: %s", key);

    int id = field_name_to_id(key);
    if (id != fld->type) {
        log_debug("fld value for key: false");
        lua_pushboolean(L, false);
        return 1;
    } else {
        lua_createtable(L, 0, 1);   /* table with one field, 'val' */
        lua_pushstring(L, "val");   /* key */

        /* now push value of 'val' */
        switch(id) {
        case FIELD_NAME:
            log_debug("found FIELD_NAME");
            lua_pushstring(L, fld->name);
            /* return 1; */
            break;
        case FIELD_PUBLIC_NAME:
            lua_pushstring(L, fld->name);
            /* return 1; */
            break;
        case FIELD_LIBRARIES: {
            /* data is UT_array ptr, but userdata is ptr ptr */
            //FIXME: create a table
            UT_array **libs = lua_newuserdata(L, sizeof(UT_array*));
            *libs = fld->libraries; /* array of strings */
            luaL_getmetatable(L, "OBazl.dune.wordlist");
            lua_setmetatable(L, -2);
            /* return 1; */
            break;
        }
        case FIELD_MODULES: {
            //FIXME: create a table
            struct modules_s **modules = lua_newuserdata(L, sizeof(struct modules_s*));
            *modules = fld->modules;
            luaL_getmetatable(L, "OBazl.dune.modules");
            lua_setmetatable(L, -2);
            /* return 1; */
            break;
        }
        default:
            log_warn("Unexpected key: %d", key);
            return 0;
        }
        lua_settable(L, -3);
        return 1;
    }
}

static int dune_field_len (lua_State *L) {
    log_debug("dune_field_len");
    struct stanza_field_s *fields = *(struct stanza_field_s **)lua_touserdata(L, 1);
    int len = HASH_CNT(hh, fields);
    lua_pushinteger(L, len);
    return 1;
}

/* **************************************************************** */
/* metatable: OBazl.dune.modules */
static int dune_modules_gc (lua_State *L) {
    log_debug("dune_modules_gc");
    return 0;
}

static int dune_modules_index (lua_State *L) {
    log_debug("dune_modules_index");
    struct modules_s *modules = *(struct modules_s**)lua_touserdata(L, 1);

    const char *key = luaL_checkstring(L, 2);
    log_debug("indexing %s", key);

    UT_array **words = lua_newuserdata(L, sizeof(UT_array*));
    luaL_getmetatable(L, "OBazl.dune.wordlist");
    lua_setmetatable(L, -2);

    if (strncmp(key, "include", 7) == 0) {
        if (modules->include) {
            *words = modules->include;
        } else {
            log_debug("not found");
            return 0;
        }
    } else {
        if (strncmp(key, "exclude", 7) == 0) {
            if (modules->exclude) {
                *words = modules->exclude;
            } else {
                log_debug("not found");
                return 0;
            }
        } else {
            if (strncmp(key, "fileseq", 7) == 0) {
                if (modules->fileseq) {
                    *words = modules->fileseq;
                } else {
                    log_debug("not found");
                    return 0;
                }
            } else {
                log_debug("invalid key: %s", key);
                return 0;
            }
        }
    }
    return 1;
}

static int dune_modules_len (lua_State *L)
{
    log_debug("dune_modules_len");
    lua_pushinteger(L, 3);
    return 1;
}

static int dune_modules_enumerator (lua_State *L)
{

    log_debug("dune_modules_enumerator");

    /* first the upvalues */
    int modules_count = lua_tointeger(L, lua_upvalueindex(1));
    log_debug("modules_count: %d", modules_count);
    int upcounter = lua_tointeger(L, lua_upvalueindex(2));
    log_debug("upcounter: %d", upcounter);

    /* now the args */
    struct modules_s *modules = (struct modules_s*)lua_touserdata(L, 1);
    log_debug("invariant (modules): %p", modules);

    int arg = lua_tointeger(L, 2);
    log_debug("ctl var value:: %d", arg);

    if (upcounter >= modules_count) return 0;

    /* prep for the next iteration */
    lua_pushinteger(L, ++upcounter);
    lua_copy(L, -1, lua_upvalueindex(2));

    /* now push the k,v results */
    /* result 1: string key */
    /* result 2: wordlist */
    switch(upcounter) {
    case 0: {
        lua_pushstring(L, "fileseq");
        UT_array **fileseq = lua_newuserdata(L, sizeof(UT_array*));
        *fileseq = modules->fileseq;
        luaL_getmetatable(L, "OBazl.dune.wordlist");
        lua_setmetatable(L, -2);
        break;
    }
    case 1: {
        lua_pushstring(L, "include");
        UT_array **include = lua_newuserdata(L, sizeof(UT_array*));
        *include = modules->include;
        luaL_getmetatable(L, "OBazl.dune.wordlist");
        lua_setmetatable(L, -2);
        break;
    }
    case 2: {
        lua_pushstring(L, "exclude");
        UT_array **exclude = lua_newuserdata(L, sizeof(UT_array*));
        *exclude = modules->exclude;
        luaL_getmetatable(L, "OBazl.dune.wordlist");
        lua_setmetatable(L, -2);
        break;
    }
    }
    return 2;
}

static int dune_modules_pairs (lua_State *L)
{
    log_debug("dune_modules_pairs");
    struct modules_s *modules = (struct modules_s *)lua_touserdata(L, 1);

    /*  result 1: "generator" closure with upvalues */
    int len = 3; //FIXME: omit null flds
    lua_pushinteger(L, len);    /* upval 1: word ct */
    lua_pushinteger(L, 0);      /* upval 2: counter */
    lua_pushcclosure(L, dune_modules_enumerator, 2);

    /* result 2: invariant */
    struct modules_s **new_modules = lua_newuserdata(L, sizeof(struct modules_s*));
    *new_modules = modules;
    luaL_getmetatable(L, "OBazl.dune.modules");
    lua_setmetatable(L, -2);

    /* result 3: initial val for control var */
    lua_pushinteger(L, 0);
    return 3;
}

static int dune_modules_tostring (lua_State *L) {
    /* log_debug("dune_modules_tostring"); */
    struct modules_s *new_modules = *(struct modules_s**)lua_touserdata(L, 1);
    UT_string *buf;
    utstring_new(buf);
    char **p = NULL;
    utstring_printf(buf, "Include: ");
    while ( (p=(char**)utarray_next(new_modules->include, p))) {
        utstring_printf(buf, "%s ", *p);
    }
    utstring_printf(buf, "; Exclude: ");
    p = NULL;
    while ( (p=(char**)utarray_next(new_modules->exclude, p))) {
        utstring_printf(buf, "%s ", *p);
    }
    /* log_debug("done"); */

    lua_pushstring(L, utstring_body(buf));
    utstring_free(buf);
    return 1;
}

/* **************************************************************** */
/* metatable: OBazl.dune.wordlist */
static int dune_wordlist_gc (lua_State *L) {
    log_debug("dune_wordlist_gc");
    return 0;
}

/* static int dune_wordlist_call (lua_State *L) { */
/*     char *fn = lua_tostring(L, 1); */
/*     log_debug("dune_wordlist_call: %s", fn); */
/*     /\* log_debug("fn: %s", fn); *\/ */
/*     return 0; */
/* } */

static int dune_wordlist_index (lua_State *L) {
    /* log_debug("dune_wordlist_index"); */
    UT_array *strings = *(UT_array **)lua_touserdata(L, 1);
    int i = luaL_checkinteger(L, 2);
    int len = utarray_len(strings);
    if (i > 0 && i <= len) {
        char **s = (char**)utarray_eltptr(strings, i-1);
        lua_pushstring(L, *s);
        return 1;
    } else {
        /* NB: iteration terminates when nil is returned; this will
           happen when the iterator tries to access index len+1. */
        return 0;
    }
}

static int dune_wordlist_len (lua_State *L)
{
    log_debug("dune_wordlist_len");
    UT_array *strings = *(UT_array **)lua_touserdata(L, 1);
    lua_pushinteger(L, utarray_len(strings));
    return 1;
}

static int dune_wordlist_enumerator (lua_State *L)
{

    /* first the upvalues */
    /* log_debug("dune_wordlist_enumerator"); */
    int wordcount = lua_tointeger(L, lua_upvalueindex(1));
    /* log_debug("wordcount: %d", wordcount); */
    int upcounter = lua_tointeger(L, lua_upvalueindex(2));
    /* log_debug("upcounter: %d", upcounter); */
    char **curritem = (char**)lua_touserdata(L, lua_upvalueindex(3)); /* initially NULL */
    /* log_debug("curritem ptr: %p", curritem); */

    /* now the args */
    /* ignore first, we have it in upvalue 3 */
    UT_array *words = *(UT_array **)lua_touserdata(L, 1);
    /* log_debug("invariant (words): %p", words); */

    /* ignore */
    /* int arg = lua_tointeger(L, 2); */
    /* log_debug("ctl var value:: %d", arg); */

    curritem=(char**)utarray_next(words, curritem);
    if (curritem == NULL) return 0;
    if (upcounter >= wordcount) return 0;

    /* prep for the next iteration */
    lua_pushinteger(L, ++upcounter);
    lua_copy(L, -1, lua_upvalueindex(2));

    char **next_item = lua_newuserdata(L, sizeof(char*));
    next_item = curritem;
    lua_pushlightuserdata(L, curritem); /* upval 3: ptr to next item */
    lua_copy(L, -1, lua_upvalueindex(3));

    /* now push the k,v results */
    lua_pushinteger(L, upcounter);
    lua_pushstring(L, *curritem);
    return 2;
}

static int dune_wordlist_pairs (lua_State *L)
{
    log_debug("dune_wordlist_pairs");
    UT_array *words = *(UT_array **)lua_touserdata(L, 1);

    /*  result 1: "generator" closure with upvalues */
    char **curritem = NULL;           /* we use this to keep track of where we are in list */
    int len = utarray_len(words);
    /* log_debug("wordlist ct: %d", len); */
    lua_pushinteger(L, len);    /* upval 1: word ct */
    lua_pushinteger(L, 0);      /* upval 2: counter */
    lua_pushlightuserdata(L, curritem); /* upval 3: ptr to current item in list */
    lua_pushcclosure(L, dune_wordlist_enumerator, 3);

    /* result 2: invariant (word list) */
    UT_array **new_words = lua_newuserdata(L, sizeof(UT_array*));
    *new_words = words;
    luaL_getmetatable(L, "OBazl.dune.wordlist");
    lua_setmetatable(L, -2);

    /* result 3: initial val for control var */
    lua_pushinteger(L, 0);

    return 3;
}

static int dune_wordlist_tostring (lua_State *L)
{
    /* log_debug("dune_wordlist_tostring"); */
    UT_array *strings = *(UT_array **)lua_touserdata(L, 1);
    UT_string *buf;
    utstring_new(buf);
    char **p = NULL;
    while ( (p=(char**)utarray_next(strings, p))) {
        utstring_printf(buf, "%s ", *p);
    }
    /* log_debug("done"); */

    lua_pushstring(L, utstring_body(buf));
    utstring_free(buf);
    return 1;
}

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
    /* create userdata first */
    /* *pkg = NULL; */

    luaL_newmetatable(L, "OBazl.dune.pkg");

    struct obazl_dune_package_s **pkg0 = lua_newuserdata(L, sizeof(struct obazl_dune_package_s *));
    *pkg0 = parsed;
    lua_pushcclosure(L, dune_pkg_gc, 1);
    lua_setfield(L, -2, "__gc");

    struct obazl_dune_package_s **pkg1 = lua_newuserdata(L, sizeof(struct obazl_dune_package_s *));
    *pkg1 = parsed;
    lua_pushcclosure(L, dune_pkg_index, 1);
    lua_setfield(L, -2, "__index");

    struct obazl_dune_package_s **pkg2 = lua_newuserdata(L, sizeof(struct obazl_dune_package_s *));
    *pkg2 = parsed;
    lua_pushcclosure(L, dune_pkg_pairs, 1);
    lua_setfield(L, -2, "__pairs");

    log_debug("5 xxxx");

    lua_newtable(L);
    log_debug("6 xxxx");
    luaL_getmetatable(L, "OBazl.dune.pkg");
    lua_setmetatable(L, -2);

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
LUAMOD_API int luaopen_obazl_dune (lua_State *L) {
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

    luaL_newmetatable(L, "OBazl.dune.stanzas");
    lua_pushcfunction(L, dune_stanzas_gc);
    lua_setfield(L, -2, "__gc");
    lua_pushcfunction(L, dune_stanzas_index);
    lua_setfield(L, -2, "__index");
    lua_pushcfunction(L, dune_stanzas_len);
    lua_setfield(L, -2, "__len");
    lua_pushcfunction(L, dune_stanzas_pairs);
    lua_setfield(L, -2, "__pairs");

    luaL_newmetatable(L, "OBazl.dune.stanza");
    lua_pushcfunction(L, dune_stanza_gc);
    lua_setfield(L, -2, "__gc");
    lua_pushcfunction(L, dune_stanza_index);
    lua_setfield(L, -2, "__index");
    lua_pushcfunction(L, dune_stanza_len);
    lua_setfield(L, -2, "__len");
    lua_pushcfunction(L, dune_stanza_pairs);
    lua_setfield(L, -2, "__pairs");

    luaL_newmetatable(L, "OBazl.dune.fields");
    lua_pushcfunction(L, dune_fields_gc);
    lua_setfield(L, -2, "__gc");
    lua_pushcfunction(L, dune_fields_index);
    lua_setfield(L, -2, "__index");
    lua_pushcfunction(L, dune_fields_len);
    lua_setfield(L, -2, "__len");
    lua_pushcfunction(L, dune_fields_pairs);
    lua_setfield(L, -2, "__pairs");

    luaL_newmetatable(L, "OBazl.dune.field");
    lua_pushcfunction(L, dune_field_gc);
    lua_setfield(L, -2, "__gc");
    lua_pushcfunction(L, dune_field_index);
    lua_setfield(L, -2, "__index");
    lua_pushcfunction(L, dune_field_len);
    lua_setfield(L, -2, "__len");

    luaL_newmetatable(L, "OBazl.dune.modules");
    lua_pushcfunction(L, dune_modules_gc);
    lua_setfield(L, -2, "__gc");
    lua_pushcfunction(L, dune_modules_index);
    lua_setfield(L, -2, "__index");
    lua_pushcfunction(L, dune_modules_len);
    lua_setfield(L, -2, "__len");
    lua_pushcfunction(L, dune_modules_pairs);
    lua_setfield(L, -2, "__pairs");
    lua_pushcfunction(L, dune_modules_tostring);
    lua_setfield(L, -2, "__tostring");

    luaL_newmetatable(L, "OBazl.dune.wordlist");
    lua_pushcfunction(L, dune_wordlist_gc);
    lua_setfield(L, -2, "__gc");
    lua_pushcfunction(L, dune_wordlist_index);
    lua_setfield(L, -2, "__index");
    lua_pushcfunction(L, dune_wordlist_len);
    lua_setfield(L, -2, "__len");
    lua_pushcfunction(L, dune_wordlist_pairs);
    lua_setfield(L, -2, "__pairs");
    lua_pushcfunction(L, dune_wordlist_tostring);
    lua_setfield(L, -2, "__tostring");

    /* create the library */

    luaL_newlib(L, obazl_dune);
    /* add more stuff to the table created by luaL_newlib ... */
    lua_pushstring(L, "0.2.0");
    lua_setfield(L, -2, "version");
    return 1;
}
