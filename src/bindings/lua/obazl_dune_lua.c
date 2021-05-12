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
#if INTERFACE
#include "utarray.h"
#endif
#include "uthash.h"
#include "utstring.h"

#include "obazl.h"
#include "obazl_dune_lua.h"

/* **************************************************************** */
void stanzas_table(lua_State *L, obzl_dune_package_s *pkg)
{
    log_debug(">>>>stanzas_table");
    int len = utarray_len((UT_array*)pkg->stanzas);
    log_debug("stanza ct: %d", len);
    lua_createtable(L, len, 0);
    struct stanza_s **next_stanza = NULL; /* stanzas is a list of ptr ptrs */
    int i = 0;
    for(next_stanza=(struct stanza_s**)utarray_front(pkg->stanzas);
        next_stanza!=NULL;
        next_stanza=(struct stanza_s**)utarray_next(pkg->stanzas, next_stanza)) {

        log_debug("stanza i: %d, %p", i, next_stanza);
        lua_pushinteger(L, ++i);  /* index */
        // NB: deref the ptr ptr
        stanza_table(L, *next_stanza); /* creates and pushes stanza table */
        lua_settable(L, -3);
    }
    return;
}

void stanza_table(lua_State *L, struct stanza_s *stanza)
{
    log_debug(">>>>stanza_table");

    lua_newtable(L);
    lua_pushstring(L, "type");  /* key */
    const char *typ = stanza_name[stanza->type];
    lua_pushstring(L, typ);     /* val */
    lua_settable(L, -3);

    int len = HASH_CNT(hh, stanza->fields);
    if (len > 0) {
        lua_pushstring(L, "fields");  /* key */
        lua_newtable(L);
        struct stanza_field_s *next_fld;
        int i = 0;
        for (next_fld = stanza->fields; next_fld != NULL; next_fld = next_fld->hh.next) {
            field_table(L, next_fld);
        }
        /* i++; */
        /* for (next_fld = stanza->fields; next_fld != NULL; next_fld = next_fld->hh.next) { */
        /*      i = field_flag(L, next_fld, i); */
        /* } */
        lua_settable(L, -3);
    } else {
        lua_pushnil(L);
        log_error("empty fields");
    }
    return;
}

void field_table(lua_State *L, struct stanza_field_s *field)
{
    log_debug(">>>>field_table");

    /* lua_newtable(L); */
    /* lua_pushstring(L, "type");  /\* key *\/ */
    const char *typ = field_name[field->type];
    log_debug("adding table %s", typ);
    /* key: */
    lua_pushstring(L, typ);

    /* val: */
    switch(field->type) {
    case FIELD_NAME:
        lua_pushstring(L, field->name);
        break;
    case FIELD_PUBLIC_NAME:
        lua_pushstring(L, field->name);
        break;
    case FIELD_LIBRARIES: {
        lua_newtable(L);
        lua_pushstring(L, "list");
        wordlist_table(L, field->libraries);
        lua_settable(L, -3);
        break;
    }
    case FIELD_MODULES: {
        modules_table(L, (struct modules_s*)field->modules);
        break;
    }
    case FIELD_PREPROCESS: {
        //FIXME: create a table
        lua_pushstring(L, "FIXME:preprocess");
        break;
    }
    default:
        log_debug("DEFAULT");
        lua_pushnil(L);
    }
    lua_settable(L, -3);
    return;
}

void modules_table(lua_State *L, struct modules_s *module)
{
    int len;

    lua_newtable(L);

    if (module->exclude) {
        len = utarray_len((UT_array*)module->include);
        if (len > 0) {
            lua_pushstring(L, "include");
            lua_newtable(L);
            lua_pushstring(L, "list");
            wordlist_table(L, module->include);
            lua_settable(L, -3);
            lua_settable(L, -3);
        }
    }

    if (module->exclude) {
        len = utarray_len((UT_array*)module->exclude);
        if (len > 0) {
            lua_pushstring(L, "exclude");
            lua_newtable(L);
            lua_pushstring(L, "list");
            wordlist_table(L, module->exclude);
            lua_settable(L, -3);
            lua_settable(L, -3);
        }
    }

    if (module->resolved) {
        len = utarray_len((UT_array*)module->resolved);
        if (len > 0) {
            lua_pushstring(L, "resolved");
            lua_newtable(L);
            lua_pushstring(L, "list");
            wordlist_table(L, module->exclude);
            lua_settable(L, -3);
            lua_settable(L, -3);
        }
    }
}

void wordlist_table(lua_State *L, UT_array *words)
{
    lua_newtable(L);

    int len = utarray_len((UT_array*)words);

    char **word = NULL;
    int i = 0;
    while ( (word=(char**)utarray_next(words, word)) ) {
        lua_pushinteger(L, ++i);
        lua_newtable(L);
        lua_pushstring(L, "item");
        lua_pushstring(L, *word);
        lua_settable(L, -3);
        if (i >= len) {
            lua_pushstring(L, "last");
            lua_pushboolean(L, true);
            lua_settable(L, -3);
        }
        lua_settable(L, -3);
    }
    /* lua_settable(L, -3); */
    /* lua_settable(L, -3); */
}

int field_flag(lua_State *L, struct stanza_field_s *field, int i)
{
    log_debug(">>>>field_flag %d", i);
    /* lua_newtable(L); */

    switch(field->type) {
    case FIELD_LIBRARIES: {
        log_debug("pushing flag field a");
        lua_pushinteger(L, i++);
        lua_newtable(L);
        lua_pushstring(L, "has_libraries");
        lua_pushboolean(L, true);
        lua_settable(L, -3);
        lua_settable(L, -3);
        return i;
        break;
    }
    case FIELD_MODULES: {
        log_debug("pushing flag field b");
        lua_pushinteger(L, i++);
        lua_newtable(L);
        lua_pushstring(L, "has_modules");
        lua_pushboolean(L, true);
        lua_settable(L, -3);
        lua_settable(L, -3);
        return i;
        break;
    }
    case FIELD_PREPROCESS: {
        log_debug("pushing flag field c");
        lua_pushinteger(L, i++);
        lua_newtable(L);
        lua_pushstring(L, "has_preprocess");
        lua_pushboolean(L, true);
        lua_settable(L, -3);
        lua_settable(L, -3);
        return i;
        break;
    }
    }
    return i;
}
