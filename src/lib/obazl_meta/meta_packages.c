#include "meta_packages.h"
#include "log.h"

static int indent = 2;
static int delta = 2;
static char *sp = " ";

#if INTERFACE
struct obazl_meta_package {
    char *name;
    char *directory;
    char *metafile;
    obazl_meta_entries *entries;          /* list of struct obazl_meta_entry */
};
#endif

EXPORT char *obazl_meta_package_name(obazl_meta_package *_pkg)
{
    return _pkg->name;
}

EXPORT char *obazl_meta_package_dir(obazl_meta_package *_pkg)
{
    return _pkg->directory;
}

EXPORT char *obazl_meta_package_src(obazl_meta_package *_pkg)
{
    return _pkg->metafile;
}

EXPORT obazl_meta_entries *obazl_meta_package_entries(obazl_meta_package *_pkg)
{
    return _pkg->entries;
}

EXPORT obazl_meta_property *obazl_meta_package_property(obazl_meta_package *_pkg, char *_name)
{
#if DEBUG_TRACE
    log_trace("obazl_meta_package_property('%s')", _name);
#endif
    /* utarray_find requires a sort; not worth the cost */
    obazl_meta_entry *e = NULL;
    for (int i = 0; i < obazl_meta_entries_count(_pkg->entries); i++) {
        e = obazl_meta_entries_nth(_pkg->entries, i);
        if (e->type == OMP_PROPERTY) {
            if (strncmp(e->property->name, _name, 256) == 0) {
                return e->property;
            }
        }
        /* log_debug("iteration %d", i); */
    }
    return NULL;
}

/* **************************************************************** */
#if DEBUG_TRACE
void dump_package(int indent, struct obazl_meta_package *pkg)
{
    log_debug("%*sdump_package:", indent, sp);
    log_debug("%*sname:      %s", delta+indent, sp, pkg->name);
    log_debug("%*sdirectory: %s", delta+indent, sp, pkg->directory);
    log_debug("%*smetafile:  %s", delta+indent, sp, pkg->metafile);
    dump_entries(delta+indent, pkg->entries);
}
#endif
