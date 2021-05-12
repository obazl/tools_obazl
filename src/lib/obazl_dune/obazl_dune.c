#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>
#endif

#include "log.h"

#include "utarray.h"

#include "obazl_dune.h"

int errnum;

/* #if EXPORT_INTERFACE */
/* struct logging { */
/*     int verbosity; */
/*     int log_level; */
/*     int parse_verbosity; */
/*     int parse_log_level; */
/*     int lex_verbosity; */
/*     int lex_log_level; */
/*     bool quiet; */
/*     bool log_color; */
/* }; */
/* #endif */

/* EXPORT struct logging logger; */

/* struct obzl_dune_package *ast; */

EXPORT char *obzl_dune_version()
{
    return "0.1.0";
}

LOCAL char *package_name_from_file_name(char *fname)
{
    char *bn = basename(fname);
    int x = strlen(bn) - 5;
    if ( strncmp(&bn[x], ".DUNE", 5) == 0) {
        bn[x] = '\0';
        return bn;
    } else {
        return basename(dirname(fname));
    }
}

LOCAL bool is_empty(const char *s)
{
  while (*s) {
    if (!isspace(*s))
      return false;
    s++;
  }
  return true;
}

/* EXPORT struct dune_package_s *obzl_dune_parse_file(char *fname) */
/* { */
/*     log_info("obzl_dune_parse_file: %s", fname); */

/*     return parse_dunefile(fname); */

/*     /\* FILE *f; *\/ */

/*     /\* f = fopen(fname, "r"); *\/ */
/*     /\* if (f == NULL) { *\/ */
/*     /\*     /\\* errnum = errno; *\\/ *\/ */
/*     /\*     /\\* log_error("fopen failure for %s", fname); *\\/ *\/ */
/*     /\*     /\\* log_error("Value of errno: %d", errnum); *\\/ *\/ */
/*     /\*     /\\* log_error("fopen error %s", strerror( errnum )); *\\/ *\/ */
/*     /\*     return NULL; *\/ */
/*     /\* } *\/ */
/*     /\* fseek(f, 0, SEEK_END); *\/ */
/*     /\* const size_t fsize = (size_t) ftell(f); *\/ */
/*     /\* if (fsize == 0) { *\/ */
/*     /\*     fclose(f); *\/ */
/*     /\*     errno = -1; *\/ */
/*     /\*     return NULL; *\/ */
/*     /\* } *\/ */
/*     /\* fseek(f, 0, SEEK_SET); *\/ */
/*     /\* char *buffer = (char*) malloc(fsize + 1); *\/ */
/*     /\* fread(buffer, 1, fsize, f); *\/ */
/*     /\* buffer[fsize] = 0; *\/ */
/*     /\* fclose(f); *\/ */

/*     /\* if (is_empty(buffer)) { *\/ */
/*     /\*     fclose(f); *\/ */
/*     /\*     errno = -2; *\/ */
/*     /\*     return NULL; *\/ */
/*     /\* } *\/ */

/*     /\* free(buffer); *\/ */
/*     /\* return ast; *\/ */
/* } */
