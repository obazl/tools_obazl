#include <ctype.h>
#include <libgen.h>
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>             /* PATH_MAX */
#endif
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "utstring.h"

#include "log.h"

#include "obazl_utils.h"

int strsort(const void *_a, const void *_b)
{
    const char *a = *(const char* const *)_a;
    const char *b = *(const char* const *)_b;
    /* printf("strsort: %s =? %s\n", a, b); */
    return strcmp(a,b);
}

EXPORT char* mystrcat( char* dest, char* src )
{
     while (*dest) dest++;
     while ( (*dest++ = *src++) );
     return --dest;
}

bool is_empty(const char *s)
{
  while (*s) {
    if (!isspace(*s))
      return false;
    s++;
  }
  return true;
}

/**
   prereq: fname is .ml or .mli
   user must free result
*/
char *fname_to_pkgname(char *fname)
{
    /* log_debug("fname_to_pkgname: %s", fname); */
    /* dirname(3) may modify its arg, so we copy it */

    char *work = strdup(fname);
    char *dname = dirname(work);
    char *pkgname = basename(dname);
    free(work);
    return strdup(pkgname);
}

/**
   prereq: fname is .ml or .mli
   user must free result
*/
char *fname_to_pkgpath(char *fname)
{
    /* log_debug("fname_to_pkgpath: %s", fname); */
    /* dirname(3) may modify its arg, so we copy it */

    char *work = strdup(fname);
    char *path = dirname(work);
    free(work);
    return strdup(path);
}

/**
   prereq: fname is .ml or .mli
   user must free result
*/
UT_string *fname_to_target(char *fname)
{
    /* log_debug("fname_to_target: %s", fname); */

    char *work = strdup(fname);
    char *path = dirname(work);
    char *mod = fname_to_mname(fname);
    UT_string *result;
    utstring_new(result);
    utstring_printf(result, "//%s:%s", path, mod);
    return result;
}

/**
   user must free result?
*/
char *fname_to_mname(char *fname)
{
    /* log_debug("fname_to_mname: %s", fname); */
    char *bname = basename(fname);
    char *result;
    int len = strlen(bname);
    char *p = bname;
    if (strncmp(".ml", p+len-3, 3) == 0) {
        result = strndup(bname, len-3);
    } else {
        if (strncmp(".mli", p+len-4, 4) == 0) {
            result = strndup(bname, len-4);
        } else {
            return NULL;
        }
    }
    result[0] = toupper(result[0]);
    return result;
}

/**
   user must free result?
*/
bool is_structfile(char *fname)
{
    /* log_debug("is_structfile: %s", fname); */
    int len = strlen(fname);
    bool result = strncmp(&fname[len-3], ".ml", 3);
    return (bool)!result;       /* 0 = eq, 1 = true */
}

bool is_sigfile(char *fname)
{
    /* log_debug("is_sigfile %s", fname); */
    int len = strlen(fname);
    char *p = fname;
    if (strncmp(".mli", p+len-4, 4) == 0) {
        /* log_debug("sigfile yes"); */
        return true;
    }
    else return false;
}

//FIXME: switch from popen to posix_spawn
char *run_cmd(char *cmd)
{
    static char buf[BUFSIZ];
    FILE *fp;

    if ((fp = popen(cmd, "r")) == NULL) {
        printf("Error opening pipe!\n");
        return NULL;
    }

    while (fgets(buf, sizeof buf, fp) != NULL) {
        /* printf("SWITCH: %s\n", buf); */
        buf[strcspn(buf, "\n")] = 0;
    }

    if(pclose(fp))  {
        printf("Command not found or exited with error status\n");
        return NULL;
    }
    return buf;
}

char *get_cwd(void)
{
    log_debug("GET_CWD");
    char cwd[PATH_MAX];
    memset(cwd, '\0', PATH_MAX);
    /* int cwd_len = 0; */
    if (getcwd(cwd, sizeof(cwd)) != NULL) {
        log_debug("Current working dir: %s", cwd);
        return (char*)&cwd;
    } else {
        log_error("getcwd failure");
        return NULL;
    }
}
