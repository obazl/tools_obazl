#include <string.h>

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


