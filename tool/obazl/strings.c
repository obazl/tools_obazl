#include <ctype.h>
#include <string.h>
#include <stdbool.h>

#include "strings.h"

bool is_uppercase(char *s)
{
    static char c;
    while(*s++) {
        c = *s;
        if (islower(c))
            return false;
    }
    return true;
}


char* mystrcat( char* dest, char* src )
{
     while (*dest) dest++;
     while ( (*dest++ = *src++) );
     return --dest;
}
