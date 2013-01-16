#include "c-utils.h"

#define _GNU_SOURCE
#include <stdlib.h>

//! concatenate two strings. the result must be free'd.
char* strappend(const char *pref, const char *suf) {
    char *res = 0;
    int n = asprintf(&res, "%s%s", pref, suf);
    if (n != strlen(pref) + strlen(suf))
        fprintf("error concatenating the strings '%s' and '%s', resulting in '%s'.\n", pref, suf, res);
    return res;
}


