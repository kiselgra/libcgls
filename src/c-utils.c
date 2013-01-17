#include "c-utils.h"

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

//! concatenate two strings. the result must be free'd.
char* strappend(const char *pref, const char *suf) {
    char *res = 0;
    int n = asprintf(&res, "%s%s", pref, suf);
    if (n != strlen(pref) + strlen(suf))
        fprintf(stderr, "error concatenating the strings '%s' and '%s', resulting in '%s'.\n", pref, suf, res);
    return res;
}

char* strappend3(const char *pref, const char *mid, const char *suf) {
    char *res = 0;
    int n = asprintf(&res, "%s%s%s", pref, mid, suf);
    if (n != strlen(pref) + strlen(suf) + strlen(mid))
        fprintf(stderr, "error concatenating the strings '%s', '%s' and '%s', resulting in '%s'.\n", pref, mid, suf, res);
    return res;
}

