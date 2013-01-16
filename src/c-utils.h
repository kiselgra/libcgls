#ifndef __CGLS_C_UTILS_H__ 
#define __CGLS_C_UTILS_H__ 

#define define_slist(NAME, DECLS) struct NAME { struct NAME *next; DECLS; };

char* strappend(const char *pref, const char *suf);

#endif

