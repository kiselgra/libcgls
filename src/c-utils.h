#ifndef __CGLS_C_UTILS_H__ 
#define __CGLS_C_UTILS_H__ 

#define define_slist(NAME, DECLS) struct NAME { struct NAME *next; DECLS; };

#ifdef __cplusplus
extern "C" {
#endif

char* strappend(const char *pref, const char *suf);
char* strappend3(const char *pref, const char *mid, const char *suf);

#ifdef __cplusplus
}
#endif

#endif

