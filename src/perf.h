#ifndef __CGLS_PERF_H__ 
#define __CGLS_PERF_H__ 

#ifdef __cplusplus
extern "C" {
#endif

	extern void (*init_pre)(void);
	extern void (*init_post)(void);
	extern void (*update)(void);
	extern void (*render)(void);

// 	extern bool make_gbuffer;

	void enter(const char *name, int glmaj, int glmin, int res_x, int res_y, const char *configfile);

#ifdef __cplusplus
}
#endif

#endif

