#ifndef __CGLS_DRAWELEMENT_H__ 
#define __CGLS_DRAWELEMENT_H__ 

#include <libcgl/libcgl.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	int id;
} drawelement_ref;



typedef bool (*uniform_setter_t)(drawelement_ref drawelement, shader_ref shader);



drawelement_ref make_drawelement(const char *modelname, mesh_ref mr, shader_ref sr);

void render_drawelement(drawelement_ref ref);


#ifdef __cplusplus
}
#endif


#endif

