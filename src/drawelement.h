#ifndef __CGLS_DRAWELEMENT_H__ 
#define __CGLS_DRAWELEMENT_H__ 

#include <libcgl/libcgl.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	int id;
} drawelement_ref;



typedef bool (*uniform_setter_t)(drawelement_ref drawelement, const char *uniform, int location);
bool default_matrix_uniform_handler(drawelement_ref ref, const char *uniform, int location);


drawelement_ref make_drawelement(const char *modelname, mesh_ref mr, shader_ref sr);

void prepend_uniform_handler(drawelement_ref ref, uniform_setter_t handler);
void render_drawelement(drawelement_ref ref);


#ifdef __cplusplus
}
#endif


#endif

