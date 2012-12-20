#ifndef __CGLS_DRAWELEMENT_H__ 
#define __CGLS_DRAWELEMENT_H__ 

#include "material.h"

#include <libcgl/libcgl.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	int id;
} drawelement_ref;

struct drawelement_list {
    struct drawelement_list *next;
    drawelement_ref ref;
};

typedef bool (*uniform_setter_t)(drawelement_ref drawelement, const char *uniform, int location);
bool default_matrix_uniform_handler(drawelement_ref ref, const char *uniform, int location);
bool default_material_uniform_handler(drawelement_ref ref, const char *uniform, int location); // defined in material.c



drawelement_ref make_drawelement(const char *modelname, mesh_ref mr, shader_ref sr, material_ref matr);

const char* drawelement_name(drawelement_ref ref);
matrix4x4f* drawelement_trafo(drawelement_ref ref);
mesh_ref drawelement_mesh(drawelement_ref ref);
shader_ref drawelement_shader(drawelement_ref ref);
material_ref drawelement_material(drawelement_ref ref);

material_ref drawelement_change_material(drawelement_ref ref, material_ref m);
shader_ref drawelement_change_shader(drawelement_ref ref, shader_ref s);
void render_drawelement_with_shader(drawelement_ref ref, shader_ref shader);
void render_drawelement_with_material(drawelement_ref ref, material_ref material);
void render_drawelement_with(drawelement_ref ref, shader_ref shader, material_ref material);
void bind_uniforms_and_render_indices_of_drawelement(drawelement_ref ref);

void prepend_uniform_handler(drawelement_ref ref, uniform_setter_t handler);
void render_drawelement(drawelement_ref ref);
drawelement_ref find_drawelement(const char *name);
struct drawelement_list* list_drawelements();


#ifdef __cplusplus
}
#endif


#endif

