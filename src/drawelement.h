#ifndef __CGLS_DRAWELEMENT_H__ 
#define __CGLS_DRAWELEMENT_H__ 

#include "refs.h"
#include "material.h"
#include "uniforms.h"
#include "skeletal.h"
#include "path.h"

#include <libcgl/libcgl.h>

#define CGLS_DRAWELEMENT_BB_VIS 1

#ifdef __cplusplus
extern "C" {
#endif

define_array(drawelement);

bool default_matrix_uniform_handler(drawelement_ref *ref, const char *uniform, int location);
bool default_material_uniform_handler(drawelement_ref *ref, const char *uniform, int location); // defined in material.c

drawelement_ref make_drawelement(const char *name, mesh_ref mr, shader_ref sr, material_ref matr);
bool valid_drawelement_ref(drawelement_ref ref); // mm.m4

const char* drawelement_name(drawelement_ref ref);
matrix4x4f* drawelement_trafo(drawelement_ref ref);
void replace_drawelement_trafo(drawelement_ref ref, matrix4x4f *new_trafo);
mesh_ref drawelement_mesh(drawelement_ref ref);
shader_ref drawelement_shader(drawelement_ref ref);
material_ref drawelement_material(drawelement_ref ref);
void set_drawelement_bounding_box(drawelement_ref ref, vec3f *min, vec3f *max);
void drop_drawelement_bounding_box(drawelement_ref ref);
void bounding_box_of_drawelement(drawelement_ref ref, vec3f *min, vec3f *max);
bool drawelement_has_bounding_box(drawelement_ref ref);

material_ref drawelement_change_material(drawelement_ref ref, material_ref m);
shader_ref drawelement_change_shader(drawelement_ref ref, shader_ref s);
void render_drawelement_with_shader(drawelement_ref ref, shader_ref shader);
void render_drawelement_with_material(drawelement_ref ref, material_ref material);
void render_drawelement_with(drawelement_ref ref, shader_ref shader, material_ref material);
void bind_uniforms_and_render_indices_of_drawelement(drawelement_ref ref);
void bind_uniforms_and_render_drawelement_nonindexed(drawelement_ref ref);
void set_drawelement_index_buffer_range(drawelement_ref ref, unsigned int start, unsigned int count);
bool drawelement_using_index_range(drawelement_ref ref);
unsigned int drawelement_index_range_start(drawelement_ref ref);
unsigned int drawelement_index_range_len(drawelement_ref ref);

void prepend_drawelement_uniform_handler(drawelement_ref ref, uniform_setter_t handler);
void append_drawelement_uniform_handler(drawelement_ref ref, uniform_setter_t handler);
#ifdef WITH_GUILE
void append_drawelement_scheme_uniform_handler(drawelement_ref ref, SCM handler);
#endif
void pop_drawelement_uniform_handler(drawelement_ref ref);

void hide_drawelement(drawelement_ref ref, bool hide);
bool drawelement_hidden(drawelement_ref ref);

void render_drawelement(drawelement_ref ref);
drawelement_ref find_drawelement(const char *name);
struct drawelement_list* list_drawelements();
struct drawelement_list* find_drawelements_matching(const char *name);

#if CGLS_DRAWELEMENT_BB_VIS == 1
void drawelement_show_bounding_box(drawelement_ref ref, bool yes);
bool drawelement_shows_bounding_box(drawelement_ref ref);
void render_drawelement_box(drawelement_ref ref);
#endif

struct uniform_handler_node* drawelement_uniform_handlers(drawelement_ref ref);

void assign_bones_to_drawelement(drawelement_ref ref, int n, struct bone **bones);
struct bone** drawelement_bones(drawelement_ref ref);
int drawelement_number_of_bones(drawelement_ref ref);
bool drawelement_with_bones(drawelement_ref ref);
skeletal_animation_ref drawelement_skeletal_animation(drawelement_ref ref);
void make_drawelement_part_of_skeletal_animation(drawelement_ref ref, skeletal_animation_ref sa);
matrix4x4f* drawelement_bone_matrix_area(drawelement_ref ref);

void make_drawelement_part_of_path_animation(drawelement_ref ref, path_animation_ref pa);
bool drawelement_with_path(drawelement_ref ref);
path_animation_ref drawelement_path_animation(drawelement_ref ref);

bool bone_matrix_uniform_handler(drawelement_ref *ref, const char *uniform, int location);

void enable_instancing_for_drawelement(drawelement_ref ref, int n);
void disable_instancing_for_drawelement(drawelement_ref ref, int n);
int drawelement_instances(drawelement_ref);
bool drawelement_with_instancing(drawelement_ref);
texture_ref drawelement_instance_trafos(drawelement_ref ref);
void drawelement_set_instance_trafos(drawelement_ref ref, texture_ref texbuf);

#ifdef __cplusplus
}
#endif


#endif

