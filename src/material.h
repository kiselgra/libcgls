#ifndef __MATERIAL_H__ 
#define __MATERIAL_H__ 

#include <libcgl/libcgl.h>
#include "slist.h"

/* a very simple material definition, suited to standard gl rendering. 
 * extensible the good ol' c way.
 */

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	int id;
} material_ref;

inline bool equal_material_refs(material_ref a, material_ref b) {
	return a.id == b.id;
}

define_slist(texture_node, const char *name; texture_ref ref; int unit);


material_ref make_material(const char *name, vec4f *amb, vec4f *diff, vec4f *spec);
material_ref make_material3f(const char *name, vec3f *amb, vec3f *diff, vec3f *spec);
material_ref find_material(const char *name);
const char* material_name(material_ref ref);

vec4f* material_ambient_color(material_ref ref);
vec4f* material_diffuse_color(material_ref ref);
vec4f* material_specular_color(material_ref ref);
float material_specular_exponent(material_ref ref);
void material_set_specular_exponent(material_ref ref, float s);

int material_number_of_textures(material_ref ref);
struct texture_node* material_textures(material_ref ref);
void material_add_texture(material_ref ref, texture_ref tex);
void material_add_texture_as(material_ref ref, texture_ref tex, const char *name);

shader_ref material_shader(material_ref);

void* material_aux(material_ref ref);
void material_set_aux(material_ref ref, void *aux);

#ifdef __cplusplus
}
#endif

#endif

