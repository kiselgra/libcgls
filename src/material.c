#include "material.h"
#include "drawelement.h"

#include <string.h>
#include <stdlib.h>

#include <stdio.h>

/* material. 
 *
 * a material collects all its textures into one list.
 * this is to emphasize that the material itself has no way to enforce a
 * specific use a any texture: this is controlled by the shader requesting it.
 * 
 * the texture list caches the names of the stored textures for faster
 * searching time. note that these may beome stale as you replace textures.
 *
 */
struct material {
	char *name;
	vec4f k_amb, k_diff, k_spec;
	struct texture_node *textures, *back;
	// blend stuff?
	void *aux;
};

#define TYPE material
#define ARRAY materials
#define REF material_ref
#include <libcgl/mm.h>

material_ref make_material(const char *name, vec4f *amb, vec4f *diff, vec4f *spec) {
	material_ref ref = allocate_ref();
	struct material *mat = materials+ref.id;
	
	mat->name = strdup(name);

	mat->k_amb = *amb;
	mat->k_diff = *diff;
	mat->k_spec = *spec;

	mat->textures = mat->back = 0;
	mat->aux = 0;
	return ref;
}

material_ref make_material3f(const char *name, vec3f *amb, vec3f *diff, vec3f *spec) {
	vec4f a = { amb->x, amb->y, amb->z };
	vec4f d = { diff->x, diff->y, diff->z };
	vec4f s = { spec->x, spec->y, spec->z };
	return make_material(name, &a, &d, &s);
}

material_ref find_material(const char *name) {
	material_ref ref = { -1 };
	if (!name || strlen(name) == 0) return ref;
	for (int i = 0; i < next_index; ++i)
		if (strcmp(materials[i].name, name) == 0) {
			ref.id = i;
			return ref;
		}
	return ref;
}

const char* material_name(material_ref ref) {
	struct material *mat = materials+ref.id;
	return mat->name;
}

vec4f* material_ambient_color(material_ref ref) {
	struct material *mat = materials+ref.id;
	return &mat->k_amb;
}

vec4f* material_diffuse_color(material_ref ref) {
	struct material *mat = materials+ref.id;
	return &mat->k_diff;
}

vec4f* material_specular_color(material_ref ref) {
	struct material *mat = materials+ref.id;
	return &mat->k_spec;
}

struct texture_node* material_textures(material_ref ref) {
	struct material *mat = materials+ref.id;
	return mat->textures;
}

void material_add_texture(material_ref ref, texture_ref tex) {
	struct material *mat = materials+ref.id;
	struct texture_node *new_node = malloc(sizeof(struct texture_node));
	int unit = 0;
	if (mat->textures == 0)
		mat->textures = new_node;
	else {
		mat->back->next = new_node; 
		unit = mat->back->unit;
	}
	mat->back = new_node;
	mat->back->ref = tex;
	mat->back->unit = unit;
	mat->back->name = strdup(texture_name(tex));
	mat->back->next = 0;
}

void* material_aux(material_ref ref) {
	struct material *mat = materials+ref.id;
	return mat->aux;
}

void material_set_aux(material_ref ref, void *aux) {
	struct material *mat = materials+ref.id;
	mat->aux = aux;
}

// uniform handlers

#define str_eq(X, Y) (strcmp(X, Y) == 0)
#define mat drawelement_material(ref)

bool default_material_uniform_handler(drawelement_ref ref, const char *uniform, int location) {
	if (str_eq(uniform, "ambient_color"))
		glUniform4fv(location, 1, (float*)material_ambient_color(mat));
	else if (str_eq(uniform, "diffuse_color"))
		glUniform4fv(location, 1, (float*)material_diffuse_color(mat));
	else if (str_eq(uniform, "specular_color"))
		glUniform4fv(location, 1, (float*)material_specular_color(mat));
	else {
		for (struct texture_node *run = material_textures(mat); run; run = run->next)
			if (str_eq(uniform, run->name)) {
				bind_texture(run->ref, run->unit);
				return true;
			}
		if (strlen(uniform) == 4 && uniform[3] >= '0' && uniform[3] <= '9' && strncmp(uniform, "tex", 3) == 0) {
			int nr = uniform[3] - '0';
			for (struct texture_node *run = material_textures(mat); run; run = run->next)
				if (nr == 0) {
					bind_texture(run->ref, run->unit);
					return true;
				}
		}
		return false;
	}
	return true;
}

#undef mat
#undef str_eq

#ifdef WITH_GUILE

#include <libguile.h>

SCM_DEFINE(s_make_material, "make-material", 4, 0, 0, (SCM name, SCM amb, SCM diff, SCM spec), "") {
	vec4f a = list_to_vec4f(amb),
		  d = list_to_vec4f(diff),
		  s = list_to_vec4f(spec);
	char *n = scm_to_locale_string(name);
	material_ref ref = make_material(n, &a, &d, &s);
	free(n);
	return scm_from_int(ref.id);
}

SCM_DEFINE(s_find_material, "find-material", 1, 0, 0, (SCM name), "") {
	char *n = scm_to_locale_string(name);
	material_ref ref = find_material(n);
	free(n);
	return scm_from_int(ref.id);
}

SCM_DEFINE(s_valid_material_p, "valid-material?", 1, 0, 0, (SCM id), "") {
	int i = scm_to_int(id);
	return i >= 0 ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_DEFINE(s_material_name, "material-name", 1, 0, 0, (SCM id), "") {
	material_ref ref = { scm_to_int(id) };
	return scm_from_locale_string(material_name(ref));
}

SCM_DEFINE(s_material_textures, "material-has-textures?", 1, 0, 0, (SCM id), "") {
	material_ref ref = { scm_to_int(id) };
	return material_textures(ref) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_DEFINE(s_material_add_texture, "material-add-texture", 2, 0, 0, (SCM mat, SCM tex), "") {
    material_ref m = { scm_to_int(mat) };
    texture_ref t = { scm_to_int(tex) };
    material_add_texture(m, t);
    return SCM_BOOL_T;
}

void register_scheme_functions_for_material() {
#include "material.x"
}

#endif

