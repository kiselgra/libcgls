#include "material.h"
#include "drawelement.h"

#include <libcgl/libcgl.h>

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
	float shininess;
	struct texture_node *textures_head, *back;
    int textures;
	// blend stuff?
	//! material's default shader.
	shader_ref shader;
	bool use_shader;
	void *aux;
};

// see http://gustedt.wordpress.com/2010/11/29/myth-and-reality-about-inline-in-c99/
extern inline bool equal_material_refs(material_ref a, material_ref b);

#include <libcgl/mm.h>
define_mm(material, materials, material_ref);
#include "material.xx"

material_ref make_material(const char *name, vec4f *amb, vec4f *diff, vec4f *spec) {
	material_ref ref = allocate_material_ref();
	struct material *mat = materials+ref.id;
	
	mat->name = strdup(name);

	mat->k_amb = *amb;
	mat->k_diff = *diff;
	mat->k_spec = *spec;

    mat->textures = 0;
	mat->textures_head = mat->back = 0;

	mat->shader.id -1;
	mat->use_shader = false;

	mat->aux = 0;
	return ref;
}

material_ref make_material3f(const char *name, vec3f *amb, vec3f *diff, vec3f *spec) {
	vec4f a = { amb->x, amb->y, amb->z };
	vec4f d = { diff->x, diff->y, diff->z };
	vec4f s = { spec->x, spec->y, spec->z };
	return make_material(name, &a, &d, &s);
}

void material_set_specular_exponent(material_ref ref, float s) {
	struct material *mat = materials+ref.id;
	mat->shininess = s;
}

float material_specular_exponent(material_ref ref) {
	struct material *mat = materials+ref.id;
	return mat->shininess;
}

material_ref find_material(const char *name) {
	material_ref ref = { -1 };
	if (!name || strlen(name) == 0) return ref;
	for (int i = 0; i < next_material_index; ++i)
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
	return mat->textures_head;
}

int material_number_of_textures(material_ref ref) {
	struct material *mat = materials+ref.id;
	return mat->textures;
}

void material_add_texture_as(material_ref ref, texture_ref tex, const char *name) {
	struct material *mat = materials+ref.id;
	struct texture_node *new_node = malloc(sizeof(struct texture_node));
	int unit = 0;
	if (mat->textures_head == 0)
		mat->textures_head = new_node;
	else {
		mat->back->next = new_node; 
		unit = mat->back->unit + 1;
	}
	mat->back = new_node;
	mat->back->ref = tex;
	mat->back->unit = unit;
	mat->back->name = strdup(name);
	mat->back->next = 0;
    mat->textures++;
}

void material_add_texture(material_ref ref, texture_ref tex) {
	material_add_texture_as(ref, tex, texture_name(tex));
}

void* material_aux(material_ref ref) {
	struct material *mat = materials+ref.id;
	return mat->aux;
}

void material_set_aux(material_ref ref, void *aux) {
	struct material *mat = materials+ref.id;
	mat->aux = aux;
}

// shader "generation"
#include "stock-shader.h"

void material_use_stock_shader(material_ref ref) {
	struct material *mat = materials+ref.id;
	struct texture_node *textures = material_textures(ref);
	bool has_texture_called(const char *name) {
		for (struct texture_node *run = textures; run; run = run->next)
			if (strcmp(run->name, name) == 0)
				return true;
		return false;
	}
	bool ambient_tex  = has_texture_called("ambient_tex");
	bool diffuse_tex  = has_texture_called("diffuse_tex");
	bool specular_tex = has_texture_called("specular_tex");
	bool mask_tex     = has_texture_called("mask_tex");
	mat->use_shader = true;

	struct stockshader_fragments ssf;
	init_stockshader_fragments(&ssf);
	stock_shader(&ssf, ambient_tex, diffuse_tex, specular_tex, mask_tex);
	mat->shader = make_shader("test", stockshader_inputs(&ssf), stockshader_uniforms(&ssf));
	populate_shader_with_fragments(mat->shader, &ssf);
	if (!compile_and_link_shader(mat->shader)) {
		fprintf(stderr, "SHADER ERROR\n");
		if (vertex_shader_info_log(mat->shader))
			fprintf(stderr, "VERTEX-LOG:\n%s\n", vertex_shader_info_log(mat->shader));
		if (fragment_shader_info_log(mat->shader))
			fprintf(stderr, "FRAGMENT-LOG:\n%s\n", fragment_shader_info_log(mat->shader));
		if (shader_info_log(mat->shader))
			fprintf(stderr, "PROGRAM-LOG:\n%s\n", shader_info_log(mat->shader));
	}
}

bool material_has_shader(material_ref ref) {
	struct material *mat = materials+ref.id;
	return valid_shader_ref(mat->shader);
}

void enable_material_shader(material_ref ref) {
	if (material_has_shader(ref)) {
		struct material *mat = materials+ref.id;
		mat->use_shader = true;
	}
}

void disable_material_shader(material_ref ref) {
	if (material_has_shader(ref)) {
		struct material *mat = materials+ref.id;
		mat->use_shader = false;
	}
}

bool material_shader_enabled(material_ref ref) {
	if (!material_has_shader(ref))
		return false;
	struct material *mat = materials+ref.id;
	return mat->use_shader;
}

shader_ref material_shader(material_ref ref) {
	struct material *mat = materials+ref.id;
	return mat->shader;
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
	else if (str_eq(uniform, "shininess"))
		glUniform1f(location, material_specular_exponent(mat));
	else {
		for (struct texture_node *run = material_textures(mat); run; run = run->next)
			if (str_eq(uniform, run->name)) {
				bind_texture(run->ref, run->unit);
                glUniform1i(location, run->unit);
				return true;
			}
        // texN specifically requests the texture bound to unit N, therefore we don't use ...->unit here.
		if (strlen(uniform) == 4 && uniform[3] >= '0' && uniform[3] <= '9' && strncmp(uniform, "tex", 3) == 0) {
			int nr = uniform[3] - '0';
            int i = 0;
			for (struct texture_node *run = material_textures(mat); run; run = run->next)
				if (i == nr) {
					bind_texture(run->ref, nr);
                    glUniform1i(location, nr);
					return true;
				}
                else ++i;
		}
		return false;
	}
	return true;
}

#undef mat
#undef str_eq

#ifdef WITH_GUILE

#include <libguile.h>

#define REF(id) material_ref ref = { scm_to_int(id) };

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
	REF(id);
	return scm_from_locale_string(material_name(ref));
}

SCM_DEFINE(s_material_has_textures, "material-has-textures?", 1, 0, 0, (SCM id), "") {
	REF(id);
	return material_textures(ref) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_DEFINE(s_material_no_of_textures, "material-number-of-textures", 1, 0, 0, (SCM id), "") {
	REF(id);
	return scm_from_int(material_number_of_textures(ref));
}

SCM_DEFINE(s_material_textures, "material-textures", 1, 0, 0, (SCM id), "") {
    SCM list = SCM_EOL;
	REF(id);
    
    for (struct texture_node *run = material_textures(ref); run; run = run->next)
        list = scm_cons(scm_from_int(run->ref.id), list);

    return list;
}

SCM_DEFINE(s_material_add_texture, "material-add-texture", 2, 0, 0, (SCM mat, SCM tex), "") {
    REF(mat);
    texture_ref t = { scm_to_int(tex) };
    material_add_texture(ref, t);
    return SCM_BOOL_T;
}

SCM_DEFINE(s_material_ambient_color, "material-ambient-color", 1, 0, 0, (SCM id), "") {
    REF(id);
    return vec4f_to_scm_vec(material_ambient_color(ref));
}

SCM_DEFINE(s_material_diffuse_color, "material-diffuse-color", 1, 0, 0, (SCM id), "") {
    REF(id);
    return vec4f_to_scm_vec(material_diffuse_color(ref));
}

SCM_DEFINE(s_material_specular_color, "material-specular-color", 1, 0, 0, (SCM id), "") {
    REF(id);
    return vec4f_to_scm_vec(material_specular_color(ref));
}

SCM_DEFINE(s_material_ambient_color_x, "set-material-ambient-color!", 2, 0, 0, (SCM id, SCM col), "") {
    REF(id);
    vec4f color = scm_vec_to_vec4f(col);
    *material_ambient_color(ref) = color;
    return SCM_BOOL_T;
}

SCM_DEFINE(s_material_diffuse_color_x, "set-material-diffuse-color!", 2, 0, 0, (SCM id, SCM col), "") {
    REF(id);
    vec4f color = scm_vec_to_vec4f(col);
    *material_diffuse_color(ref) = color;
    return SCM_BOOL_T;
}

SCM_DEFINE(s_material_specular_color_x, "set-material-specular-color!", 2, 0, 0, (SCM id, SCM col), "") {
    REF(id);
    vec4f color = scm_vec_to_vec4f(col);
    *material_specular_color(ref) = color;
    return SCM_BOOL_T;
}

SCM_DEFINE(s_material_use_stock_shader, "material-use-stock-shader!", 1, 0, 0, (SCM id), "") {
	REF(id);
	material_use_stock_shader(ref);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_mat_shader, "material-shader", 1, 0, 0, (SCM id), "") {
	material_ref ref = { scm_to_int(id) };
	shader_ref sref = material_shader(ref);
	return scm_from_int(sref.id);
}


void register_scheme_functions_for_material() {
#include "material.x"
}

#endif

