#include "light.h"

#include "scene.h"
#include "material.h"
#include "drawelement.h"
#include "stock-shader.h"

#include <libmcm/camera-matrices.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

struct light {
	char *name;
	drawelement_ref deferred_drawelement,
					representation;	//!< the representation is what is drawn in the geometry pass.

	vec3f color;
	matrix4x4f *trafo;
	bool allocated_trafo;
	struct light_uniform_handler_node *uniform_handlers;
	bool on;
	bool render_representaion_if_off;
	bool change_representation_color_if_off;
	unsigned int type;	//!< not all light types have to use \c aux.
	void *aux;
};

#include <libcgl/mm.h>
define_mm(light, lights, light_ref);
#include "light.xx"

bool basic_light_uniform_handler(light_ref *ref, const char *uniform, int location);

//! this is an interface for derived light types, not to be used directly.
light_ref make_light(const char *name) {
	light_ref ref = allocate_light_ref();
	struct light *light = lights+ref.id;

	light->name = strdup(name);
	light->trafo = malloc(sizeof(matrix4x4f));
	make_unit_matrix4x4f(light->trafo);
	light->allocated_trafo = true;
	light->deferred_drawelement.id = -1;
	light->representation.id = -1;
	light->uniform_handlers = 0;
	light->type = wrong_light_t;
	light->aux = 0;
	light->color.x = light->color.y = light->color.z = 1;
	light->on = true;
	light->render_representaion_if_off = true;
	light->change_representation_color_if_off = true;

	add_light_uniform_handler(ref, basic_light_uniform_handler);

	return ref;
}

#define referred_light (lights+ref.id)

const char* light_name(light_ref ref) {
	return referred_light->name;
}

bool light_is_on(light_ref ref) { return referred_light->on; }
void light_on(light_ref ref)    { referred_light->on = true; }
void light_off(light_ref ref)   { referred_light->on = false; }

bool show_light_representation_if_off(light_ref ref)              { return referred_light->render_representaion_if_off; }
void light_representation_mode_if_off(light_ref ref, bool render) { referred_light->render_representaion_if_off = render; }

bool dim_light_representation_if_off(light_ref ref)                   { return referred_light->change_representation_color_if_off; }
void light_representation_dim_mode_if_off(light_ref ref, bool render) { referred_light->change_representation_color_if_off = render; }

void light_use_deferred_drawelement(light_ref ref, drawelement_ref de) {
	referred_light->deferred_drawelement = de;
}

drawelement_ref light_deferred_drawelement(light_ref ref) {
	return referred_light->deferred_drawelement;
}

void light_use_as_representation(light_ref ref, drawelement_ref de) {
	referred_light->representation = de;
}

drawelement_ref light_representation(light_ref ref) {
	return referred_light->representation;
}

matrix4x4f* light_trafo(light_ref ref) {
	return referred_light->trafo;
}

void change_light_color3f(light_ref ref, float r, float g, float b) {
	referred_light->color.x = r;
	referred_light->color.y = g;
	referred_light->color.z = b;

	if (valid_drawelement_ref(referred_light->representation)) {
		material_ref mat = drawelement_material(referred_light->representation);
		if (valid_material_ref(mat)) {
			material_ambient_color(mat)->x = r;
			material_ambient_color(mat)->y = g;
			material_ambient_color(mat)->z = b;
		}
	}
}

void change_light_color(light_ref ref, vec3f *c) {
	change_light_color3f(ref, c->x, c->y, c->z);
}

vec3f* light_color(light_ref ref) {
	return &referred_light->color;
}

void* light_aux(light_ref ref) {
	return referred_light->aux;
}

unsigned int light_type(light_ref ref) {
	return referred_light->type;
}

void set_light_aux(light_ref ref, unsigned int new_type, void *p) {
	referred_light->aux = p;
	referred_light->type = new_type;
}

//! \attention the pointer will be kept!
void replace_light_trafo(light_ref ref, matrix4x4f *mat) {
	struct light *light = lights+ref.id;
	if (light->allocated_trafo) {
		free(light->trafo);
		light->allocated_trafo = false;
	}
	light->trafo = mat;
}

struct light_uniform_handler_node* light_uniform_handlers(light_ref ref) {
	return referred_light->uniform_handlers;
}

void add_light_uniform_handler(light_ref ref, bool (*handler)(light_ref *, const char *, int)) {
	struct light_uniform_handler_node *new = malloc(sizeof(struct light_uniform_handler_node));
	new->next = referred_light->uniform_handlers;
	new->handler = (uniform_setter_t)handler;
	referred_light->uniform_handlers = new;
}

light_ref find_light_by_representation(drawelement_ref rep) {
	light_ref ref = { -1 };
	if (valid_drawelement_ref(rep))
		for (int i = 0; i < next_light_index; ++i)
			if (lights[i].representation.id == rep.id) {
				ref.id = i;
				break;
			}
	return ref;
}

/*! \brief You can call this to add another light after the fact. 
 *  \note You'll have to prepare the gl state.
 *	\note Deliberately ignores \ref on, so you can add a light as `off' to the scene, do your own processing, and still render the final step using this function.
 */
void apply_single_deferred_light(light_ref ref) {
		for (struct light_uniform_handler_node *node = light_uniform_handlers(ref); node; node = node->next)
			push_global_uniform_handler(&ref, node->handler);
		drawelement_ref de = light_deferred_drawelement(ref);
		render_drawelement(de);
		for (struct light_uniform_handler_node *node = light_uniform_handlers(ref); node; node = node->next)
			pop_global_uniform_handler();
}

void apply_deferred_lights(framebuffer_ref gbuffer, struct light_list *lights) {
// 	glClearColor(0.1,0.1,0.4,0);
	glClearColor(cgls_scene_clear_color.x, cgls_scene_clear_color.y, cgls_scene_clear_color.z, cgls_scene_clear_color.w);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
// 	render_drawelement(stock_deferred_copydepth);
	copy_gbuffer_depth(gbuffer);
	glEnable(GL_BLEND);
	glBlendFunc(GL_ONE, GL_ONE);
	glDisable(GL_DEPTH_TEST);
	while (lights) {
		if (light_is_on(lights->ref))
			apply_single_deferred_light(lights->ref);
		lights = lights->next;
	}
	glEnable(GL_DEPTH_TEST);
	glDisable(GL_BLEND);
}

struct stock_forward_light_uniforms {
	int hemi_lights;
	vec3f *hemi_dir, *hemi_col;
	int spot_lights;
	vec3f *spot_dir, *spot_col, *spot_pos;
	float *spot_cos_cutoff;
};
// we only have one.
static struct stock_forward_light_uniforms stock_forward_light_uniform_setup;

static bool stock_forward_shading_uniform_handler(struct stock_forward_light_uniforms *setup, const char *uniform, int location) {
	if (strcmp(uniform, "hemi_dir") == 0)               glUniform3fv(location, setup->hemi_lights, (float*)setup->hemi_dir);
	else if (strcmp(uniform, "hemi_col") == 0)          glUniform3fv(location, setup->hemi_lights, (float*)setup->hemi_col);
	else if (strcmp(uniform, "hemi_lights") == 0)       glUniform1i(location, setup->hemi_lights);
	else if (strcmp(uniform, "spot_dir") == 0)          glUniform3fv(location, setup->spot_lights, (float*)setup->spot_dir);
	else if (strcmp(uniform, "spot_pos") == 0)          glUniform3fv(location, setup->spot_lights, (float*)setup->spot_pos);
	else if (strcmp(uniform, "spot_col") == 0)          glUniform3fv(location, setup->spot_lights, (float*)setup->spot_col);
	else if (strcmp(uniform, "spot_cos_cutoff") == 0)   glUniform1fv(location, setup->spot_lights, (float*)setup->spot_cos_cutoff);
	else if (strcmp(uniform, "spot_lights") == 0)       glUniform1i(location, setup->spot_lights);
	else
		return false;
	return true;
}

void stock_forward_shading_light_setup(struct light_list *lights) {
	struct stock_forward_light_uniforms *setup = &stock_forward_light_uniform_setup;
	setup->hemi_lights = 0;
	
	// count light types
	for (struct light_list *run = lights; run; run = run->next) {
		enum built_in_light_types type = light_type(run->ref);
		if (type == hemi_light_t)
			setup->hemi_lights++;
		else if (type == spot_light_t)
			setup->spot_lights++;
	}
	if (setup->hemi_lights > 4) {
		fprintf(stderr, "The stock shading system is only configured to use up to 4 hemi lights. The excess ones will be ignored.\n");
	}
	if (setup->hemi_lights > 16) {
		fprintf(stderr, "The stock shading system is only configured to use up to 16 spot lights. The excess ones will be ignored.\n");
	}
	setup->hemi_dir = malloc(sizeof(vec3f)*setup->hemi_lights);
	setup->hemi_col = malloc(sizeof(vec3f)*setup->hemi_lights);
	setup->hemi_lights = 0;
	setup->spot_dir = malloc(sizeof(vec3f)*setup->spot_lights);
	setup->spot_col = malloc(sizeof(vec3f)*setup->spot_lights);
	setup->spot_pos = malloc(sizeof(vec3f)*setup->spot_lights);
	setup->spot_cos_cutoff = malloc(sizeof(float)*setup->spot_lights);
	setup->spot_lights = 0;
	
	matrix4x4f *view = gl_view_matrix_of_cam(current_camera());
	matrix4x4f *norm = gl_normal_matrix_for_view_of(current_camera());

	for (struct light_list *run = lights; run; run = run->next) {
		enum built_in_light_types type = light_type(run->ref);
		if (type == hemi_light_t && setup->hemi_lights < 4) {
			vec3f *dir = (vec3f*)light_aux(run->ref);
			vec4f in = { dir->x, dir->y, dir->z, 0 }, res;
			multiply_matrix4x4f_vec4f(&res, view, &in);
			setup->hemi_dir[setup->hemi_lights].x = res.x;
			setup->hemi_dir[setup->hemi_lights].y = res.y;
			setup->hemi_dir[setup->hemi_lights].z = res.z;
			setup->hemi_col[setup->hemi_lights] = *light_color(run->ref);
			setup->hemi_lights++;
		}
		else if (type == spot_light_t && setup->spot_lights < 16) {
			vec3f dir, pos;
			extract_dir_vec3f_of_matrix(&dir, light_trafo(run->ref));
			extract_pos_vec3f_of_matrix(&pos, light_trafo(run->ref));
			{
				vec4f in = { dir.x, dir.y, dir.z, 0 }, res;
				multiply_matrix4x4f_vec4f(&res, view, &in);
				setup->spot_dir[setup->spot_lights].x = dir.x = res.x;
				setup->spot_dir[setup->spot_lights].y = dir.y = res.y;
				setup->spot_dir[setup->spot_lights].z = dir.z = res.z;
			} {
				vec4f in = { pos.x, pos.y, pos.z, 1 }, res;
				multiply_matrix4x4f_vec4f(&res, view, &in);
				setup->spot_pos[setup->spot_lights].x = pos.x = res.x;
				setup->spot_pos[setup->spot_lights].y = pos.y = res.y;
				setup->spot_pos[setup->spot_lights].z = pos.z = res.z;
			}
			setup->spot_col[setup->spot_lights] = *light_color(run->ref);
			setup->spot_cos_cutoff[setup->spot_lights] = cosf(*(float*)light_aux(run->ref));
			setup->spot_lights++;
		}
	}
	push_global_uniform_handler(setup, (uniform_setter_t)stock_forward_shading_uniform_handler);
}

void stock_forward_shading_light_cleanup(struct light_list *lights) {
	struct stock_forward_light_uniforms *popped = (struct stock_forward_light_uniforms*)pop_global_uniform_handler();
	if (popped != &stock_forward_light_uniform_setup)
		fprintf(stderr, "ERROR: we pushed the global uniform handler for forwad shading, but popped something else.\nYour uniform stack is messed up!\n");
}

//! we provide this just because of the light on/off logic (and the global uniform handler)
void render_light_representation(light_ref ref) {
	drawelement_ref repr = light_representation(ref);
	if (valid_drawelement_ref(repr))
		if (light_is_on(ref) || show_light_representation_if_off(ref)) {
			for (struct light_uniform_handler_node *node = light_uniform_handlers(ref); node; node = node->next)
				push_global_uniform_handler(&ref, node->handler);
			render_drawelement(repr);
			for (struct light_uniform_handler_node *node = light_uniform_handlers(ref); node; node = node->next)
				pop_global_uniform_handler();
		}
}
void render_light_representation_with_shader(light_ref ref, shader_ref shader, uniform_setter_t handler) {
	drawelement_ref repr = light_representation(ref);
	if (valid_drawelement_ref(repr))
		if (light_is_on(ref) || show_light_representation_if_off(ref)) {
			for (struct light_uniform_handler_node *node = light_uniform_handlers(ref); node; node = node->next)
				push_global_uniform_handler(&ref, node->handler);
			if (handler) prepend_drawelement_uniform_handler(repr, handler);
			render_drawelement_with_shader(repr, shader);
			if (handler) pop_drawelement_uniform_handler(repr);
			for (struct light_uniform_handler_node *node = light_uniform_handlers(ref); node; node = node->next)
				pop_global_uniform_handler();
		}
}

#define looking_for(Y) (strcmp(uniform, Y) == 0)
#define transform_pos_to_eyespace(V) \
	matrix4x4f *view = gl_view_matrix_of_cam(current_camera()); \
	vec4f in = { (V).x, (V).y, (V).z, 1 }, res; \
	multiply_matrix4x4f_vec4f(&res, view, &in); \
	(V).x = res.x; (V).y = res.y; (V).z = res.z;
#define transform_dir_to_eyespace(V) \
	matrix4x4f *view = gl_normal_matrix_for_view_of(current_camera()); \
	vec4f in = { (V).x, (V).y, (V).z, 0 }, res; \
	multiply_matrix4x4f_vec4f(&res, view, &in); \
	(V).x = res.x; (V).y = res.y; (V).z = res.z;

bool basic_light_uniform_handler(light_ref *ref, const char *uniform, int location) {
	if (looking_for("light_col")) {
		vec3f col = *light_color(*ref);
		if (!light_is_on(*ref) && dim_light_representation_if_off(*ref))
			mul_vec3f_by_scalar(&col, &col, .2);
		glUniform3fv(location, 1, (float*)&col);
		vec3f v = *light_color(*ref);
	}
	else if (looking_for("light_pos")) {
		vec3f v;
		extract_pos_vec3f_of_matrix(&v, light_trafo(*ref));
		transform_pos_to_eyespace(v);
		glUniform3fv(location, 1, (float*)&v);
	}
	else if (looking_for("light_dir")) {
		vec3f v;
		extract_dir_vec3f_of_matrix(&v, light_trafo(*ref));
		transform_dir_to_eyespace(v);
		glUniform3fv(location, 1, (float*)&v);
	}
	else if (looking_for("light_up")) {
		vec3f v;
		extract_up_vec3f_of_matrix(&v, light_trafo(*ref));
		transform_dir_to_eyespace(v);
		glUniform3fv(location, 1, (float*)&v);
	}
	else 
		return false;
	return true;
}

bool stock_spotlight_uniform_handler(light_ref *ref, const char *uniform, int location) {
	if (looking_for("spot_cos_cutoff")) {
		glUniform1f(location, cosf(*(float*)light_aux(*ref)));
	}
	else
		return false;
	return true;

}

bool stock_hemilight_uniform_handler(light_ref *ref, const char *uniform, int location) {
	if (looking_for("hemi_dir")) {
		matrix4x4f *view = gl_view_matrix_of_cam(current_camera());
		vec3f *dir = (vec3f*)light_aux(*ref);
		vec4f in = { dir->x, dir->y, dir->z, 0 }, res;
		multiply_matrix4x4f_vec4f(&res, view, &in);
		glUniform3fv(location, 1, (float*)&res);
	}
	else if (looking_for("hemi_col")) {
		glUniform3f(location, 1,1,1);
	}
	else
		return false;
	return true;
}
#undef looking_for

light_ref make_headmounted_spotlight(const char *name, framebuffer_ref gbuffer, float cutoff) {
	light_ref ref = make_light(name);
	float *aux = malloc(sizeof(float));
	*aux = cutoff;
	set_light_aux(ref, spot_light_t, aux);
	add_light_uniform_handler(ref, stock_spotlight_uniform_handler);
	
	drawelement_ref deferred = make_stock_gbuffer_default_drawelement(gbuffer, name, stock_effect_headmounted_spot());
	light_use_deferred_drawelement(ref, deferred);

	return ref;
}

drawelement_ref build_spot_light_representation_drawelement(const char *lightname, light_ref ref, float size_scale, float cutoff) {
	char *n = strappend("material for repr of spotlight ", lightname);
	vec3f null = { 0,0,0 };
	material_ref mat = make_material3f(n, light_color(ref), &null, &null);
	matrix4x4f scale; vec3f v = { size_scale, size_scale, (1 + fabs(cos(cutoff*M_PI/180.0))) * size_scale };
	make_scale_matrix4x4f(&scale, &v);
	mesh_ref mesh = make_cylinder(lightname, 31, &scale);
	
	shader_ref shader = { -1 };
	drawelement_ref rep = make_drawelement(lightname, mesh, shader, mat);
	
	// get stock shader
	struct stockshader_fragments ssf;
	init_stockshader_fragments(&ssf);
	stockshader_add_uniform(&ssf, "light_col");
	// remove fragment code and add new fragment code
	stockshader_clear_fsource(&ssf);
	// go on
	char *n2 = strappend("shader for lightrep of ", lightname);
	shader = make_stock_shader(n2, rep, &ssf, true, stock_light_representation_shader());
	drawelement_change_shader(rep, shader);
	prepend_drawelement_uniform_handler(rep, (uniform_setter_t)default_matrix_uniform_handler);
	prepend_drawelement_uniform_handler(rep, (uniform_setter_t)default_material_uniform_handler);
	free(n);
	free(n2);
	return rep;
}

light_ref make_spotlight(const char *name, framebuffer_ref gbuffer, 
                         vec3f *pos, vec3f *dir, vec3f *up, float cutoff) {
	light_ref ref = make_light(name);
	float *aux = malloc(sizeof(float));
	*aux = cutoff*M_PI/180.0f;
	set_light_aux(ref, spot_light_t, aux);
	add_light_uniform_handler(ref, stock_spotlight_uniform_handler);
	
	drawelement_ref deferred = make_stock_gbuffer_default_drawelement(gbuffer, name, stock_effect_spot());
	light_use_deferred_drawelement(ref, deferred);
	add_shader_uniform(drawelement_shader(deferred), "light_pos");
	add_shader_uniform(drawelement_shader(deferred), "light_dir");
	add_shader_uniform(drawelement_shader(deferred), "light_col");
	add_shader_uniform(drawelement_shader(deferred), "spot_cos_cutoff");

	drawelement_ref rep = build_spot_light_representation_drawelement(name, ref, 10, cutoff);

	make_lookat_matrixf(light_trafo(ref), pos, dir, up);
	replace_drawelement_trafo(rep, light_trafo(ref));
	light_use_as_representation(ref, rep);

	return ref;
}

light_ref make_spotlight_from_camera(const char *name, framebuffer_ref gbuffer, camera_ref cam) {
	light_ref ref = make_light(name);
	float cutoff = camera_fovy(cam);
	float *aux = malloc(sizeof(float));
	*aux = cutoff*M_PI/180.0f;
	set_light_aux(ref, spot_light_t, aux);
	add_light_uniform_handler(ref, stock_spotlight_uniform_handler);
	
	drawelement_ref deferred = make_stock_gbuffer_default_drawelement(gbuffer, name, stock_effect_spot());
	light_use_deferred_drawelement(ref, deferred);
	add_shader_uniform(drawelement_shader(deferred), "light_pos");
	add_shader_uniform(drawelement_shader(deferred), "light_dir");
	add_shader_uniform(drawelement_shader(deferred), "light_col");
	add_shader_uniform(drawelement_shader(deferred), "spot_cos_cutoff");

	drawelement_ref rep = build_spot_light_representation_drawelement(name, ref, 10, cutoff);

	replace_light_trafo(ref, lookat_matrix_of_cam(cam));
	replace_drawelement_trafo(rep, lookat_matrix_of_cam(cam));
	light_use_as_representation(ref, rep);

	return ref;
}

drawelement_ref build_rectangular_light_representation_drawelement(const char *lightname, light_ref ref, float size_scale_x, float size_scale_y) {
	char *n = strappend("material for repr of rectlight ", lightname);
	vec3f null = { 0,0,0 };
	material_ref mat = make_material3f(n, light_color(ref), &null, &null);
	matrix4x4f scale; vec3f v = { size_scale_x, size_scale_y, 1 };
	make_scale_matrix4x4f(&scale, &v);
	mesh_ref mesh = make_quad(lightname, &scale);
	
	shader_ref shader = { -1 };
	drawelement_ref rep = make_drawelement(lightname, mesh, shader, mat);
	
	// get stock shader
	struct stockshader_fragments ssf;
	init_stockshader_fragments(&ssf);
	stockshader_add_uniform(&ssf, "light_col");
	// remove fragment code and add new fragment code
	stockshader_clear_fsource(&ssf);
	// go on
	char *n2 = strappend("shader for lightrep of ", lightname);
	shader = make_stock_shader(n2, rep, &ssf, true, stock_light_representation_shader());
	drawelement_change_shader(rep, shader);
	prepend_drawelement_uniform_handler(rep, (uniform_setter_t)default_matrix_uniform_handler);
	prepend_drawelement_uniform_handler(rep, (uniform_setter_t)default_material_uniform_handler);
	free(n);
	free(n2);
	return rep;
}

light_ref make_rectangular_light(const char *name, framebuffer_ref gbuffer, 
                                 vec3f *pos, vec3f *dir, vec3f *up, float width, float height) {
	light_ref ref = make_light(name);
	float *aux = malloc(sizeof(float)*2);
	aux[0] = width;
	aux[1] = height;
	set_light_aux(ref, rect_light_t, aux);
	add_light_uniform_handler(ref, stock_spotlight_uniform_handler);
	
	drawelement_ref deferred = make_stock_gbuffer_default_drawelement(gbuffer, name, stock_effect_spot());
	light_use_deferred_drawelement(ref, deferred);
	add_shader_uniform(drawelement_shader(deferred), "light_pos");
	add_shader_uniform(drawelement_shader(deferred), "light_dir");
	add_shader_uniform(drawelement_shader(deferred), "light_col");
	add_shader_uniform(drawelement_shader(deferred), "spot_cos_cutoff");

	drawelement_ref rep = build_rectangular_light_representation_drawelement(name, ref, width, height);

	make_lookat_matrixf(light_trafo(ref), pos, dir, up);
	replace_drawelement_trafo(rep, light_trafo(ref));
	light_use_as_representation(ref, rep);

	return ref;
}

light_ref make_hemispherical_light(const char *name, framebuffer_ref gbuffer, vec3f *up) {
	light_ref ref = make_light(name);
	vec3f *aux = malloc(sizeof(vec3f));
	*aux = *up;
	set_light_aux(ref, hemi_light_t, aux);
	add_light_uniform_handler(ref, stock_hemilight_uniform_handler);

	drawelement_ref deferred = make_stock_gbuffer_default_drawelement(gbuffer, name, stock_effect_hemisphere_lighting());
	add_shader_uniform(drawelement_shader(deferred), "light_up");
	add_shader_uniform(drawelement_shader(deferred), "light_col");
	matrix4x4f *trafo = light_trafo(ref);
	trafo->col_major[4] = up->x;
	trafo->col_major[5] = up->y;
	trafo->col_major[6] = up->z;
	
	light_use_deferred_drawelement(ref, deferred);

	return ref;
}

light_ref make_ambient_light(const char *name, framebuffer_ref gbuffer, vec3f *color) {
	light_ref ref = make_light(name);
	change_light_color(ref, color);
	drawelement_ref deferred = make_stock_gbuffer_default_drawelement(gbuffer, name, stock_effect_ambient_light());
	add_shader_uniform(drawelement_shader(deferred), "light_col");
	light_use_deferred_drawelement(ref, deferred);
	return ref;
}

light_ref find_light(const char *name) {
	light_ref ref = { -1 };
	if (strlen(name) == 0) return ref;
    for (int i = 0; i < next_light_index; ++i) {
        if (strcmp(lights[i].name, name) == 0) {
			ref.id = i;
			return ref;
		}
	}
	return ref;
}


#ifdef WITH_GUILE

SCM_DEFINE(s_find_light, "find-light", 1, 0, 0, (SCM name), "") {
    char *n = scm_to_locale_string(name);
    light_ref ref = find_light(n);
    free(n);
    return scm_from_int(ref.id);
}

SCM_DEFINE(s_make_hemi_light, "make-hemispherical-light", 3, 0, 0, (SCM name, SCM gbuffer, SCM up), "") {
	char *n = scm_to_locale_string(name);
	framebuffer_ref gb = { scm_to_int(gbuffer) };
	vec3f u = scm_vec_to_vec3f(up);
	light_ref l = make_hemispherical_light(n, gb, &u);
	free(n);
	return scm_from_int(l.id);
}

SCM_DEFINE(s_make_spot_from_cam, "make-spotlight-from-camera", 3, 0, 0, (SCM name, SCM gbuffer, SCM cam), "") {
	char *n = scm_to_locale_string(name);
	framebuffer_ref gb = { scm_to_int(gbuffer) };
	camera_ref c = { scm_to_int(cam) };
	light_ref l = make_spotlight_from_camera(n, gb, c);
	free(n);
	return scm_from_int(l.id);
}

SCM_DEFINE(s_make_spot_light, "make-spotlight", 6, 0, 0, (SCM name, SCM gbuffer, SCM pos, SCM dir, SCM up, SCM cutoff), "") {
	char *n = scm_to_locale_string(name);
	framebuffer_ref gb = { scm_to_int(gbuffer) };
	vec3f p = scm_vec_to_vec3f(pos);
	vec3f d = scm_vec_to_vec3f(dir);
	vec3f u = scm_vec_to_vec3f(up);
	float co = scm_to_double(cutoff);
	light_ref l = make_spotlight(n, gb, &p, &d, &u, co);
	free(n);
	return scm_from_int(l.id);
}

SCM_DEFINE(s_make_rect_light, "make-rectangular-light", 6, 0, 0, (SCM name, SCM gbuffer, SCM pos, SCM dir, SCM up, SCM width, SCM height), "") {
	char *n = scm_to_locale_string(name);
	framebuffer_ref gb = { scm_to_int(gbuffer) };
	vec3f p = scm_vec_to_vec3f(pos);
	vec3f d = scm_vec_to_vec3f(dir);
	vec3f u = scm_vec_to_vec3f(up);
	float w = scm_to_double(width);
	float h = scm_to_double(height);
	light_ref l = make_rectangular_light(n, gb, &p, &d, &u, w, h);
	free(n);
	return scm_from_int(l.id);
}

SCM_DEFINE(s_light_color, "light-color", 1, 0, 0, (SCM id), "") {
    light_ref ref = { scm_to_int(id) };
	return vec3f_to_list(light_color(ref));
}

SCM_DEFINE(s_light_color_x, "set-light-color!", 2, 0, 0, (SCM id, SCM col), "") {
    light_ref ref = { scm_to_int(id) };
	vec3f c = scm_vec_to_vec3f(col);
	change_light_color(ref, &c);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_light_pos, "light-pos", 1, 0, 0, (SCM id), "") {
	vec3f v;
	light_ref ref = { scm_to_int(id) };
	extract_pos_vec3f_of_matrix(&v, light_trafo(ref));
	return vec3f_to_list(&v);
}

SCM_DEFINE(s_light_dir, "light-dir", 1, 0, 0, (SCM id), "") {
	vec3f v;
	light_ref ref = { scm_to_int(id) };
	extract_dir_vec3f_of_matrix(&v, light_trafo(ref));
	return vec3f_to_list(&v);
}

SCM_DEFINE(s_light_up, "light-up", 1, 0, 0, (SCM id), "") {
	vec3f v;
	light_ref ref = { scm_to_int(id) };
	extract_up_vec3f_of_matrix(&v, light_trafo(ref));
	return vec3f_to_list(&v);
}

void register_scheme_functions_for_light() {
#ifndef SCM_MAGIC_SNARFER
#include "light.x"
	scm_c_eval_string("(define (light-config-for-animation id) (list 'add-node-to-camera-animation 'light-anim (light-pos id) (light-dir id) (light-up id) 't))");
#endif
}


#endif
