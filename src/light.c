#include "light.h"

#include "scene.h"
#include "drawelement.h"
#include "stock-shader.h"

#include <libmcm/camera-matrices.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct light {
	char *name;
	drawelement_ref deferred_drawelement,
					representation;	//!< the representation is what is drawn in the geometry pass.

	matrix4x4f *trafo;
	struct light_uniform_handler_node *uniform_handlers;
	unsigned int type;	//!< not all light types have to use \c aux.
	void *aux;
};

#include <libcgl/mm.h>
define_mm(light, lights, light_ref);
#include "light.xx"

//! this is an interface for derived light types, not to be used directly.
light_ref make_light(const char *name) {
	light_ref ref = allocate_light_ref();
	struct light *light = lights+ref.id;

	light->name = strdup(name);
	light->trafo = 0;
	light->deferred_drawelement.id = -1;
	light->representation.id = -1;
	light->uniform_handlers = 0;
	light->type = wrong_light_t;
	light->aux = 0;

	return ref;
}

#define referred_light (lights+ref.id)

void light_use_deferred_drawelement(light_ref ref, drawelement_ref de) {
	referred_light->deferred_drawelement = de;
}

drawelement_ref light_deferred_drawelement(light_ref ref) {
	return referred_light->deferred_drawelement;
}

void light_use_as_representation(light_ref ref, drawelement_ref de) {
	referred_light->representation = de;
}

drawelement_ref light_as_representation(light_ref ref) {
	return referred_light->representation;
}

matrix4x4f* light_trafo(light_ref ref) {
	return referred_light->trafo;
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
	light->trafo = mat;
}

struct light_uniform_handler_node* light_uniform_handler(light_ref ref) {
	return referred_light->uniform_handlers;
}

void add_light_uniform_handler(light_ref ref, bool (*handler)(light_ref *, const char *, int)) {
	struct light_uniform_handler_node *new = malloc(sizeof(struct light_uniform_handler_node));
	new->next = referred_light->uniform_handlers;
	new->handler = (uniform_setter_t)handler;
	referred_light->uniform_handlers = new;
}


void apply_deferred_lights(struct light_list *lights) {
	glClearColor(0,0,0,0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glEnable(GL_BLEND);
	glBlendFunc(GL_ONE, GL_ONE);
	// copy depth?
	glDisable(GL_DEPTH_TEST);
	while (lights) {
		drawelement_ref de = light_deferred_drawelement(lights->ref);
		render_drawelement(de);
		lights = lights->next;
	}
	glEnable(GL_DEPTH_TEST);
	glDisable(GL_BLEND);
}


#define looking_for(Y) (strcmp(uniform, Y) == 0)
bool stock_spotlight_uniform_handler(light_ref *ref, const char *uniform, int location) {
	if (looking_for("spot_pos")) {
		vec3f pos;
		matrix4x4f *mat = light_trafo(*ref);
		extract_pos_vec3f_of_matrix(&pos, mat);
		glUniform3fv(location, 1, (float*)&pos);
	}
	else if (looking_for("spot_dir")) {
		vec3f dir;
		matrix4x4f *mat = light_trafo(*ref);
		extract_dir_vec3f_of_matrix(&dir, mat);
		glUniform3fv(location, 1, (float*)&dir);
	}
	else if (looking_for("spot_col")) {
		glUniform3f(location, 1,1,1);
	}
	else if (looking_for("spot_cos_cutoff")) {
		glUniform1f(location, cosf(*(float*)light_aux(*ref)));
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
light_ref make_spotlight(const char *name, framebuffer_ref gbuffer, 
                         vec3f *pos, vec3f *dir, vec3f *up, float cutoff) {
	light_ref ref = make_light(name);
	float *aux = malloc(sizeof(float));
	*aux = cutoff;
	set_light_aux(ref, spot_light_t, aux);
	add_light_uniform_handler(ref, stock_spotlight_uniform_handler);
	
	drawelement_ref deferred = make_stock_gbuffer_default_drawelement(gbuffer, name, stock_effect_spot());
	matrix4x4f *matrix = malloc(sizeof(matrix4x4f));
	make_lookat_matrixf(matrix, pos, dir, up);

	return ref;
}

