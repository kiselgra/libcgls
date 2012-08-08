#include "drawelement.h"

#include <string.h>
#include <stdlib.h>

#include <stdio.h>

struct handler_node {
	struct handler_node *next;
	uniform_setter_t handler;
};

struct drawelement {
	char *shortname, *name;
	mesh_ref mesh;
	shader_ref shader;
	struct handler_node *handler_chain;
	matrix4x4f trafo;
};

#define TYPE drawelement
#define ARRAY drawelements
#define REF drawelement_ref
#include <libcgl/mm.h>

drawelement_ref make_drawelement(const char *modelname, mesh_ref mr, shader_ref sr) {
	drawelement_ref ref = allocate_ref();
	struct drawelement *de = drawelements+ref.id;

	int meshname_len = strlen(mesh_name(mr));
	de->shortname = malloc(meshname_len+1);
	strcpy(de->shortname, mesh_name(mr));

	if (modelname) {
		int modelname_len = strlen(modelname);
		de->name = malloc(modelname_len + 1 + meshname_len + 1);
		strcpy(de->name, modelname);
		strcpy(de->name+modelname_len, "/");
		strcpy(de->name+modelname_len+1, de->shortname);
	}
	else
		de->name = strdup(de->shortname);
		
	de->mesh = mr;
	de->shader = sr;

	de->handler_chain = 0;
	make_unit_matrix4x4f(&de->trafo);

// 	printf("create drawelement %s.\n", de->name);
// 	printf("       drawelement %s.\n", modelname);
// 	printf("       drawelement %s.\n", mesh_name(mr));
	return ref;
}

matrix4x4f* drawelement_trafo(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;
	return &de->trafo;
}

// uniform handlers

#define str_eq(X, Y) (strcmp(X, Y) == 0)

bool default_matrix_uniform_handler(drawelement_ref ref, const char *uniform, int location) {
	if (str_eq(uniform, "proj"))
		glUniformMatrix4fv(location, 1, GL_FALSE, projection_matrix_of_cam(current_camera())->col_major);
	else if (str_eq(uniform, "view"))
		glUniformMatrix4fv(location, 1, GL_FALSE, gl_view_matrix_of_cam(current_camera())->col_major);
	else if (str_eq(uniform, "normal_matrix"))
		glUniformMatrix4fv(location, 1, GL_FALSE, gl_normal_matrix_for_view_of(current_camera())->col_major);
	else if (str_eq(uniform, "model"))
		glUniformMatrix4fv(location, 1, GL_FALSE, drawelement_trafo(ref)->col_major);
	else
		return false;
	return true;
}

// may also be abused to get called just before the drawelement is rendered. ;)
void prepend_uniform_handler(drawelement_ref ref, uniform_setter_t handler) {
	struct drawelement *de = drawelements+ref.id;
	struct handler_node *cdr = de->handler_chain;
	de->handler_chain = malloc(sizeof(struct handler_node));
	de->handler_chain->handler = handler;
	de->handler_chain->next = cdr;
}

// 

void render_drawelement(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;

	bind_shader(de->shader);

	for (int i = 0; i < shader_uniforms(de->shader); ++i) {
		const char *name = shader_uniform_name_by_id(de->shader, i);
		int loc = shader_uniform_location_by_id(de->shader, i);
		struct handler_node *run = de->handler_chain;
		while (run && !run->handler(ref, name, loc))
			run = run->next;
		if (!run)
			printf("WARNING: cannot find a handler for uniform %s of shader %s when attached to drawelement %s.\n", 
					name, shader_name(de->shader), de->name);
	}

	bind_mesh_to_gl(de->mesh);
	draw_mesh(de->mesh, GL_TRIANGLES);
	unbind_mesh_from_gl(de->mesh);

	unbind_shader(de->shader);
}

drawelement_ref find_drawelement(const char *name) {
	drawelement_ref ref = { -1 };
	if (strlen(name) == 0) return ref;
	for (int i = 0; i < next_index; ++i) {
		if (strcmp(drawelements[i].name, name) == 0) {
			ref.id = i;
			return ref;
		}
	}
	return ref;
}

