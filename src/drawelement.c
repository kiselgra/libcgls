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
}

// uniform handlers

// bool de

void render_drawelement(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;

	bind_shader(de->shader);

	bind_mesh_to_gl(de->mesh);
	draw_mesh(de->mesh, GL_TRIANGLES);
	unbind_mesh_from_gl(de->mesh);

	unbind_shader(de->shader);
}
