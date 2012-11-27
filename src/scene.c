#include "scene.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct scene {
	char *name;
	void *aux;
	scene_traverser_t trav;
	drawelement_node *drawelements;
	drawelement_node *back;
};

#include <libcgl/mm.h>
define_mm(scene, scenes, scene_ref);
#include "scene.xx"

scene_ref make_scene(char *name) { // no pun intended.
	scene_ref ref = allocate_scene_ref();
	if (ref.id != 0)
		fprintf(stderr, "creating more than one scene is not really supported at the moment. good luck.\n");
	struct scene *scene = scenes+ref.id;

	if (!name) name = "default";
	scene->name = malloc(strlen(name)+1);
	strcpy(scene->name, name);

	scene->drawelements = 0;
	scene->aux = 0;
	scene->trav = default_scene_renderer;

	return ref;
}

void* scene_aux(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->aux;
}

void scene_set_aux(scene_ref ref, void *aux) {
	struct scene *scene = scenes+ref.id;
	scene->aux = aux;
}

void scene_set_traverser(scene_ref ref, scene_traverser_t trav) {
	struct scene *scene = scenes+ref.id;
	scene->trav = trav;
}

scene_traverser_t scene_traverser(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->trav;
}

void scene_add_drawelement(scene_ref ref, drawelement_ref de) {
	struct scene *scene = scenes+ref.id;
	drawelement_node *new_node = malloc(sizeof(drawelement_node));
	if (scene->drawelements == 0)
		scene->drawelements = scene->back = new_node;
	else {
		scene->back->next = new_node;
		scene->back = new_node;
	}
	scene->back->ref = de;
	scene->back->next = 0;
}

drawelement_node* scene_drawelements(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->drawelements;
}

void render_scene(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	scene->trav(ref);
}

void default_scene_renderer(scene_ref ref) {
	for (drawelement_node *run = scene_drawelements(ref); run; run = run->next)
		render_drawelement(run->ref);
}

#ifdef WITH_GUILE
#include <libguile.h>

SCM_DEFINE(s_make_scene, "make-scene", 1, 0, 0, (SCM name), "") {
	char *n = scm_to_locale_string(name);
	scene_ref ref = make_scene(n);
	free(n);
	return scm_from_int(ref.id);
}

SCM_DEFINE(s_scene_add_drawelement, "add-drawelement-to-scene", 2, 0, 0, (SCM scene, SCM de), "") {
	scene_ref s = { scm_to_int(scene) };
	drawelement_ref d = { scm_to_int(de) };
	scene_add_drawelement(s, d);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_scene_drawelements, "drawelement-of-scene", 1, 0, 0, (SCM scene), "") {
	scene_ref s = { scm_to_int(scene) };
	struct drawelement_node *node = scene_drawelements(s);
	SCM list = scm_list_1(scm_from_int(node->ref.id));
	while (node->next) {
		node = node->next;
		list = scm_cons(scm_from_int(node->next->ref.id), list);
	}
	return scm_reverse_x(list, SCM_EOL);
}

void register_scheme_functions_for_scene() {
#include "scene.x"
}

#endif
