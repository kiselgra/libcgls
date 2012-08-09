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

#define TYPE scene
#define ARRAY scenes
#define REF scene_ref
#include <libcgl/mm.h>

scene_ref make_scene(char *name) { // no pun intended.
	scene_ref ref = allocate_ref();
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

