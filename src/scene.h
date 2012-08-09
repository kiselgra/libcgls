#ifndef __CGLS_SCENE_H__ 
#define __CGLS_SCENE_H__ 

#include "drawelement.h"

/* a very simple scene definition.
 *
 * note: internally we provide means to create multiple scene objects (as usual),
 * but this is just for our convenience - we're not expecting to be using multiple
 * scenes in one run.
 */

typedef struct {
	int id;
} scene_ref;

typedef void (*scene_traverser_t)(scene_ref);
void default_scene_renderer(scene_ref ref);

typedef struct drawelement_node {
	struct drawelement_node *next;
	drawelement_ref ref;
} drawelement_node;


scene_ref make_scene(char *name);
void* scene_aux(scene_ref ref);
void scene_set_aux(scene_ref ref, void *aux);
void scene_set_traverser(scene_ref ref, scene_traverser_t trav);
void scene_add_drawelement(scene_ref ref, drawelement_ref de);
drawelement_node* scene_drawelements(scene_ref ref);
void render_scene(scene_ref ref);

	
void default_scene_renderer(scene_ref ref);

#endif

