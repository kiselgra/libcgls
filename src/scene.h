#ifndef __CGLS_SCENE_H__ 
#define __CGLS_SCENE_H__ 

#include "drawelement.h"

/* a very simple scene definition.
 *
 * note: internally we provide means to create multiple scene objects (as usual),
 * but this is just for our convenience - we're not expecting to be using multiple
 * scenes in one run.
 */

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	int id;
} scene_ref;

typedef void (*scene_traverser_t)(scene_ref);
void default_scene_renderer(scene_ref ref);

typedef void (*scene_drawelement_inserter_t)(scene_ref, drawelement_ref);
void default_scene_drawelement_inserter(scene_ref s, drawelement_ref d);

enum { scene_type_default = 0, scene_type_graph = 1 };

typedef struct drawelement_node {
	struct drawelement_node *next;
	drawelement_ref ref;
} drawelement_node;


scene_ref make_scene(const char *name);
void* scene_aux(scene_ref ref);
unsigned int scene_aux_type(scene_ref ref);
// your aux data structure should have a certain magic number by which you can identify it.
void scene_set_aux(scene_ref ref, unsigned int type, void *aux);
// your inserter should still call the default implementation to ensure consistent data structures.
void scene_set_drawelement_inserter(scene_ref ref, scene_drawelement_inserter_t ins);
scene_drawelement_inserter_t scene_drawelement_inserter(scene_ref ref);
void scene_set_traverser(scene_ref ref, scene_traverser_t trav);
scene_traverser_t scene_traverser(scene_ref ref);
void scene_add_drawelement(scene_ref ref, drawelement_ref de);
drawelement_node* scene_drawelements(scene_ref ref);
void render_scene(scene_ref ref);

	
void default_scene_renderer(scene_ref ref);


scene_ref make_graph_scene(const char *name);

#ifdef __cplusplus
}
#endif

#endif

