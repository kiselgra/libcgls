#ifndef __CGLS_SCENE_H__ 
#define __CGLS_SCENE_H__ 

#include "drawelement.h"

/*! a very simple scene definition.
 *
 * the basic mechanism is as follows:
 * - a scene is constructed by using its `inserter'
 * - a scene is rendered by using its `traverser'
 *
 * the basic scene just collects all the drawelements in a list and iterates it
 * on traversal.
 *
 * a scene traverser can be changed dynamically and (as long as it is
 * compatible to the customly built scene data structure, or uses the default
 * structure) can be used to implement different types of rendering, e.g.
 * rendering using per submesh materials for display as opposed to rendering
 * the whole mesh in a single bulk for shadowmapping (as is possible with the
 * graph_scene).
 *
 * \note when constructing a scene via a custom inserter, make sure to call the
 * default inserter, too. this is to ensure a consistend base layout.
 * 
 * \note internally we provide means to create multiple scene objects (as usual),
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
void graph_scene_traverser(scene_ref ref);

/*! traverses the toplevel nodes (differend models/scenes) of the scene.
 *  does not bind and shaders, this will have to be done before calling \ref render_scene.
 */
void graph_scene_bulk_traverser(scene_ref ref);

scene_ref make_graph_scene(const char *name);

define_slist(graph_scene_bulk_de_list, drawelement_ref ref);
void free_graph_scene_bulk_de_list(struct graph_scene_bulk_de_list *list);
struct graph_scene_bulk_de_list* graph_scene_bulk_drawelements(scene_ref ref);

#ifdef __cplusplus
}
#endif

#endif

