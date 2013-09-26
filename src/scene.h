#ifndef __CGLS_SCENE_H__ 
#define __CGLS_SCENE_H__ 

#include "refs.h"

#include "drawelement.h"
#include "light.h"

#include <libmcm/vectors.h>

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

typedef void (*scene_traverser_t)(scene_ref);
void default_scene_renderer(scene_ref ref);

typedef void (*scene_drawelement_inserter_t)(scene_ref, drawelement_ref);
void default_scene_drawelement_inserter(scene_ref s, drawelement_ref d);

typedef void (*scene_light_application_t)(struct light_list *);
//!< a default is supplied in light.c

enum { scene_type_default = 0, scene_type_graph = 1 };

// typedef struct drawelement_node {
// 	struct drawelement_node *next;
// 	drawelement_ref ref;
// } drawelement_node;
typedef struct drawelement_list drawelement_node;


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

void scene_set_lighting(scene_ref ref, scene_light_application_t app);
void add_light_to_scene(scene_ref ref, light_ref light);
bool scene_render_light_representations(scene_ref ref);
void scene_rendering_of_light_representations(scene_ref ref, bool on);

void set_scene_skybox(scene_ref ref, drawelement_ref de);
drawelement_ref scene_skybox(scene_ref ref);
void render_skybox_to_buffer(scene_ref ref, framebuffer_ref gbuffer);

void use_alternate_shaders(scene_ref ref, shader_ref *as, uniform_setter_t extra_handler);

void render_scene_to_gbuffer(scene_ref ref, framebuffer_ref gbuffer);
void render_scene_from_gbuffer(scene_ref ref, framebuffer_ref gbuffer);

void render_scene(scene_ref ref);
void render_scene_deferred(scene_ref ref, framebuffer_ref gbuffer);
void render_scene_to_buffer(scene_ref ref, framebuffer_ref target);
void render_scene_deferred_to_buffer(scene_ref ref, framebuffer_ref gbuffer, framebuffer_ref target);

void render_scene_with_shader(scene_ref ref, shader_ref shader, uniform_setter_t extra_handler);
	
void default_scene_renderer(scene_ref ref);
void graph_scene_traverser(scene_ref ref);

/*! traverses the toplevel nodes (different models/scenes) of the scene.
 *  does not bind shaders, this will have to be done before calling \ref render_scene.
 */
void graph_scene_bulk_traverser(scene_ref ref);

scene_ref make_graph_scene(const char *name);

define_slist(graph_scene_bulk_de_list, drawelement_ref ref);
void free_graph_scene_bulk_de_list(struct graph_scene_bulk_de_list *list);
struct graph_scene_bulk_de_list* graph_scene_bulk_drawelements(scene_ref ref);


extern vec4f cgls_scene_clear_color;



struct custom_shader_fragment {
	struct custom_shader_fragment *next;
	char *name;
	char *source;
	int uniforms;
	char **uniform;
};

single_material_pass_ref make_single_material_pass(const char *name, struct drawelement_list *drawelements, const char *fragment_source, int uniforms, char **uniform, uniform_setter_t extra_handler);
single_material_pass_ref make_single_material_pass_using_array(const char *name, struct drawelement_array *array, const char *fragment_source, int uniforms, char **uniform, uniform_setter_t extra_handler);
single_material_pass_ref make_single_material_pass_from_fragment(const char *name, struct drawelement_list *drawelements, const char *fragment, uniform_setter_t extra_handler);
single_material_pass_ref make_single_material_pass_from_fragment_using_array(const char *name, struct drawelement_array *array, const char *fragment, uniform_setter_t extra_handler);
void add_drawelement_to_single_material_pass(single_material_pass_ref ref, drawelement_ref de);
void finalize_single_material_pass(single_material_pass_ref ref);
void finalize_single_material_passes_for_array(struct drawelement_array *array);
void render_single_material_pass(single_material_pass_ref ref);
void enable_backface_culling_for_material_pass(single_material_pass_ref ref, bool yes);
bool using_backface_culling_for_material_pass(single_material_pass_ref ref);

void register_custom_shader_fragment(const char *name, const char *source, int u, char **U);
struct custom_shader_fragment* find_shader_fragment(const char *name);

define_array(scene);
define_array(single_material_pass);

#ifdef __cplusplus
}
#endif

#endif

