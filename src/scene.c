#include "scene.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct scene {
	char *name;
    scene_drawelement_inserter_t inserter;
	scene_traverser_t trav;
	drawelement_node *drawelements;
	drawelement_node *back;
    unsigned int aux_type;
	shader_ref with_shader;  //!< this is the temporary shader for special purpose passes.
	shader_ref *shader;      //!< makes for faster checking.
	uniform_setter_t extra_uniform_handler; //!< valid only when rendering in single-shader mode.
	void *aux;
};

#include <libcgl/mm.h>
define_mm(scene, scenes, scene_ref);
#include "scene.xx"

/*! \defgroup basic_scene
 *  standard, most primitive, scene type
 */

scene_ref make_scene(const char *name) { // no pun intended.
	scene_ref ref = allocate_scene_ref();
	if (ref.id != 0)
		fprintf(stderr, "creating more than one scene is not really supported at the moment. good luck.\n");
	struct scene *scene = scenes+ref.id;

	if (!name) name = "default";
	scene->name = malloc(strlen(name)+1);
	strcpy(scene->name, name);

	scene->drawelements = 0;
	scene->aux = 0;
    scene->inserter = default_scene_drawelement_inserter;
	scene->trav = default_scene_renderer;

	scene->with_shader = make_invalid_shader();
	scene->shader = 0;
	scene->extra_uniform_handler = 0;

	return ref;
}

void* scene_aux(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->aux;
}

unsigned int scene_aux_type(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->aux_type;
}

void scene_set_aux(scene_ref ref, unsigned int type, void *aux) {
	struct scene *scene = scenes+ref.id;
    scene->aux_type = type;
	scene->aux = aux;
}

void scene_set_drawelement_inserter(scene_ref ref, scene_drawelement_inserter_t ins) {
	struct scene *scene = scenes+ref.id;
	scene->inserter = ins;
}

scene_drawelement_inserter_t scene_drawelement_inserter(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->inserter;
}

void scene_set_traverser(scene_ref ref, scene_traverser_t trav) {
	struct scene *scene = scenes+ref.id;
	scene->trav = trav;
}

scene_traverser_t scene_traverser(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->trav;
}

bool use_single_shader_for_scene(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->shader != 0;
}

shader_ref single_shader_for_scene(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->with_shader;
}

uniform_setter_t single_shader_extra_uniform_handler(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->extra_uniform_handler;
}

void default_scene_drawelement_inserter(scene_ref ref, drawelement_ref de) {
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

void scene_add_drawelement(scene_ref ref, drawelement_ref de) {
	struct scene *scene = scenes+ref.id;
    scene->inserter(ref, de);
}

drawelement_node* scene_drawelements(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->drawelements;
}

void render_scene(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	scene->trav(ref);
}

void render_scene_with_shader(scene_ref ref, shader_ref shader, uniform_setter_t extra_handler) {
	struct scene *scene = scenes+ref.id;
	scene->with_shader = shader;
	scene->shader = &scene->with_shader;
	scene->extra_uniform_handler = extra_handler;
	scene->trav(ref);
	scene->shader = 0;
	scene->with_shader = make_invalid_shader();
	scene->extra_uniform_handler = 0;
}

void default_scene_renderer(scene_ref ref) {
	if (use_single_shader_for_scene(ref)) {
		shader_ref shader = single_shader_for_scene(ref);
		for (drawelement_node *run = scene_drawelements(ref); run; run = run->next) {
			prepend_drawelement_uniform_handler(run->ref, single_shader_extra_uniform_handler(ref));
			render_drawelement_with_shader(run->ref, shader);
			pop_drawelement_uniform_handler(run->ref);
		}
	}
	else
		for (drawelement_node *run = scene_drawelements(ref); run; run = run->next)
			render_drawelement(run->ref);
}

/*! \defgroup graph_scene
 *  first level vbo, second level material.
 */

//! internal \ref graph_scene structure. \ingroup graph_scene
struct by_material {
    material_ref mat;
    struct drawelement_node *drawelements;
    struct by_material *next;
};

struct by_mesh {
    mesh_ref mesh;
    struct by_material *materials;
    struct by_mesh *next;
	drawelement_ref bulk_de;
};

struct graph_scene_aux {
    struct by_mesh *meshes;
};

/*! add a drawelement to the scene.
 *
 * 	\note for the bulk-rendering this procedure constructs a separate
 * 	drawelement (when the first drawelement of a mesh is added).
 * 	the uniform handlers of the newly created drawelement are taken from the
 * 	first drawelement added for this mesh.
 *
 * 	\ingroup graph_scene
 */
void graph_scene_drawelement_inserter(scene_ref ref, drawelement_ref de) {
	if (scene_aux_type(ref) != scene_type_graph) {
		fprintf(stderr, "Called graph_scene_drawelement_inserter on scene of type %d (expected %d).\n", scene_aux_type(ref), scene_type_graph);
		return;
	}
	default_scene_drawelement_inserter(ref, de);
	mesh_ref mesh = drawelement_mesh(de);
	material_ref material = drawelement_material(de);
	struct graph_scene_aux *gs = scene_aux(ref);
	bool found = false;
	struct by_mesh *mit;
	// find mesh node
	for (mit = gs->meshes; mit; mit = mit->next)
		if (equal_mesh_refs(mit->mesh, mesh))
			break;
	// if not found
	if (!mit) {
		struct by_mesh *new_entry = malloc(sizeof(struct by_mesh));
		new_entry->next = gs->meshes;
		gs->meshes = new_entry;
		new_entry->mesh = mesh;
		new_entry->materials = 0;
		mit = new_entry;
		// build bulk-de
		char *name = strappend("bulk_de_for_", mesh_name(new_entry->mesh));
		char *mat_name = strappend("material_for_", name);
		vec4f c = { .8, .8, .8, 1 };
		material_ref mat = make_material(mat_name, &c, &c, &c);
		material_use_stock_shader(mat);
		new_entry->bulk_de = make_drawelement(name, mesh, material_shader(mat), mat);
		//! \attention when different model transformations are applied to different submeshes, our scheme (copying the first we get a hold of) will break.
		copy_matrix4x4f(drawelement_trafo(new_entry->bulk_de), drawelement_trafo(de));
		for (struct uniform_handler_node *run = drawelement_uniform_handlers(de); run; run = run->next)
			if (run->handler)
				append_drawelement_uniform_handler(new_entry->bulk_de, run->handler);
#ifdef WITH_GUILE
			else
				append_drawelement_scheme_uniform_handler(new_entry->bulk_de, run->scheme_handler);
#endif
		free(name);
		free(mat_name);
	}
	// now it's there, anyway; search for material
	struct by_material *mat;
	for (mat = mit->materials; mat; mat = mat->next)
		if (equal_material_refs(mat->mat, material))
			break;
	// if not found
	if (!mat) {
		struct by_material *new_entry = malloc(sizeof(struct by_material));
		new_entry->next = mit->materials;
		mit->materials = new_entry;
		new_entry->mat = material;
		new_entry->drawelements = 0;
		mat = new_entry;
	}
	// now it's there, anyway; add drawelement
	struct drawelement_node *deno = malloc(sizeof(struct drawelement_node));
	deno->next = mat->drawelements;
	mat->drawelements = deno;
	deno->ref = de;
}

bool is_graph_scene(scene_ref ref) {
	if (scene_aux_type(ref) != scene_type_graph)
		return false;
	return true;
}

bool must_be_graph_scene(scene_ref ref, const char *fun) {
	bool igs = is_graph_scene(ref);
	if (!igs)
		fprintf(stderr, "Called graph_scene_traverser on scene of type %d (expected %d).\n", scene_aux_type(ref), scene_type_graph);
	return igs;
}

#define graph_scene_or_return(A) if (!must_be_graph_scene(A, __FUNCTION__)) return;
#define graph_scene_or_return_X(A,X) if (!must_be_graph_scene(A, __FUNCTION__)) return (X);

void graph_scene_traverser(scene_ref ref) {
	graph_scene_or_return(ref);
	struct graph_scene_aux *gs = scene_aux(ref);
	bool use_single_shader = use_single_shader_for_scene(ref);
	shader_ref shader = single_shader_for_scene(ref);
	uniform_setter_t uniform_setter = single_shader_extra_uniform_handler(ref);

	if (use_single_shader)
		for (struct by_mesh *by_mesh = gs->meshes; by_mesh; by_mesh = by_mesh->next) {
			bind_mesh_to_gl(by_mesh->mesh);
			for (struct by_material *by_mat = by_mesh->materials; by_mat; by_mat = by_mat->next) {
				// we'd have to separate material uniforms (textures [incl.
				// binding], ...) and drawelement uniforms (object trafo)
				bind_shader(shader);
				for (struct drawelement_node *deno = by_mat->drawelements; deno; deno = deno->next) {
					shader_ref old_shader = drawelement_change_shader(by_mat->drawelements->ref, shader);
					prepend_drawelement_uniform_handler(deno->ref, uniform_setter);
					if (drawelement_using_index_range(deno->ref))
						bind_uniforms_and_render_indices_of_drawelement(deno->ref);
					else
						bind_uniforms_and_render_drawelement_nonindexed(deno->ref);
					pop_drawelement_uniform_handler(deno->ref);
					drawelement_change_shader(by_mat->drawelements->ref, old_shader);
				}
				unbind_shader(shader);
			}
			unbind_mesh_from_gl(by_mesh->mesh);
		}
	else
		for (struct by_mesh *by_mesh = gs->meshes; by_mesh; by_mesh = by_mesh->next) {
			bind_mesh_to_gl(by_mesh->mesh);
			for (struct by_material *by_mat = by_mesh->materials; by_mat; by_mat = by_mat->next) {
				// we'd have to separate material uniforms (textures [incl.
				// binding], ...) and drawelement uniforms (object trafo)
				shader = drawelement_shader(by_mat->drawelements->ref);
				bind_shader(shader);
				for (struct drawelement_node *deno = by_mat->drawelements; deno; deno = deno->next) {
					if (drawelement_using_index_range(deno->ref))
						bind_uniforms_and_render_indices_of_drawelement(deno->ref);
					else
						bind_uniforms_and_render_drawelement_nonindexed(deno->ref);
				}
				unbind_shader(shader);
			}
			unbind_mesh_from_gl(by_mesh->mesh);
		}
}

void graph_scene_bulk_traverser(scene_ref ref) {
	graph_scene_or_return(ref);
	struct graph_scene_aux *gs = scene_aux(ref);

	if (use_single_shader_for_scene(ref)) {
		shader_ref shader = single_shader_for_scene(ref);
		for (struct by_mesh *by_mesh = gs->meshes; by_mesh; by_mesh = by_mesh->next) {
			prepend_drawelement_uniform_handler(by_mesh->bulk_de, single_shader_extra_uniform_handler(ref));
			render_drawelement_with_shader(by_mesh->bulk_de, shader);
			pop_drawelement_uniform_handler(by_mesh->bulk_de);
		}
	}
	else
		for (struct by_mesh *by_mesh = gs->meshes; by_mesh; by_mesh = by_mesh->next)
			render_drawelement(by_mesh->bulk_de);
}

void free_graph_scene_bulk_de_list(struct graph_scene_bulk_de_list *list) {
	while (list) {
		struct graph_scene_bulk_de_list *tmp = list;
		list = list->next;
		free(tmp);
	}
}

struct graph_scene_bulk_de_list* graph_scene_bulk_drawelements(scene_ref ref) {
	graph_scene_or_return_X(ref, 0);
	struct graph_scene_bulk_de_list *list = 0;
	struct graph_scene_aux *gs = scene_aux(ref);
	for (struct by_mesh *by_mesh = gs->meshes; by_mesh; by_mesh = by_mesh->next) {
		struct graph_scene_bulk_de_list *node = malloc(sizeof(struct graph_scene_bulk_de_list));
		node->ref = by_mesh->bulk_de;
		node->next = list;
		list = node;
	}
	return list;
}

scene_ref make_graph_scene(const char *name) {
    scene_ref ref = make_scene(name);
    scene_set_drawelement_inserter(ref, graph_scene_drawelement_inserter);
    scene_set_traverser(ref, graph_scene_traverser);
    struct graph_scene_aux *aux = malloc(sizeof(struct graph_scene_aux));
    scene_set_aux(ref, scene_type_graph, aux);
	aux->meshes = 0;
	return ref;
}


#ifdef WITH_GUILE
#include <libguile.h>

SCM_DEFINE(s_make_scene, "make-scene", 1, 0, 0, (SCM name), "") {
	char *n = scm_to_locale_string(name);
	scene_ref ref = make_scene(n);
	free(n);
	return scm_from_int(ref.id);
}

SCM_DEFINE(s_make_graph_scene, "make-graph-scene", 1, 0, 0, (SCM name), "") {
	char *n = scm_to_locale_string(name);
	scene_ref ref = make_graph_scene(n);
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

SCM_DEFINE(s_is_graph_scene, "is-graph-scene", 1, 0, 0, (SCM scene), "") {
	scene_ref s = { scm_to_int(scene) };
	return scm_from_bool(is_graph_scene(s));
}

SCM_DEFINE(s_graph_scene_bulk_des, "graph-scene-bulk-drawelements", 1, 0, 0, (SCM scene), "") {
	scene_ref s = { scm_to_int(scene) };
	SCM list = SCM_EOL;
	struct graph_scene_bulk_de_list *bulk_des = graph_scene_bulk_drawelements(s);
	for (struct graph_scene_bulk_de_list *run = bulk_des; run; run = run->next)
		list = scm_cons(scm_from_int(run->ref.id), list);
	free_graph_scene_bulk_de_list(bulk_des);
	return list;
}

SCM_DEFINE(s_scene_stats, "scene-stats", 0, 0, 0, (), "") {
    scene_ref ref = { 0 };
	if (scene_aux_type(ref) == scene_type_default) {
	    printf("the scene is a simple drawelement list.\n");
	    int de = 0;
        for (drawelement_node *run = scene_drawelements(ref); run; run = run->next)
            ++de;
	    printf("there are %d drawelements.\n", de);
	}
	if (scene_aux_type(ref) == scene_type_graph) {
	    printf("the scene is a built-in graph scene.\n");
        struct graph_scene_aux *gs = scene_aux(ref);
        int n = 0, m = 0, de = 0;
        int i = 0, j = 0;
        for (struct by_mesh *by_mesh = gs->meshes; by_mesh; by_mesh = by_mesh->next)
            ++n;
        printf("scene contains %d meshes.\n", n);
        for (struct by_mesh *by_mesh = gs->meshes; by_mesh; by_mesh = by_mesh->next) {
            n = 0;
            for (struct by_material *by_mat = by_mesh->materials; by_mat; by_mat = by_mat->next)
                ++n;
            printf("  mesh %d contains %d materials.\n", i++, n);
            j = 0;
            for (struct by_material *by_mat = by_mesh->materials; by_mat; by_mat = by_mat->next) {
                m = 0;
                for (struct drawelement_node *deno = by_mat->drawelements; deno; deno = deno->next)
                    ++m, ++de;
                printf("    material %d contains %d drawelements.\n", j++, m);
            }
        }
        printf("all in all there are %d drawelements.\n", de);
    }
    return SCM_BOOL_T;
}

void register_scheme_functions_for_scene() {
#include "scene.x"
}

#endif
