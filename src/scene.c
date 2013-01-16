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
	void *aux;
};

#include <libcgl/mm.h>
define_mm(scene, scenes, scene_ref);
#include "scene.xx"

// standard, most primitive, scene type

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

void default_scene_renderer(scene_ref ref) {
	for (drawelement_node *run = scene_drawelements(ref); run; run = run->next)
		render_drawelement(run->ref);
}

// graph scene: first level vbo, second level material

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
		for (struct uniform_handler_node *run = drawelement_uniform_handlers(de); run; run = run->next)
			append_uniform_handler(de, run->handler);
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

void graph_scene_traverser(scene_ref ref) {
	if (scene_aux_type(ref) != scene_type_graph) {
		fprintf(stderr, "Called graph_scene_traverser on scene of type %d (expected %d).\n", scene_aux_type(ref), scene_type_graph);
		return;
	}
	struct graph_scene_aux *gs = scene_aux(ref);
	for (struct by_mesh *by_mesh = gs->meshes; by_mesh; by_mesh = by_mesh->next) {
		bind_mesh_to_gl(by_mesh->mesh);
		for (struct by_material *by_mat = by_mesh->materials; by_mat; by_mat = by_mat->next) {
			// we'd have to separate material uniforms (textures [incl.
			// binding], ...) and drawelement uniforms (object trafo)
			bind_shader(drawelement_shader(by_mat->drawelements->ref));
			for (struct drawelement_node *deno = by_mat->drawelements; deno; deno = deno->next)
				bind_uniforms_and_render_indices_of_drawelement(deno->ref);
			unbind_shader(drawelement_shader(by_mat->drawelements->ref));
		}
		unbind_mesh_from_gl(by_mesh->mesh);
	}
}

void graph_scene_bulk_traverser(scene_ref ref) {
	if (scene_aux_type(ref) != scene_type_graph) {
		fprintf(stderr, "Called graph_scene_traverser on scene of type %d (expected %d).\n", scene_aux_type(ref), scene_type_graph);
		return;
	}
	struct graph_scene_aux *gs = scene_aux(ref);
	for (struct by_mesh *by_mesh = gs->meshes; by_mesh; by_mesh = by_mesh->next) {
		bind_mesh_to_gl(by_mesh->mesh);
		draw_mesh(by_mesh->mesh);
		unbind_mesh_from_gl(by_mesh->mesh);
	}
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
