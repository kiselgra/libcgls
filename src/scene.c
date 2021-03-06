#include "scene.h"

#include "stock-shader.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct scene {
	char *name;
    scene_drawelement_inserter_t inserter;
	scene_traverser_t trav;
	drawelement_node *drawelements;
	drawelement_node *back;

	shader_ref with_shader;  //!< this is the temporary shader for special purpose passes.
	shader_ref *shader;      //!< makes for faster checking.
	uniform_setter_t extra_uniform_handler; //!< valid only when rendering in single-shader mode.

	scene_light_application_t apply_lights;
	scene_light_setup_t light_setup;	//!< called before rendering. could be used to setup uniform handlers.
	scene_light_setup_t light_cleanup;	//!< called after rendering.
	struct light_list *lights;
	bool show_light_representations;

	drawelement_ref skybox;
	bool cull;

    unsigned int aux_type;
	void *aux;

	shader_ref *alterante_shaders;
};

struct smp_by_shader {
	struct smp_by_shader *next;
	struct drawelement_list *des;
	shader_ref shader;
};
struct smp_by_mesh {
	struct smp_by_mesh *next;
	struct smp_by_shader *by_shader;
	struct drawelement_list *tmp_des;
	mesh_ref mesh;
};
struct single_material_pass {
	char *name;
	struct drawelement_list *drawelements;
	struct drawelement_array *de_array;
	struct smp_by_mesh *by_mesh;
	uniform_setter_t extra_handler;
	int uniforms;
	char **uniform;
	char *fragment_source;
	bool closed;
	char *fragment;
	bool cull;
};

#include <libcgl/mm.h>
define_mm(scene, scenes, scene_ref);
define_mm(single_material_pass, material_passes, single_material_pass_ref);
#include "scene.xx"

vec4f cgls_scene_clear_color = { 0, 0 ,0, 0 };

/*! \defgroup basic_scene Basic Scene
 *  Standard, most primitive, scene type
 *
 *  For an extension (requiring indexed drawelements) 
 */

/*! \addtogroup basic_scene
 * 	@{
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
	scene->lights = 0;
	scene->apply_lights = 0;
	scene->light_setup = 0;
	scene->light_cleanup = 0;
	scene->show_light_representations = true;
	scene->skybox.id = -1;
	scene->cull = true;
	scene->alterante_shaders = 0;

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

void add_light_to_scene(scene_ref ref, light_ref light) {
	struct scene *scene = scenes+ref.id;
	struct light_list *node = malloc(sizeof(struct light_list));
	node->next = scene->lights;
	node->ref = light;
	scene->lights = node;
}

void scene_set_lighting(scene_ref ref, scene_light_application_t app) {
	struct scene *scene = scenes+ref.id;
	scene->apply_lights = app;
}

void scene_set_light_setup(scene_ref ref, scene_light_setup_t op) {
	struct scene *scene = scenes+ref.id;
	scene->light_setup = op;
}

void scene_set_light_cleanup(scene_ref ref, scene_light_setup_t op) {
	struct scene *scene = scenes+ref.id;
	scene->light_cleanup = op;
}

bool scene_render_light_representations(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->show_light_representations;
}

void scene_rendering_of_light_representations(scene_ref ref, bool on) {
	struct scene *scene = scenes+ref.id;
	scene->show_light_representations = on;
}

struct light_list* scene_lights(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->lights;
}

drawelement_ref scene_skybox(scene_ref ref) {
	return scenes[ref.id].skybox;
}

void set_scene_skybox(scene_ref ref, drawelement_ref de) {
	scenes[ref.id].skybox = de;
}

void enable_backface_culling_for_scene(scene_ref ref, bool yes) {
	struct scene *scene = scenes+ref.id;
	scene->cull = yes;
}

bool using_backface_culling_for_scene(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	return scene->cull;
}

scene_ref find_scene(const char *name) {
	scene_ref ref = { -1 };
	if (strlen(name) == 0) return ref;
    for (int i = 0; i < next_scene_index; ++i) {
        if (strcmp(scenes[i].name, name) == 0) {
			ref.id = i;
			return ref;
		}
	}
	return ref;
}


/*! \brief Transparently change shaders for all drawelements of the scene.
 *
 *  The provided array is index by drawelement ids, so it might necessarily be sparse.
 *  To revert to the original behaviour just call with 0.
 *  \note If such an array is set the shaders are taken from this list exclusively.
 *  \note There is no additional check if a shader to be used is valid. I.e
 *        there must be a valid shader in the array for each drawelement of the
 *        scene.
 *  \note In the same spirit: The supplied shaders are used instead of the
 *        shader that would otherwise be bound. For scenes with more advanced
 *        grouping this does not guarantee that a drawelement is drawn using
 *        its assigned replacement shader, but might use some proxy
 *        drawelement's replacement shader.
 *  \attention Correctly implemented for graph-scene, only. Sorry later me.
 */
void use_alternate_shaders(scene_ref ref, shader_ref *as, uniform_setter_t extra_handler) {
	struct scene *scene = scenes+ref.id;
	scene->alterante_shaders = as;
	scene->extra_uniform_handler = extra_handler;
}

/*! \brief Render the scene.
 *
 * 	Traversal.
 * 	Calls the scene traverser to render the drawelements. Potentially to a Gbuffer.
 * 	
 * 	Lighting.
 * 	Currently we only support lighting by a scheme in accordance to our stock
 * 	lighting, see \ref light.h, and our deferred pipeline.
 * 	Extending this function to handle lighting under forward rendering should
 * 	be straight forward, given appropriate shaders are bound to the
 * 	drawelements being rendered.
 */
void render_scene(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	scene->trav(ref);
}

//!	\copydoc render_scene
void render_scene_to_buffer(scene_ref ref, framebuffer_ref target) {
	bind_framebuffer(target);
	struct scene *scene = scenes+ref.id;
	scene->trav(ref);
	unbind_framebuffer(target);
}

//! just fill the given gbuffer
void render_scene_to_gbuffer(scene_ref ref, framebuffer_ref gbuffer) {
	bind_framebuffer(gbuffer);
	struct scene *scene = scenes+ref.id;
	scene->trav(ref);
	unbind_framebuffer(gbuffer);
}

void render_skybox_to_buffer(scene_ref ref, framebuffer_ref gbuffer) {
	struct scene *scene = scenes+ref.id;
	if (valid_drawelement_ref(scene->skybox)) {
		bind_framebuffer(gbuffer);
		float near = camera_near(current_camera()),
			  far = camera_far(current_camera());
// 		change_projection_of_cam(current_camera(), fovy, aspect, near, 1e10);
		change_near_far_of_cam(current_camera(), near, 1e10);
		recompute_gl_matrices_of_cam(current_camera());
		int depthfunc = 0;
		glGetIntegerv(GL_DEPTH_FUNC, &depthfunc);
		glDepthFunc(GL_LEQUAL);
		render_drawelement(scene->skybox);
		glDepthFunc(depthfunc);
// 		change_projection_of_cam(current_camera(), fovy, aspect, near, far);
		change_near_far_of_cam(current_camera(), near, far);
		recompute_gl_matrices_of_cam(current_camera());
		unbind_framebuffer(gbuffer);
	}
}

void render_scene_from_gbuffer(scene_ref ref, framebuffer_ref gbuffer) {
	struct scene *scene = scenes+ref.id;
	if (scene->apply_lights)
		scene->apply_lights(gbuffer, scene->lights);
	if (scene->show_light_representations)
		for (struct light_list *run = scene->lights; run; run = run->next)
			render_light_representation(run->ref);

	if (valid_drawelement_ref(scene->skybox)) {
		float near = camera_near(current_camera()),
			  far = camera_far(current_camera());
// 		change_projection_of_cam(current_camera(), fovy, aspect, near, 1e10);
		change_near_far_of_cam(current_camera(), near, 1e10);
		recompute_gl_matrices_of_cam(current_camera());
		int depthfunc = 0;
		glGetIntegerv(GL_DEPTH_FUNC, &depthfunc);
		glDepthFunc(GL_LEQUAL);
		render_drawelement(scene->skybox);
		glDepthFunc(depthfunc);
// 		change_projection_of_cam(current_camera(), fovy, aspect, near, far);
		change_near_far_of_cam(current_camera(), near, far);
		recompute_gl_matrices_of_cam(current_camera());
	}
}

void render_gbuffer_visualization(scene_ref ref, framebuffer_ref gbuffer) {
	glClearColor(0,0,0,0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	static drawelement_ref debug = { -1 };
	if (!valid_drawelement_ref(debug))
		debug = make_stock_gbuffer_default_drawelement(gbuffer, "debug", 0);
	render_drawelement(debug);
}

//!	\copydoc render_scene
void render_scene_deferred(scene_ref ref, framebuffer_ref gbuffer) {
	render_scene_to_gbuffer(ref, gbuffer);

	/*
	glClearColor(0,0,0,0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	static drawelement_ref debug = { -1 };
	if (!valid_drawelement_ref(debug))
		debug = make_stock_gbuffer_default_drawelement(gbuffer, "debug", 0);
	render_drawelement(debug);
	return;
	*/
	
	render_scene_from_gbuffer(ref, gbuffer);
}

//!	\copydoc render_scene
void render_scene_deferred_to_buffer(scene_ref ref, framebuffer_ref gbuffer, framebuffer_ref target) {
	render_scene_to_gbuffer(ref, gbuffer);

// 	bind_framebuffer(target);
// 	if (scene->apply_lights)
// 		scene->apply_lights(scene->lights);
// 	if (scene->show_light_representations)
// 		for (struct light_list *run = scene->lights; run; run = run->next)
// 			render_light_representation(run->ref);
// 	unbind_framebuffer(target);

	bind_framebuffer(target);
	render_scene_from_gbuffer(ref, gbuffer);
	unbind_framebuffer(target);
}

void render_scene_with_shader(scene_ref ref, shader_ref shader, uniform_setter_t extra_handler) {
	struct scene *scene = scenes+ref.id;
	scene->with_shader = shader;
	scene->shader = &scene->with_shader;
	scene->extra_uniform_handler = extra_handler;
	scene->trav(ref);
	if (scene->show_light_representations)
		for (struct light_list *run = scene->lights; run; run = run->next)
			render_light_representation_with_shader(run->ref, shader, single_shader_extra_uniform_handler(ref));
	scene->shader = 0;
	scene->with_shader = make_invalid_shader();
	scene->extra_uniform_handler = 0;
}

void default_scene_renderer(scene_ref ref) {
	struct scene *scene = scenes+ref.id;
	if (scene->cull) {
		glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);
	}
	if (use_single_shader_for_scene(ref)) {
		shader_ref shader = single_shader_for_scene(ref);
		for (drawelement_node *run = scene_drawelements(ref); run; run = run->next) {
			if (!drawelement_hidden(run->ref)) {
				uniform_setter_t extra = single_shader_extra_uniform_handler(ref);
				if (extra)
					prepend_drawelement_uniform_handler(run->ref, extra);
				render_drawelement_with_shader(run->ref, shader);
				if (extra)
					pop_drawelement_uniform_handler(run->ref);
			}
		}
	}
	else {
		frustum_culling_t data;
		populate_frustum_culling_info(current_camera(), &data);
		int all = 0, c = 0;
		for (drawelement_node *run = scene_drawelements(ref); run; run = run->next)
			if (!drawelement_hidden(run->ref)) {
				all++;
#if CGLS_DRAWELEMENT_BB_VIS == 1
				if (!drawelement_shows_bounding_box(run->ref)) {
#endif
					vec3f min, max;
					bounding_box_of_drawelement(run->ref, &min, &max);
					if (!drawelement_has_bounding_box(run->ref) || drawelement_with_instancing(run->ref) || aabb_in_frustum(&data, &min, &max)) {
						c++;
						render_drawelement(run->ref);
					}
#if CGLS_DRAWELEMENT_BB_VIS == 1
				}
				else {
					render_drawelement_box(run->ref);
				}
#endif
			}
// 		printf("rendered %d of %d\n", c, all);
	}
	if (scene->cull) {
		glDisable(GL_CULL_FACE);
	}
}

//! @}

/*! \defgroup graph_scene Graph Scene
 *  First level vbo, second level material.
 *
 *  \note The public interface of \ref basic_scene still applies and is not repeated here.
 *  		The graph scene is implemented via \ref scene_aux.
 */

/*! \addtogroup graph_scene
 * 	@{
 */

//! internal \ref graph_scene structure. \ingroup graph_scene
struct by_material {
    material_ref mat;
    drawelement_node *drawelements;
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
		shader_ref bulk_shader = { -1 };
		new_entry->bulk_de = make_drawelement(name, mesh, bulk_shader, mat);
		bulk_shader = make_stock_shader(0, new_entry->bulk_de, 0, true, 0);
		drawelement_change_shader(new_entry->bulk_de, bulk_shader);
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
	drawelement_node *deno = malloc(sizeof(drawelement_node));
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

//! @}

#define graph_scene_or_return(A) if (!must_be_graph_scene(A, __FUNCTION__)) return;
#define graph_scene_or_return_X(A,X) if (!must_be_graph_scene(A, __FUNCTION__)) return (X);

/*! \addtogroup graph_scene
 * 	@{
 */

void graph_scene_traverser(scene_ref ref) {
	graph_scene_or_return(ref);
	struct scene *scene = scenes+ref.id;
	struct graph_scene_aux *gs = scene_aux(ref);
	bool use_single_shader = use_single_shader_for_scene(ref);
	shader_ref shader = single_shader_for_scene(ref);
	uniform_setter_t uniform_setter = single_shader_extra_uniform_handler(ref);

	if (scene->cull) {
		glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);
	}
	else
		glDisable(GL_CULL_FACE);

	// used for forward rendering
	if (scene->light_setup)
		scene->light_setup(scene->lights);

	if (use_single_shader)
		for (struct by_mesh *by_mesh = gs->meshes; by_mesh; by_mesh = by_mesh->next) {
			bind_mesh_to_gl(by_mesh->mesh);
			for (struct by_material *by_mat = by_mesh->materials; by_mat; by_mat = by_mat->next) {
				// we'd have to separate material uniforms (textures [incl.
				// binding], ...) and drawelement uniforms (object trafo)
				bind_shader(shader);
				for (drawelement_node *deno = by_mat->drawelements; deno; deno = deno->next) {
					if (!drawelement_hidden(deno->ref)) {
						// not sure why i changed the shader of the material group, as the uniforms are taken from the drawelement's shader.
						// shader_ref old_shader = drawelement_change_shader(by_mat->drawelements->ref, shader);
						shader_ref old_shader = drawelement_change_shader(deno->ref, shader);
						if (uniform_setter)
							prepend_drawelement_uniform_handler(deno->ref, uniform_setter);
						if (drawelement_using_index_range(deno->ref))
							bind_uniforms_and_render_indices_of_drawelement(deno->ref);
						else
							bind_uniforms_and_render_drawelement_nonindexed(deno->ref);
						if (uniform_setter)
							pop_drawelement_uniform_handler(deno->ref);
						// drawelement_change_shader(by_mat->drawelements->ref, old_shader);
						drawelement_change_shader(deno->ref, old_shader);
					}
				}
				unbind_shader(shader);
			}
			unbind_mesh_from_gl(by_mesh->mesh);
		}
	else if (scene->alterante_shaders) {
		frustum_culling_t data;
		populate_frustum_culling_info(current_camera(), &data);
		int a = 0, c = 0;
		shader_ref old;
		for (struct by_mesh *by_mesh = gs->meshes; by_mesh; by_mesh = by_mesh->next) {
			bind_mesh_to_gl(by_mesh->mesh);
			for (struct by_material *by_mat = by_mesh->materials; by_mat; by_mat = by_mat->next) {
				// we'd have to separate material uniforms (textures [incl.
				// binding], ...) and drawelement uniforms (object trafo)
				shader = scene->alterante_shaders[by_mat->drawelements->ref.id];
				bind_shader(shader);
				for (drawelement_node *deno = by_mat->drawelements; deno; deno = deno->next) {
#if CGLS_DRAWELEMENT_BB_VIS == 1
					if (!drawelement_shows_bounding_box(deno->ref)) {
#endif
						a++;
					if (!drawelement_hidden(deno->ref)) {
						vec3f min, max;
						bounding_box_of_drawelement(deno->ref, &min, &max);
						if (!drawelement_has_bounding_box(deno->ref) || drawelement_with_instancing(deno->ref) || aabb_in_frustum(&data, &min, &max)) {
							c++;
							old = drawelement_change_shader(deno->ref, shader);	// for uniform lists.
							if (uniform_setter)
								prepend_drawelement_uniform_handler(deno->ref, uniform_setter);
							if (drawelement_using_index_range(deno->ref))
								bind_uniforms_and_render_indices_of_drawelement(deno->ref);
							else
								bind_uniforms_and_render_drawelement_nonindexed(deno->ref);
							if (uniform_setter)
								pop_drawelement_uniform_handler(deno->ref);
							drawelement_change_shader(deno->ref, old);
						}
					}
#if CGLS_DRAWELEMENT_BB_VIS == 1
					}
#endif
				}
				unbind_shader(shader);
			}
			unbind_mesh_from_gl(by_mesh->mesh);
		}
	}
	else {
		frustum_culling_t data;
		populate_frustum_culling_info(current_camera(), &data);
		int a = 0, c = 0;
		for (struct by_mesh *by_mesh = gs->meshes; by_mesh; by_mesh = by_mesh->next) {
			bind_mesh_to_gl(by_mesh->mesh);
			for (struct by_material *by_mat = by_mesh->materials; by_mat; by_mat = by_mat->next) {
				// we'd have to separate material uniforms (textures [incl.
				// binding], ...) and drawelement uniforms (object trafo)
				shader = drawelement_shader(by_mat->drawelements->ref);
				bind_shader(shader);
				for (drawelement_node *deno = by_mat->drawelements; deno; deno = deno->next) {
#if CGLS_DRAWELEMENT_BB_VIS == 1
					if (!drawelement_shows_bounding_box(deno->ref)) {
#endif
						a++;
					if (!drawelement_hidden(deno->ref)) {
						vec3f min, max;
						bounding_box_of_drawelement(deno->ref, &min, &max);
						if (!drawelement_has_bounding_box(deno->ref) || drawelement_with_instancing(deno->ref) || aabb_in_frustum(&data, &min, &max)) {
							c++;
							if (drawelement_using_index_range(deno->ref))
								bind_uniforms_and_render_indices_of_drawelement(deno->ref);
							else
								bind_uniforms_and_render_drawelement_nonindexed(deno->ref);
						}
					}
#if CGLS_DRAWELEMENT_BB_VIS == 1
					}
#endif
				}
				unbind_shader(shader);
			}
			unbind_mesh_from_gl(by_mesh->mesh);
		}
// 		printf("graph scene %d / %d\n", c, a);
	}
#if CGLS_DRAWELEMENT_BB_VIS == 1
		for (drawelement_node *run = scene_drawelements(ref); run; run = run->next)
			if (drawelement_shows_bounding_box(run->ref))
				if (!drawelement_hidden(run->ref))
					render_drawelement_box(run->ref);
#endif
	if (scene->light_cleanup)
		scene->light_cleanup(scene->lights);
	if (scene->cull) {
		glDisable(GL_CULL_FACE);
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

//! @}

// 
// single material pass
//

single_material_pass_ref make_single_material_pass(const char *name, struct drawelement_list *drawelements, const char *fragment_source, int uniforms, char **uniform, uniform_setter_t extra_handler) {
	single_material_pass_ref ref = allocate_single_material_pass_ref();
	struct single_material_pass *smp = material_passes+ref.id;
	smp->name = strdup(name);
	smp->drawelements = drawelements;
	smp->de_array = 0;
	smp->closed = false;
	smp->extra_handler = extra_handler;
	smp->uniforms = uniforms;
	smp->uniform = malloc(sizeof(char*)*uniforms);
	for (int i = 0; i < uniforms; ++i)
		smp->uniform[i] = strdup(uniform[i]);
	smp->fragment_source = strdup(fragment_source);
	smp->fragment = 0;
	smp->cull = true;
	return ref;
}

single_material_pass_ref make_single_material_pass_using_array(const char *name, struct drawelement_array *array, const char *fragment_source, int uniforms, char **uniform, uniform_setter_t extra_handler) {
	single_material_pass_ref ref = make_single_material_pass(name, 0, fragment_source, uniforms, uniform, extra_handler);
	struct single_material_pass *smp = material_passes+ref.id;
	smp->de_array = array;
	return ref;
}



static struct custom_shader_fragment *single_material_shader_fragments = 0;

void register_custom_shader_fragment(const char *name, const char *source, int u, char **U) {
	struct custom_shader_fragment *new = malloc(sizeof(struct custom_shader_fragment));
	new->name = strdup(name);
	new->source = strdup(source);
	new->uniforms = u;
	new->uniform = malloc(sizeof(char*)*u);
	for (int i = 0; i < u; ++i)
		new->uniform[i] = strdup(U[i]);
	new->next = single_material_shader_fragments;
	single_material_shader_fragments = new;
}
	
struct custom_shader_fragment* find_shader_fragment(const char *name) {
	for (struct custom_shader_fragment *run = single_material_shader_fragments; run; run = run->next)
		if (strcmp(run->name, name) == 0)
			return run;
	return 0;
}

single_material_pass_ref make_single_material_pass_from_fragment(const char *name, struct drawelement_list *drawelements, const char *fragment, uniform_setter_t extra_handler) {
	for (struct custom_shader_fragment *run = single_material_shader_fragments; run; run = run->next)
		if (strcmp(run->name, fragment) == 0)
			return make_single_material_pass(name, drawelements, run->source, run->uniforms, run->uniform, extra_handler);
	single_material_pass_ref bad = { -1 };
	return bad;
}

single_material_pass_ref make_single_material_pass_from_fragment_using_array(const char *name, struct drawelement_array *array, const char *fragment, uniform_setter_t extra_handler) {
	for (struct custom_shader_fragment *run = single_material_shader_fragments; run; run = run->next)
		if (strcmp(run->name, fragment) == 0)
			return make_single_material_pass_using_array(name, array, run->source, run->uniforms, run->uniform, extra_handler);
	single_material_pass_ref bad = { -1 };
	return bad;
}

void add_drawelement_to_single_material_pass(single_material_pass_ref ref, drawelement_ref de) {
	struct single_material_pass *smp = material_passes+ref.id;
	if (smp->closed) {
		fprintf(stderr, "Error: Cannot add drawelement '%s' to already closed material pass '%s'.\n", drawelement_name(de), smp->name);
		return;
	}
	if (smp->de_array) {
		fprintf(stderr, "Error: This pass collects its drawelements via an external array. Please maintain it explicitly.\n");
		return;
	}
	struct drawelement_list *new = malloc(sizeof(struct drawelement_list));
	new->ref = de;
	new->next = smp->drawelements;
	smp->drawelements = new;
}

void finalize_single_material_pass(single_material_pass_ref ref) {
	struct single_material_pass *smp = material_passes+ref.id;
	smp->closed = true;

	// partition by mesh
	if (smp->de_array) {
		smp->by_mesh = 0;
		for (int i = 0; i < smp->de_array->size; ++i) {
// 			printf("looking at de '%s'\n", drawelement_name(smp->de_array->element[i]));
			struct smp_by_mesh *bm = smp->by_mesh;
			while (bm && bm->mesh.id != drawelement_mesh(smp->de_array->element[i]).id)
				bm = bm->next;
			if (!bm) {
				struct smp_by_mesh *new = malloc(sizeof(struct smp_by_mesh));
				new->mesh = drawelement_mesh(smp->de_array->element[i]);
				new->by_shader = 0;
				new->tmp_des = 0;
				new->next = smp->by_mesh;
				smp->by_mesh = bm = new;
			}
// 			printf("  --> into bucket of mesh '%s'\n", mesh_name(bm->mesh));
			struct drawelement_list *node = malloc(sizeof(struct drawelement_list));
			node->next = bm->tmp_des;
			node->ref = smp->de_array->element[i];
			bm->tmp_des = node;
		}
	}
	else {
		smp->by_mesh = 0;
		for (struct drawelement_list *run = smp->drawelements; run; run = run->next) {
			struct smp_by_mesh *bm = smp->by_mesh;
			while (bm && bm->mesh.id != drawelement_mesh(run->ref).id)
				bm = bm->next;
			if (!bm) {
				struct smp_by_mesh *new = malloc(sizeof(struct smp_by_mesh));
				new->mesh = drawelement_mesh(run->ref);
				new->by_shader = 0;
				new->tmp_des = 0;
				new->next = smp->by_mesh;
				smp->by_mesh = bm = new;
			}
			struct drawelement_list *node = malloc(sizeof(struct drawelement_list));
			node->next = bm->tmp_des;
			node->ref = run->ref;
			bm->tmp_des = node;
		}
	}

	// partition mesh buckets by shader
	for (struct smp_by_mesh *bm = smp->by_mesh; bm; bm = bm->next) {
		for (struct drawelement_list *run = bm->tmp_des; run; run = run->next) {
			struct smp_by_shader *bs = bm->by_shader;
			while (bs && bs->shader.id != drawelement_shader(run->ref).id)
				bs = bs->next;
			if (!bs) {
				struct smp_by_shader *new = malloc(sizeof(struct smp_by_shader));
				new->next = bm->by_shader;
				new->shader = drawelement_shader(run->ref);
				new->des = 0;
				bm->by_shader = bs = new;
			}
			struct drawelement_list *node = malloc(sizeof(struct drawelement_list));
			node->next = bs->des;
			node->ref = run->ref;
			bs->des = node;
		}
		struct drawelement_list *run = bm->tmp_des; 
		while (run) {
			struct drawelement_list *old = run;
			run = run->next;
			free(old);
		}
	}

	// replace shaders
	for (struct smp_by_mesh *bm = smp->by_mesh; bm; bm = bm->next) {
		for (struct smp_by_shader *bs = bm->by_shader; bs; bs = bs->next) {
			// default stock shader
			struct stockshader_fragments ssf;
			init_stockshader_fragments(&ssf);
			stockshader_add_fsource(&ssf, smp->fragment_source);
			for (int i = 0; i < smp->uniforms; ++i)
				stockshader_add_uniform(&ssf, smp->uniform[i]);
			// add drawelement vertex shader part, use_tc according to \ref make_stock_shader_fragments.
			bool use_tc = false;
			for (int i = 0; i < shader_uniforms(bs->shader); ++i) {
				const char *name = shader_uniform_name_by_id(bs->shader, i);
				if (strcmp(name, "ambient_tex") == 0
						|| strcmp(name, "diffuse_tex") == 0
						|| strcmp(name, "specular_tex") == 0
						|| strcmp(name, "mask_tex") == 0) {
					use_tc = true;
					break;
				}
			}
			add_stock_vertex_shader_part(&ssf, true, use_tc, drawelement_number_of_bones(bs->des->ref), drawelement_with_path(bs->des->ref), drawelement_with_instancing(bs->des->ref));
			char *sname = 0;
			int n = asprintf(&sname, "single-material-shader for pass '%s' based on shader '%s'", smp->name, shader_name(bs->shader));
			bs->shader = make_shader(sname, stockshader_inputs(&ssf));
			populate_shader_with_fragments(bs->shader, &ssf);
			compile_and_link_shader_showing_log_on_error(bs->shader);
			free(sname);
		}
	}
}

void finalize_single_material_passes_for_array(struct drawelement_array *array) {
	for (int i = 0; i < next_single_material_pass_index; ++i)
		if (material_passes[i].de_array == array) {
			single_material_pass_ref ref = { i };
			finalize_single_material_pass(ref);
		}
}

void render_single_material_pass(single_material_pass_ref refx) {
	struct single_material_pass *smp = material_passes + refx.id;
	if (smp->cull) {
		glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);
	}
	
	for (struct smp_by_mesh *bm = smp->by_mesh; bm; bm = bm->next) {
		bind_mesh_to_gl(bm->mesh);
		for (struct smp_by_shader *bs = bm->by_shader; bs; bs = bs->next) {
			bind_shader(bs->shader);

			for (struct drawelement_list *run = bs->des; run; run = run->next) {
				if (!drawelement_hidden(run->ref)) {
					shader_ref old_shader = drawelement_change_shader(run->ref, bs->shader);
					if (smp->extra_handler)
						prepend_drawelement_uniform_handler(run->ref, smp->extra_handler);
					if (drawelement_using_index_range(run->ref))
						bind_uniforms_and_render_indices_of_drawelement(run->ref);
					else
						bind_uniforms_and_render_drawelement_nonindexed(run->ref);
					if (smp->extra_handler)
						pop_drawelement_uniform_handler(run->ref);
					drawelement_change_shader(run->ref, old_shader);
				}
			}

			unbind_shader(bs->shader);
		}
		unbind_mesh_from_gl(bm->mesh);
	}
}

void enable_backface_culling_for_material_pass(single_material_pass_ref ref, bool yes) {
	struct single_material_pass *pass = material_passes+ref.id;
	pass->cull = yes;
}

bool using_backface_culling_for_material_pass(single_material_pass_ref ref) {
	struct single_material_pass *pass = material_passes+ref.id;
	return pass->cull;
}



// 
// scheme
//


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

SCM_DEFINE(s_scene_add_light, "add-light-to-scene", 2, 0, 0, (SCM scene, SCM light), "") {
	scene_ref s = { scm_to_int(scene) };
	light_ref l = { scm_to_int(light) };
	add_light_to_scene(s, l);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_scene_drawelements, "drawelement-of-scene", 1, 0, 0, (SCM scene), "") {
	scene_ref s = { scm_to_int(scene) };
	drawelement_node *node = scene_drawelements(s);
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

SCM_DEFINE(s_graph_set_bfc, "enable-backface-culling-for-scene", 2, 0, 0, (SCM scene, SCM yn), "") {
	scene_ref s = { scm_to_int(scene) };
	bool on = scm_is_true(yn);
	bool prev = using_backface_culling_for_scene(s);
	enable_backface_culling_for_scene(s, on);
	return scm_from_bool(prev);
}

SCM_DEFINE(s_graph_using_bfc, "using-backface-culling-for-scene", 1, 0, 0, (SCM scene), "") {
	scene_ref s = { scm_to_int(scene) };
	bool ret = using_backface_culling_for_scene(s);
	return scm_from_bool(ret);
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
                for (drawelement_node *deno = by_mat->drawelements; deno; deno = deno->next)
                    ++m, ++de;
                printf("    material %d contains %d drawelements.\n", j++, m);
            }
        }
        printf("all in all there are %d drawelements.\n", de);
    }
    return SCM_BOOL_T;
}

SCM_DEFINE(s_scene_clear_color_x, "scene-clear-color!", 4, 0, 0, (SCM r, SCM g, SCM b, SCM a), "") {
	cgls_scene_clear_color.x = scm_to_double(r);
	cgls_scene_clear_color.y = scm_to_double(g);
	cgls_scene_clear_color.z = scm_to_double(b);
	cgls_scene_clear_color.w = scm_to_double(a);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_register_single_material_fragment, "register-single-material-shader-fragment", 3, 0, 0, (SCM name, SCM code, SCM uniforms), "") {
	char *n = scm_to_locale_string(name);
	char *c = scm_to_locale_string(code);
	int u = scm_to_int(scm_length(uniforms));
	char **U = malloc(sizeof(char*)*u);
	for (int i = 0; i < u; ++i)
		U[i] = scm_to_locale_string(scm_list_ref(uniforms, scm_from_int(i)));
	register_custom_shader_fragment(n, c, u, U);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_scene_has_sb, "scene-with-skybox", 1, 0, 0, (SCM scene), "") {
	scene_ref s = { scm_to_int(scene) };
	drawelement_ref ref = scene_skybox(s);
	if (ref.id >= 0)
		return scm_from_int(ref.id);
	return SCM_BOOL_F;
}

SCM_DEFINE(s_scene_set_sb, "change-scene-skybox!", 2, 0, 0, (SCM scene, SCM drawelem), "") {
	scene_ref s = { scm_to_int(scene) };
	drawelement_ref d = { scm_to_int(drawelem) };
	set_scene_skybox(s, d);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_find_scene, "find-scene", 1, 0, 0, (SCM name), "") {
    char *n = scm_to_locale_string(name);
    scene_ref ref = find_scene(n);
    free(n);
    return scm_from_int(ref.id);
}



void register_scheme_functions_for_scene() {
#include "scene.x"
	scm_c_eval_string("(define register-shader-fragment register-single-material-shader-fragment)");
}

#endif
