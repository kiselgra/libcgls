#include "drawelement.h"

#include <string.h>
#include <stdlib.h>

#include <stdio.h>

/*! \defgroup drawelements Drawelements
 *
 *  A drawelement represents a submodel of the scene which
 *  \li is placed somewhere by a model matrix,
 *  \li is rendered by a single shader,
 *  \li has a single material (see \ref materials), and
 *  \li consists of a single mesh instance (see meshes in cgl).
 *
 *  Furthermore it holds a uniform-handler chain, see \ref uniforms.
 *
 *  It can be drawn entirely by the information it holds (in contrast to a plain mesh as defined by cgl).
 *
 *  It is possible for some drawelements to share the same mesh, modelmatrix, shader and material and yet be different drawelements
 *  	by indexing different intervals of the mesh.
 *  Such drawelements are referred to as `indexed drawelements', generated by \ref load_objfile_and_create_objects_with_single_vbo and best
 *  	rendered via a \ref graph_scene.
 */

/*! \file drawelement.h
 *  \ingroup drawelements
 */

struct drawelement {
	char *name;
	mesh_ref mesh;
	shader_ref shader;
	material_ref material;
	struct uniform_handler_node *handler_chain;
	matrix4x4f *trafo;
	bool malloced_trafo;
	bool use_index_range;
	unsigned int index_buffer_start, indices;
	vec3f bb_min, bb_max;
	bool has_bb;
	bool hidden;
};

#include <libcgl/mm.h>
define_mm(drawelement, drawelements, drawelement_ref);
#include "drawelement.xx"

/*! \addtogroup drawelements
 * 	@{
 */

drawelement_ref make_drawelement(const char *name, mesh_ref mr, shader_ref sr, material_ref matr) {
	drawelement_ref ref = allocate_drawelement_ref();
	struct drawelement *de = drawelements+ref.id;
	de->name = strdup(name);

	de->mesh = mr;
	de->shader = sr;
	de->material = matr;

	de->handler_chain = 0;
	de->trafo = malloc(sizeof(matrix4x4f));
	de->malloced_trafo = true;
	make_unit_matrix4x4f(de->trafo);

	de->use_index_range = false;
	de->has_bb = false;

// 	printf("create drawelement %s.\n", de->name);
	return ref;
}

const char* drawelement_name(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;
	return de->name;
}

matrix4x4f* drawelement_trafo(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;
	return de->trafo;
}

/*! \brief Make the drawelement point to the given transformation matrix.
 *
 * 	You can use this to attach a drawelement to some other scene element, e.g. a camera or a light.
 *  \attention This changes the underlying pointer, so be sure to keep your matrix around!
 */
void replace_drawelement_trafo(drawelement_ref ref, matrix4x4f *new_trafo) {
	struct drawelement *de = drawelements + ref.id;
	if (de->malloced_trafo)
		free(de->trafo);
	de->malloced_trafo = false;
	de->trafo = new_trafo;
}

mesh_ref drawelement_mesh(drawelement_ref ref) {
	struct drawelement* de = drawelements+ref.id;
	return de->mesh;
}

shader_ref drawelement_shader(drawelement_ref ref) {
	struct drawelement* de = drawelements+ref.id;
	return de->shader;
}

material_ref drawelement_material(drawelement_ref ref) {
	struct drawelement* de = drawelements+ref.id;
	return de->material;
}

shader_ref drawelement_change_shader(drawelement_ref ref, shader_ref s) {
	struct drawelement* de = drawelements+ref.id;
    shader_ref old = de->shader;
    de->shader = s;
	return old;
}

material_ref drawelement_change_material(drawelement_ref ref, material_ref m) {
	struct drawelement* de = drawelements+ref.id;
    material_ref old = drawelement_material(ref);
    de->material = m;
	return old;
}

void set_drawelement_index_buffer_range(drawelement_ref ref, unsigned int start, unsigned int count) {
	struct drawelement* de = drawelements+ref.id;
	de->index_buffer_start = start;
	de->indices = count;
	de->use_index_range = true;
}

unsigned int drawelement_index_range_start(drawelement_ref ref) {
	struct drawelement* de = drawelements+ref.id;
	return de->index_buffer_start;
}

unsigned int drawelement_index_range_len(drawelement_ref ref) {
	struct drawelement* de = drawelements+ref.id;
	return de->indices;
}

void set_drawelement_bounding_box(drawelement_ref ref, vec3f *min, vec3f *max) {
	struct drawelement* de = drawelements+ref.id;
	de->bb_min = *min;
	de->bb_max = *max;
	de->has_bb = true;
}

//! this returns the drawelement's initial bounding box, transformed by the drawelement's matrix.
void bounding_box_of_drawelement(drawelement_ref ref, vec3f *min, vec3f *max) {
	struct drawelement* de = drawelements+ref.id;
	vec4f tmp, res;
	tmp.x = de->bb_min.x; 
	tmp.y = de->bb_min.y; 
	tmp.z = de->bb_min.z; 
	tmp.w = 1;
	multiply_matrix4x4f_vec4f(&res, de->trafo, &tmp);
	min->x = res.x; 
	min->y = res.y; 
	min->z = res.z;
	tmp.x = de->bb_max.x; 
	tmp.y = de->bb_max.y; 
	tmp.z = de->bb_max.z; 
	tmp.w = 1;
	multiply_matrix4x4f_vec4f(&res, de->trafo, &tmp);
	max->x = res.x; 
	max->y = res.y; 
	max->z = res.z;
}

bool drawelement_has_bounding_box(drawelement_ref ref) {
	return drawelements[ref.id].has_bb;
}

//! @}

// uniform handlers

#define str_eq(X, Y) (strcmp(X, Y) == 0)

//! \ingroup uniforms
bool default_matrix_uniform_handler(drawelement_ref *ref, const char *uniform, int location) {
	if (str_eq(uniform, "proj"))
		glUniformMatrix4fv(location, 1, GL_FALSE, projection_matrix_of_cam(current_camera())->col_major);
	else if (str_eq(uniform, "view"))
		glUniformMatrix4fv(location, 1, GL_FALSE, gl_view_matrix_of_cam(current_camera())->col_major);
	else if (str_eq(uniform, "normal_matrix"))
		glUniformMatrix4fv(location, 1, GL_FALSE, gl_normal_matrix_for_view_of(current_camera())->col_major);
	else if (str_eq(uniform, "model"))
		glUniformMatrix4fv(location, 1, GL_FALSE, drawelement_trafo(*ref)->col_major);
	else
		return false;
	return true;
}

#undef str_eq

/*! \addtogroup drawelements
 * 	@{
 */

//! prepend (== push) \ingroup uniform_handling
void prepend_drawelement_uniform_handler(drawelement_ref ref, uniform_setter_t handler) {
	struct drawelement *de = drawelements+ref.id;
	prepend_uniform_handler(&de->handler_chain, handler);
}

//! remove handler which was prepended last \ingroup uniform_handling
void pop_drawelement_uniform_handler(drawelement_ref ref) {
	struct drawelement *de = drawelements+ref.id;
	pop_uniform_handler(&de->handler_chain);
}

/*! a little slower than \ref prepend_uniform_handler. 
 *  \ingroup uniform_handling
 */
void append_drawelement_uniform_handler(drawelement_ref ref, uniform_setter_t handler) {
	struct drawelement *de = drawelements+ref.id;
	append_uniform_handler(&de->handler_chain, handler);
}

#ifdef WITH_GUILE
void append_drawelement_scheme_uniform_handler(drawelement_ref ref, SCM handler) {
	struct drawelement *de = drawelements+ref.id;
	append_scheme_uniform_handler(&de->handler_chain, handler);
}
#endif

void bind_drawelement_uniforms(struct drawelement *de, drawelement_ref ref) {
	bind_handled_uniforms(de->handler_chain, de->shader, &ref, "drawelement", de->name);
}

void bind_uniforms_and_render_indices_of_drawelement(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;
	bind_drawelement_uniforms(de, ref);
	glDrawElements(mesh_primitive_type(de->mesh), de->indices, GL_UNSIGNED_INT, (void*)(de->index_buffer_start*sizeof(GLint)));
}

void bind_uniforms_and_render_drawelement_nonindexed(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;
	bind_drawelement_uniforms(de, ref);
	draw_mesh(de->mesh);
}

bool drawelement_using_index_range(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;
	return de->use_index_range;
}

void render_drawelement(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;

	bind_shader(de->shader);
	bind_drawelement_uniforms(de, ref);

	bind_mesh_to_gl(de->mesh);
	if (!de->use_index_range)
		draw_mesh(de->mesh);
	else
		glDrawElements(mesh_primitive_type(de->mesh), de->indices, GL_UNSIGNED_INT, (void*)(de->index_buffer_start*sizeof(GLint)));

	unbind_mesh_from_gl(de->mesh);

	unbind_shader(de->shader);
}

void render_drawelement_with_shader(drawelement_ref ref, shader_ref shader) {
    shader_ref default_s = drawelement_change_shader(ref, shader);
    render_drawelement(ref);
    drawelement_change_shader(ref, default_s);
}

void render_drawelement_with_material(drawelement_ref ref, material_ref material) {
    material_ref default_m = drawelement_change_material(ref, material);
    render_drawelement(ref);
    drawelement_change_material(ref, default_m);
}

void render_drawelement_with(drawelement_ref ref, shader_ref shader, material_ref material) {
    shader_ref default_s = drawelement_change_shader(ref, shader);
    material_ref default_m = drawelement_change_material(ref, material);
    render_drawelement(ref);
    drawelement_change_material(ref, default_m);
    drawelement_change_shader(ref, default_s);
}

//! does only return the c handlers, atm!
struct uniform_handler_node* drawelement_uniform_handlers(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;
	return de->handler_chain;
}

/*! take a drawelement out of the standard rendering process.
 *  \attention this does not have any effect on \ref render_drawelement but is used by the functions rendering the \ref scene.
 *  \note this is to make special treatment of certain drawelements more straightforward.
 */
void hide_drawelement(drawelement_ref ref, bool hide) {
	struct drawelement *de = drawelements + ref.id;
	de->hidden = hide;
}

bool drawelement_hidden(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;
	return de->hidden;
}

drawelement_ref find_drawelement(const char *name) {
	drawelement_ref ref = { -1 };
	if (strlen(name) == 0) return ref;
    for (int i = 0; i < next_drawelement_index; ++i) {
//         printf("is %s == %s?\n", name, drawelements[i].name);
        if (strcmp(drawelements[i].name, name) == 0) {
			ref.id = i;
			return ref;
		}
	}
	return ref;
}

// !!! this does not work when we start removing drawelements!
struct drawelement_list* list_drawelements() {
	if (next_drawelement_index == 0)
		return 0;
	struct drawelement_list *head = malloc(sizeof(struct drawelement_list));
	struct drawelement_list *run = head;
	run->ref.id = 0;
    for (int i = 1; i < next_drawelement_index; ++i) {
		run = run->next = malloc(sizeof(struct drawelement_list));
		run->ref.id = i;
	}
	run->next = 0;
    return head;
}

//! @}

#ifdef WITH_GUILE

// drawelement_ref make_drawelement(const char *modelname, mesh_ref mr, shader_ref sr, material_ref matr) {
SCM_DEFINE(s_make_drawelement, "make-drawelement", 4, 0, 0, (SCM name, SCM mesh, SCM shader, SCM material), "") {
	char *n = scm_to_locale_string(name);
	mesh_ref me = { scm_to_int(mesh) };
	shader_ref sh = { scm_to_int(shader) };
	material_ref ma = { scm_to_int(material) };
	drawelement_ref ref = make_drawelement(n, me, sh, ma);
	free(n);
	return scm_from_int(ref.id);
}

SCM_DEFINE(s_prepend_uniform_handler, "prepend-uniform-handler", 2, 0, 0, (SCM id, SCM handler), "") {
	struct drawelement *de = drawelements+scm_to_int(id);
	if (scm_is_symbol(handler)) {
		char *symbol = scm_to_locale_string(scm_symbol_to_string(handler));
		drawelement_ref ref = { scm_to_int(id) };
		if (strcmp(symbol, "default-material-uniform-handler") == 0)
			prepend_drawelement_uniform_handler(ref, (uniform_setter_t)default_material_uniform_handler);
		else if (strcmp(symbol, "default-matrix-uniform-handler") == 0)
			prepend_drawelement_uniform_handler(ref, (uniform_setter_t)default_matrix_uniform_handler);
		else
			scm_throw(scm_from_locale_symbol("prepend-uniform-handler-error"), scm_list_2(scm_from_locale_string("invalid key"), handler));
	}
	else {
		prepend_scheme_uniform_handler(&de->handler_chain, handler);
	}
	return SCM_BOOL_T;
}

SCM_DEFINE(s_render_drawelement, "render-drawelement", 1, 0, 0, (SCM de), "") {
	drawelement_ref ref = { scm_to_int(de) };
    render_drawelement(ref);
    return SCM_BOOL_T;
}

SCM_DEFINE(s_render_drawelement_w_s, "render-drawelement-with-shader", 2, 0, 0, (SCM de, SCM sh), "") {
	drawelement_ref ref = { scm_to_int(de) };
	shader_ref s_ref = { scm_to_int(sh) };
    render_drawelement_with_shader(ref, s_ref);
    return SCM_BOOL_T;
}

SCM_DEFINE(s_render_drawelement_w_m, "render-drawelement-with-material", 2, 0, 0, (SCM de, SCM mt), "") {
	drawelement_ref ref = { scm_to_int(de) };
	material_ref m_ref = { scm_to_int(mt) };
    render_drawelement_with_material(ref, m_ref);
    return SCM_BOOL_T;
}

SCM_DEFINE(s_render_drawelement_w, "render-drawelement-with", 3, 0, 0, (SCM de, SCM sh, SCM mt), "") {
	drawelement_ref ref = { scm_to_int(de) };
	shader_ref s_ref = { scm_to_int(sh) };
	material_ref m_ref = { scm_to_int(mt) };
    render_drawelement_with(ref, s_ref, m_ref);
}

SCM_DEFINE(s_drawelement_shader, "drawelement-shader", 1, 0, 0, (SCM de), "") {
	drawelement_ref ref = { scm_to_int(de) };
    shader_ref s = drawelement_shader(ref);
    return scm_from_int(s.id);
}

SCM_DEFINE(s_drawelement_material, "drawelement-material", 1, 0, 0, (SCM de), "") {
	drawelement_ref ref = { scm_to_int(de) };
    material_ref m = drawelement_material(ref);
    return scm_from_int(m.id);
}

SCM_DEFINE(s_find_drawelement, "find-drawelement", 1, 0, 0, (SCM name), "") {
    char *n = scm_to_locale_string(name);
    drawelement_ref ref = find_drawelement(n);
    free(n);
    return scm_from_int(ref.id);
}

SCM_DEFINE(s_de_trafo, "de-trafo", 1, 0, 0, (SCM de), "") {
    drawelement_ref ref = { scm_to_int(de) };
    return matrix4x4f_to_scm(drawelement_trafo(ref));
}

SCM_DEFINE(s_set_de_trafo_x, "set-de-trafo!", 2, 0, 0, (SCM de, SCM bv), "") {
    drawelement_ref ref = { scm_to_int(de) };
    scm_to_matrix4x4f(drawelement_trafo(ref), bv);
    return SCM_BOOL_T;
}

SCM_DEFINE(s_list_des, "list-drawelements", 0, 0, 0, (), "") {
    SCM list = scm_list_1(scm_from_locale_string(drawelements[0].name));
    for (int i = 1; i < next_drawelement_index; ++i)
        list = scm_cons(scm_from_locale_string(drawelements[i].name), list);
    return list;
}

SCM_DEFINE(s_glUniform3f, "gl:uniform3f", 4, 0, 0, (SCM loc, SCM x, SCM y, SCM z), "") {
	int l = scm_to_int(loc);
	float X = scm_to_double(x),
		  Y = scm_to_double(y),
		  Z = scm_to_double(z);
	glUniform3f(l, X, Y, Z);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_de_name, "drawelement-name", 1, 0, 0, (SCM de), "") {
    drawelement_ref ref = { scm_to_int(de) };
    return scm_from_locale_string(drawelement_name(ref));
}

SCM_DEFINE(s_de_idxbuf_range, "drawelement-index-buffer-range!", 3, 0, 0, (SCM id, SCM p, SCM l), "") {
    drawelement_ref ref = { scm_to_int(id) };
	unsigned int pos = scm_to_uint(p),
				 len = scm_to_uint(l);
	set_drawelement_index_buffer_range(ref, pos, len);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_de_setbb, "drawelement-bounding-box!", 3, 0, 0, (SCM id, SCM bbmin, SCM bbmax), "") {
    drawelement_ref ref = { scm_to_int(id) };
	vec3f bbmi = scm_vec_to_vec3f(bbmin),
		  bbma = scm_vec_to_vec3f(bbmin);
	set_drawelement_bounding_box(ref, &bbmi, &bbma);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_de_change_shader, "change-drawelement-shader", 2, 0, 0, (SCM de_ref, SCM sh_ref), "") {
    drawelement_ref de = { scm_to_int(de_ref) };
    shader_ref sh = { scm_to_int(sh_ref) };
	shader_ref old = drawelement_change_shader(de, sh);
	return scm_from_int(old.id);
}

SCM_DEFINE(s_TMP_mem_barr, "memory-barrier!!", 0, 0, 0, (), "") {
    glMemoryBarrier(GL_ALL_BARRIER_BITS);
    return SCM_BOOL_T;
}


void register_scheme_functions_for_drawelement() {
#include "drawelement.x"
}

#endif

