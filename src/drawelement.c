#include "drawelement.h"

#include <string.h>
#include <stdlib.h>

#include <stdio.h>

struct handler_node {
	struct handler_node *next;
	uniform_setter_t handler;
};

#ifdef WITH_GUILE
#include <libguile.h>
struct scheme_handler_node {
	struct scheme_handler_node *next;
	SCM handler;
};
#endif


struct drawelement {
	char *shortname, *name;
	mesh_ref mesh;
	shader_ref shader;
	material_ref material;
	struct handler_node *handler_chain;
#ifdef WITH_GUILE
	struct scheme_handler_node *scheme_handler_chain;
#endif
	matrix4x4f trafo;
};

#define TYPE drawelement
#define ARRAY drawelements
#define REF drawelement_ref
#include <libcgl/mm.h>

drawelement_ref make_drawelement(const char *modelname, mesh_ref mr, shader_ref sr, material_ref matr) {
	drawelement_ref ref = allocate_ref();
	struct drawelement *de = drawelements+ref.id;

	int meshname_len = strlen(mesh_name(mr));
	de->shortname = malloc(meshname_len+1);
	strcpy(de->shortname, mesh_name(mr));

	if (modelname) {
		int modelname_len = strlen(modelname);
		de->name = malloc(modelname_len + 1 + meshname_len + 1);
		strcpy(de->name, modelname);
		strcpy(de->name+modelname_len, "/");
		strcpy(de->name+modelname_len+1, de->shortname);
	}
	else
		de->name = strdup(de->shortname);
		
	de->mesh = mr;
	de->shader = sr;
	de->material = matr;

	de->handler_chain = 0;
#ifdef WITH_GUILE
	de->scheme_handler_chain = 0;
#endif
	make_unit_matrix4x4f(&de->trafo);

// 	printf("create drawelement %s.\n", de->name);
// 	printf("       drawelement %s.\n", modelname);
// 	printf("       drawelement %s.\n", mesh_name(mr));
	return ref;
}

const char* drawelement_name(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;
	return de->name;
}

matrix4x4f* drawelement_trafo(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;
	return &de->trafo;
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


// uniform handlers

#define str_eq(X, Y) (strcmp(X, Y) == 0)

bool default_matrix_uniform_handler(drawelement_ref ref, const char *uniform, int location) {
	if (str_eq(uniform, "proj"))
		glUniformMatrix4fv(location, 1, GL_FALSE, projection_matrix_of_cam(current_camera())->col_major);
	else if (str_eq(uniform, "view"))
		glUniformMatrix4fv(location, 1, GL_FALSE, gl_view_matrix_of_cam(current_camera())->col_major);
	else if (str_eq(uniform, "normal_matrix"))
		glUniformMatrix4fv(location, 1, GL_FALSE, gl_normal_matrix_for_view_of(current_camera())->col_major);
	else if (str_eq(uniform, "model"))
		glUniformMatrix4fv(location, 1, GL_FALSE, drawelement_trafo(ref)->col_major);
	else
		return false;
	return true;
}

#undef str_eq

// may also be abused to get called just before the drawelement is rendered. ;)
void prepend_uniform_handler(drawelement_ref ref, uniform_setter_t handler) {
	struct drawelement *de = drawelements+ref.id;
	struct handler_node *cdr = de->handler_chain;
	de->handler_chain = malloc(sizeof(struct handler_node));
	de->handler_chain->handler = handler;
	de->handler_chain->next = cdr;
}

// 

void render_drawelement(drawelement_ref ref) {
	struct drawelement *de = drawelements + ref.id;

	bind_shader(de->shader);

	for (int i = 0; i < shader_uniforms(de->shader); ++i) {
		const char *name = shader_uniform_name_by_id(de->shader, i);
		int loc = shader_uniform_location_by_id(de->shader, i);
		struct handler_node *run = de->handler_chain;
		while (run && !run->handler(ref, name, loc))
			run = run->next;
#ifdef WITH_GUILE
		struct scheme_handler_node *s_run = de->scheme_handler_chain;
		SCM s_name = scm_from_locale_string(name);
		while (s_run && scm_is_false(scm_call_3(s_run->handler, scm_from_int(ref.id), s_name, scm_from_int(loc))))
			s_run = s_run->next;
		if (s_run) run = (void*)1; // this is not very nice...
#endif
		if (!run)
			printf("WARNING: cannot find a handler for uniform %s of shader %s when attached to drawelement %s.\n", 
					name, shader_name(de->shader), de->name);
	}

	bind_mesh_to_gl(de->mesh);
	draw_mesh(de->mesh, GL_TRIANGLES);
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

drawelement_ref find_drawelement(const char *name) {
	drawelement_ref ref = { -1 };
	if (strlen(name) == 0) return ref;
	for (int i = 0; i < next_index; ++i) {
//         printf("is %s == %s?\n", name, drawelements[i].shortname);
		if (strcmp(drawelements[i].shortname, name) == 0) {
			ref.id = i;
			return ref;
		}
	}
    for (int i = 0; i < next_index; ++i) {
//         printf("is %s == %s?\n", name, drawelements[i].name);
        if (strcmp(drawelements[i].name, name) == 0) {
			ref.id = i;
			return ref;
		}
	}
	return ref;
}

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
			prepend_uniform_handler(ref, default_material_uniform_handler);
		else if (strcmp(symbol, "default-matrix-uniform-handler") == 0)
			prepend_uniform_handler(ref, default_matrix_uniform_handler);
		else
			scm_throw(scm_from_locale_symbol("prepend-uniform-handler-error"), scm_list_2(scm_from_locale_string("invalid key"), handler));
	}
	else {
		struct scheme_handler_node *node = malloc(sizeof(struct scheme_handler_node));
		node->next = de->scheme_handler_chain;
		de->scheme_handler_chain = node;
		node->handler = handler;
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
    SCM bv = scm_c_make_bytevector(16*4);
    drawelement_ref ref = { scm_to_int(de) };
    memcpy(SCM_BYTEVECTOR_CONTENTS(bv), drawelement_trafo(ref), 4*16);
    return bv;
}

SCM_DEFINE(s_set_de_trafo_x, "set-de-trafo!", 2, 0, 0, (SCM de, SCM bv), "") {
    if (!scm_is_bytevector(bv))
        scm_throw(scm_from_locale_symbol("matrix-error"), scm_list_2(scm_from_locale_string("not a bytevector"), bv));
    if (scm_c_bytevector_length(bv) != 4 * 16)
        scm_throw(scm_from_locale_symbol("matrix-error"), scm_list_2(scm_from_locale_string("bytevector of invalid size"), bv));

    drawelement_ref ref = { scm_to_int(de) };
    memcpy(drawelement_trafo(ref), SCM_BYTEVECTOR_CONTENTS(bv), 4*16);

    return SCM_BOOL_T;
}

SCM_DEFINE(s_list_des, "list-drawelements", 0, 0, 0, (), "") {
    SCM list = scm_list_1(scm_from_locale_string(drawelements[0].shortname));
    for (int i = 1; i < next_index; ++i)
        list = scm_cons(scm_from_locale_string(drawelements[i].shortname), list);
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

void register_scheme_functions_for_drawelement() {
#include "drawelement.x"
}

#endif

