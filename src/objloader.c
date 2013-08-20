#include "objloader.h"

#include "drawelement.h"
#include "basename.h"
#include "scene.h"
#include "cmdline.h"
#include "stock-shader.h"


#include <libcgl/libcgl.h>

#include <stdlib.h>

#include <stdio.h>

#include "basename.h"
#include <libgen.h>


#include "modelloader.h"	//TMP

/*! \defgroup objloading Obj File Loading
 *
 *	\see objloader.h
 *
 *	Loading of obj files is actually starting a cascade of obj loader steps,
 *  	the first one being defined in libobjloader,
 *		the second one executed by libcgl,
 *		finally leading to us.
 * 
 * 	The method of choice is \ref load_objfile_and_create_objects_with_single_vbo.
 *	There is still \ref load_objfile_and_create_objects_with_separate_vbos but it is rendered far slower (naturally) and no longer actively maintained.
 */


void add_texture_if_found(material_ref mat, const char *filename, tex_params_t *p, const char *texname) {
	printf("---------------------------------- looking for %s\n", filename);
	char *fn = find_file(filename);
	if (fn) {
		texture_ref tex = find_texture_by_filename(fn);
		if (!valid_texture_ref(tex))
			tex = make_texture(filename, fn, GL_TEXTURE_2D, p);
		material_add_texture_as(mat, tex, texname);
	}
	else
		fprintf(stderr, "Cannot find texture named '%s' in any registered search directory.\n", filename);
}

// objectname may be 0, in which case the filename will be used to prefix the generated objects.
/*!	\brief Load an obj file and create a separate vbo for each submesh.
 *	\ingroup objloading
 *
 *	See \ref load_objfile_and_create_objects_with_single_vbo for a more 'detailled' description.
 *	\deprecated use \ref load_objfile_and_create_objects_with_single_vbo.
 */
void load_objfile_and_create_objects_with_separate_vbos(const char *filename, const char *object_name, vec3f *bb_min, vec3f *bb_max, 
                                                        drawelement_ref (*make_drawelem)(const char*, mesh_ref, material_ref, vec3f *bbmin, vec3f *bbmax), material_ref fallback_material) {

// 	load_model_and_create_objects_with_separate_vbos(filename, object_name, bb_min, bb_max, make_drawelem, fallback_material);
// 	return;

	obj_data objdata;
	const char *modelname = object_name ? object_name : filename;
	time_t start = clock();
	
	char *dirname_tmp = strdup(filename);
	prepend_image_path(dirname(dirname_tmp));
	free(dirname_tmp);

	load_objfile(modelname, filename, &objdata, false, false, 0);

	time_t mid = clock();

	// convert the materials
	// note: the material names are already prefixed with the model's base name.
	for (int i = 0; i < objdata.number_of_materials; ++i) {
		obj_mtl *m = objdata.materials+i;
		material_ref mat = make_material(m->name, &m->col_amb, &m->col_diff, &m->col_spec);
		tex_params_t p = default_tex_params();
		if (m->tex_a) add_texture_if_found(mat, m->tex_a, &p, "ambient_tex");
		if (m->tex_d) add_texture_if_found(mat, m->tex_d, &p, "diffuse_tex");
		if (m->tex_s) add_texture_if_found(mat, m->tex_s, &p, "specular_tex");
		if (m->tex_alpha) add_texture_if_found(mat, m->tex_alpha, &p, "mask_tex");
		material_set_specular_exponent(mat, m->spec_exp);
	}

	// todo: vertex-buffer sharing
	for (int i = 0; i < objdata.number_of_groups; ++i) {
		obj_group *group = objdata.groups + i;

		int pos = 1,
			norm = 1,
			tc = group->t_ids ? 1 : 0;
		mesh_ref m = make_mesh(group->name, pos+norm+tc);
		bind_mesh_to_gl(m);

		int verts = group->triangles*3;
		vec3f *v = malloc(sizeof(vec3f)*verts);
		vec3f *n = malloc(sizeof(vec3f)*verts);
		vec2f *t = tc ? malloc(sizeof(vec2f)*verts) : 0;

		unsigned int *indices = malloc(sizeof(unsigned int)*verts);
		for (int tri = 0; tri < group->triangles; ++tri) {
			v[3*tri+0] = objdata.vertex_data[group->v_ids[tri].x];
			v[3*tri+1] = objdata.vertex_data[group->v_ids[tri].y];
			v[3*tri+2] = objdata.vertex_data[group->v_ids[tri].z];
			n[3*tri+0] = objdata.normal_data[group->n_ids[tri].x];
			n[3*tri+1] = objdata.normal_data[group->n_ids[tri].y];
			n[3*tri+2] = objdata.normal_data[group->n_ids[tri].z];
			if (t) {
				t[3*tri+0] = objdata.texcoord_data[group->t_ids[tri].x];
				t[3*tri+1] = objdata.texcoord_data[group->t_ids[tri].y];
				t[3*tri+2] = objdata.texcoord_data[group->t_ids[tri].z];
			}
			indices[3*tri+0] = 3*tri+0;
			indices[3*tri+1] = 3*tri+1;
			indices[3*tri+2] = 3*tri+2;
		}
		add_vertex_buffer_to_mesh(m, "in_pos", GL_FLOAT, verts, 3, v, GL_STATIC_DRAW);
		add_vertex_buffer_to_mesh(m, "in_norm", GL_FLOAT, verts, 3, n, GL_STATIC_DRAW);
		if (t) add_vertex_buffer_to_mesh(m, "in_tc", GL_FLOAT, verts, 2, t, GL_STATIC_DRAW);
		add_index_buffer_to_mesh(m, verts, indices, GL_STATIC_DRAW);
		unbind_mesh_from_gl(m);

		material_ref mat;
		if (group->mtl) mat = find_material(group->mtl->name);
		else            mat = fallback_material;

		vec3f bbmi; vec3f *bb_min = &bbmi; *bb_min = v[0];
		vec3f bbma; vec3f *bb_max = &bbma; *bb_max = v[0];
		for (int i = 0; i < verts; ++i) {
			if (v[i].x < bb_min->x) bb_min->x = v[i].x;
			if (v[i].y < bb_min->y) bb_min->y = v[i].y;
			if (v[i].z < bb_min->z) bb_min->z = v[i].z;
			if (v[i].x > bb_max->x) bb_max->x = v[i].x;
			if (v[i].y > bb_max->y) bb_max->y = v[i].y;
			if (v[i].z > bb_max->z) bb_max->z = v[i].z;
		}

		drawelement_ref de = make_drawelem(group->name, m, mat, bb_min, bb_max);
	
		free(v);
		free(n);
		free(t);
		free(indices);
		
		if (!valid_drawelement_ref(de))
			continue;
		
		if (!valid_shader_ref(drawelement_shader(de))) {
			shader_ref shader = make_stock_shader(0, de, 0, true);
			drawelement_change_shader(de, shader);
		}
	}
	
	if (bb_min && bb_max) {	// {{{
		*bb_min = objdata.vertex_data[0];
		*bb_max = objdata.vertex_data[0];
		for (int i = 0; i < objdata.vertices; ++i) {
			if (objdata.vertex_data[i].x < bb_min->x) bb_min->x = objdata.vertex_data[i].x;
			if (objdata.vertex_data[i].y < bb_min->y) bb_min->y = objdata.vertex_data[i].y;
			if (objdata.vertex_data[i].z < bb_min->z) bb_min->z = objdata.vertex_data[i].z;
			if (objdata.vertex_data[i].x > bb_max->x) bb_max->x = objdata.vertex_data[i].x;
			if (objdata.vertex_data[i].y > bb_max->y) bb_max->y = objdata.vertex_data[i].y;
			if (objdata.vertex_data[i].z > bb_max->z) bb_max->z = objdata.vertex_data[i].z;
		}
	}	// }}}

	time_t end = clock();

	printf("load: %d\n", (int)((mid-start)/CLOCKS_PER_SEC));
	printf("post: %d\n", (int)((end-mid)/CLOCKS_PER_SEC));

	pop_image_path_front();
}

/*! \brief Callback you have to provide to actually create drawelements while loading an obj file.
 *	\note This function is listed for documentation purposes, only. It does not exist.
 *	\ingroup objloading
 *
 *	The called function will have to decide if and how to create a drawelement for a given submesh, and wether or not to add it to the scene.
 *	Here is a small Scheme snipped (taken from default.c.scm) showing the default implementation (which uses a stock-shader):
 *	\code
 *  (define (make-de name mesh material)
 *    (material-use-stock-shader! material) ;; a little out of date...
 *    (let* ((shader (material-shader material))
 *           (de (make-drawelement name mesh shader material)))
 *      (prepend-uniform-handler de 'default-matrix-uniform-handler)
 *      (prepend-uniform-handler de 'default-material-uniform-handler)
 *      (add-drawelement-to-scene the-scene de)
 *      de))
 *  
 *  (define (make-de-idx name mesh material pos len)
 *    (let ((de (make-de name mesh material)))
 *      (drawelement-index-buffer-range! de pos len)))
 *	\endcode
 *	Note that these are two versions, implementing the same logic for both <tt>load_objfile_*</tt> calls.
 *
 */
static drawelement_ref example_make_drawelem(const char *name, mesh_ref mesh, material_ref mat, unsigned int start, unsigned int len) {}

static void load_objfile_and_create_objects_with_single_vbo_general(
                        const char *filename, const char *object_name, vec3f *bb_min, vec3f *bb_max,
                        drawelement_ref (*make_drawelem)(const char*, mesh_ref, material_ref, unsigned int start, unsigned int len, vec3f *bbmin, vec3f *bbmax),
                        material_ref fallback_material, bool keep_meshes_on_cpu);
/*! \brief Load and obj file and store all data in a single vbo, creating indexed drawelements.
 *	\ingroup objloading
 *
 *	\note Loading an obj file usually entails the loading of textures as well. 
 *			So be sure to register your image paths.
 *			The path the obj file is found in is automatically added to the image search path, while loading textures for that model.
 *
 *	\param filename The file to load.
 *	\param object_name The name of the model (prepended to all submeshes), defaults to filename if 0.
 *	\param bb_min Returns the min-part of the bb.
 *	\param bb_max Returns the max-part of the bb.
 *	\param make_drawelem This function is called for each sub mesh of the model. Its job is to actually create the drawelement. See \ref example_make_drawelem.
 *	\param fallback_material The material to be used should there be a sub mesh for which we can't find a material.
 */
void load_objfile_and_create_objects_with_single_vbo(	// i really don't like writing it with such strange indent, the name, however, is just too long...
                  const char *filename, const char *object_name, vec3f *bb_min, vec3f *bb_max,
                  drawelement_ref (*make_drawelem)(const char*, mesh_ref, material_ref, unsigned int start, unsigned int len, vec3f *bbmin, vec3f *bbmax),
                  material_ref fallback_material) {
	load_objfile_and_create_objects_with_single_vbo_general(filename, object_name, bb_min, bb_max, make_drawelem, fallback_material, false);
}

void load_objfile_and_create_objects_with_single_vbo_keeping_cpu_data(
                  const char *filename, const char *object_name, vec3f *bb_min, vec3f *bb_max,
                  drawelement_ref (*make_drawelem)(const char*, mesh_ref, material_ref, unsigned int start, unsigned int len, vec3f *bbmin, vec3f *bbmax),
                  material_ref fallback_material) {
	load_objfile_and_create_objects_with_single_vbo_general(filename, object_name, bb_min, bb_max, make_drawelem, fallback_material, true);
}

static void load_objfile_and_create_objects_with_single_vbo_general(
                        const char *filename, const char *object_name, vec3f *bb_min, vec3f *bb_max,
                        drawelement_ref (*make_drawelem)(const char*, mesh_ref, material_ref, unsigned int start, unsigned int len, vec3f *bbmin, vec3f *bbmax),
                        material_ref fallback_material, bool keep_meshes_on_cpu) {
	obj_data objdata;
	const char *modelname = object_name ? object_name : filename;

	char *dirname_tmp = strdup(filename);
	prepend_image_path(dirname(dirname_tmp));
	free(dirname_tmp);

	time_t start = clock();

	bool collapse_materials = true;
	load_objfile(modelname, filename, &objdata, true, collapse_materials, cmdline.collapse_factor);
	
	time_t mid = clock();

	// convert the materials
	// note: the material names are already prefixed with the model's base name.
	for (int i = 0; i < objdata.number_of_materials; ++i) {
		obj_mtl *m = objdata.materials+i;
		material_ref mat = make_material(m->name, &m->col_amb, &m->col_diff, &m->col_spec);
		tex_params_t p = default_tex_params();
		if (m->tex_a) add_texture_if_found(mat, m->tex_a, &p, "ambient_tex");
		if (m->tex_d) add_texture_if_found(mat, m->tex_d, &p, "diffuse_tex");
		if (m->tex_s) add_texture_if_found(mat, m->tex_s, &p, "specular_tex");
		if (m->tex_alpha) add_texture_if_found(mat, m->tex_alpha, &p, "mask_tex");
		material_set_specular_exponent(mat, m->spec_exp);
	}

    int comps = 1;
    if (objdata.normals) comps++;
    if (objdata.texcoords) comps++;
    mesh_ref m = make_mesh(modelname, comps);
	if (keep_meshes_on_cpu)
		mesh_keep_cpu_data(m);
	bind_mesh_to_gl(m);

	add_vertex_buffer_to_mesh(m, "in_pos", GL_FLOAT, objdata.vertices, 3, objdata.vertex_data, GL_STATIC_DRAW);
	if (objdata.normals)
		add_vertex_buffer_to_mesh(m, "in_norm", GL_FLOAT, objdata.normals, 3, objdata.normal_data, GL_STATIC_DRAW);
	if (objdata.texcoords)
		add_vertex_buffer_to_mesh(m, "in_tc", GL_FLOAT, objdata.texcoords, 2, objdata.texcoord_data, GL_STATIC_DRAW);

    int indices = 0;

	for (int i = 0; i < objdata.number_of_groups; ++i) {
		obj_group *group = objdata.groups + i;
		indices += group->triangles * 3;
	}

	int *index_buffer = malloc(sizeof(int)*indices);
	int offset = 0;
	for (int i = 0; i < objdata.number_of_groups; ++i) {
		obj_group *group = objdata.groups + i;
		memcpy(index_buffer+offset, group->v_ids, sizeof(int)*group->triangles*3);
		offset += group->triangles * 3;
	}

	add_index_buffer_to_mesh(m, indices, index_buffer, GL_STATIC_DRAW);
	unbind_mesh_from_gl(m);

	int pos = 0;
	for (int i = 0; i < objdata.number_of_groups; ++i) {
		obj_group *g = objdata.groups + i;
		material_ref mat;
		if (g->mtl) mat = find_material(g->mtl->name);
		else        mat = fallback_material;

		vec3f bbmi; vec3f *bb_min = &bbmi; *bb_min = objdata.vertex_data[index_buffer[pos]];
		vec3f bbma; vec3f *bb_max = &bbma; *bb_max = objdata.vertex_data[index_buffer[pos]];
		for (int i = 0; i < g->triangles*3; ++i) {
			int id = index_buffer[pos+i];
			if (objdata.vertex_data[id].x < bb_min->x) bb_min->x = objdata.vertex_data[id].x;
			if (objdata.vertex_data[id].y < bb_min->y) bb_min->y = objdata.vertex_data[id].y;
			if (objdata.vertex_data[id].z < bb_min->z) bb_min->z = objdata.vertex_data[id].z;
			if (objdata.vertex_data[id].x > bb_max->x) bb_max->x = objdata.vertex_data[id].x;
			if (objdata.vertex_data[id].y > bb_max->y) bb_max->y = objdata.vertex_data[id].y;
			if (objdata.vertex_data[id].z > bb_max->z) bb_max->z = objdata.vertex_data[id].z;
		}

		drawelement_ref de = make_drawelem(g->name, m, mat, pos, g->triangles*3, bb_min, bb_max);
		pos += g->triangles*3;
		
		if (!valid_drawelement_ref(de))
			continue;
		
		if (!valid_shader_ref(drawelement_shader(de))) {
			shader_ref shader = make_stock_shader(0, de, 0, true);
			drawelement_change_shader(de, shader);
		}
	}
	
	free(index_buffer);

	
	if (bb_min && bb_max) {
		*bb_min = objdata.vertex_data[0];
		*bb_max = objdata.vertex_data[0];
		for (int i = 0; i < objdata.vertices; ++i) {
			if (objdata.vertex_data[i].x < bb_min->x) bb_min->x = objdata.vertex_data[i].x;
			if (objdata.vertex_data[i].y < bb_min->y) bb_min->y = objdata.vertex_data[i].y;
			if (objdata.vertex_data[i].z < bb_min->z) bb_min->z = objdata.vertex_data[i].z;
			if (objdata.vertex_data[i].x > bb_max->x) bb_max->x = objdata.vertex_data[i].x;
			if (objdata.vertex_data[i].y > bb_max->y) bb_max->y = objdata.vertex_data[i].y;
			if (objdata.vertex_data[i].z > bb_max->z) bb_max->z = objdata.vertex_data[i].z;
		}
	}
	time_t end = clock();

	printf("OBJ TIMINGS:\n");
	printf("load: %d\n", (int)((mid-start)/CLOCKS_PER_SEC));
	printf("post: %d\n", (int)((end-mid)/CLOCKS_PER_SEC));


	pop_image_path_front();
}

#ifdef WITH_GUILE
#include <libguile.h>
	
#pragma GCC diagnostic ignored "-Wtrampolines"

bool custom_light_handler(drawelement_ref ref, const char *uniform, int location);

SCM_DEFINE(s_load_objfile_and_create_objects_with_separate_vbos,
           "load-objfile-and-create-objects-with-separate-vbos", 4, 0, 0, (SCM filename, SCM object_name, SCM callback, SCM fallback_mat), "") {
	char *f = scm_to_locale_string(filename);
	char *o = scm_to_locale_string(object_name);
	drawelement_ref create_drawelement_forwarder(const char *modelname, mesh_ref mesh, material_ref mat, vec3f *bmi, vec3f *bma) {
		SCM id = scm_call_5(callback, scm_from_locale_string(modelname), scm_from_int(mesh.id), scm_from_int(mat.id), vec3f_to_list(bmi), vec3f_to_list(bma));
		drawelement_ref ref = { scm_to_int(id) };
		return ref;
	}
	vec3f min, max;
	vec4f amb = {1,0,0,1}, diff = {1,0,0,1}, spec = {1,0,0,1};
	material_ref fallback = { scm_to_int(fallback_mat) };
	load_objfile_and_create_objects_with_separate_vbos(f, o, &min, &max, create_drawelement_forwarder, fallback);
	return scm_values(scm_list_2(vec3f_to_list(&min), vec3f_to_list(&max)));
}

SCM_DEFINE(s_load_objfile_and_create_objects_with_single_vbos_general,
           "load-objfile-and-create-objects-with-single-vbo-general", 5, 0, 0,
		   (SCM filename, SCM object_name, SCM callback, SCM fallback_mat, SCM keep_cpu_data), "") {
	char *f = scm_to_locale_string(filename);
	char *o = scm_to_locale_string(object_name);
	bool keep = scm_is_true(keep_cpu_data);
	drawelement_ref create_drawelement_forwarder(const char *modelname, mesh_ref mesh, material_ref mat, unsigned int pos, unsigned int len, vec3f *bmi, vec3f *bma) {
		SCM id = scm_call_7(callback, scm_from_locale_string(modelname), scm_from_int(mesh.id), scm_from_int(mat.id), scm_from_uint(pos), scm_from_uint(len), vec3f_to_list(bmi), vec3f_to_list(bma));
		drawelement_ref ref = { scm_to_int(id) };
		return ref;
	}
	vec3f min, max;
	vec4f amb = {1,0,0,1}, diff = {1,0,0,1}, spec = {1,0,0,1};
	material_ref fallback = { scm_to_int(fallback_mat) };
	load_objfile_and_create_objects_with_single_vbo_general(f, o, &min, &max, create_drawelement_forwarder, fallback, keep);
	return scm_values(scm_list_2(vec3f_to_list(&min), vec3f_to_list(&max)));
}

void register_scheme_functions_for_cgls_objloader() {
#ifndef SCM_MAGIC_SNARFER
#include "objloader.x"
#endif
	scm_c_eval_string("(define (load-objfile-and-create-objects-with-single-vbo filename objname callback fallback-mat)\
						 (format #t \"~a ~a ~a ~a #f\" filename objname callback fallback-mat)\
	                     (load-objfile-and-create-objects-with-single-vbo-general filename objname callback fallback-mat #f))");
	scm_c_eval_string("(define (load-objfile-and-create-objects-with-single-vbo-keeping-cpu-data filename objname callback fallback-mat)\
						 (format #t \"~a ~a ~a ~a #t\" filename objname callback fallback-mat)\
	                     (load-objfile-and-create-objects-with-single-vbo-general filename objname callback fallback-mat #t))");
}

#endif

// vim: set foldmethod=marker :
