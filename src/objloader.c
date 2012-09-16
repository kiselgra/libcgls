#include "drawelement.h"
#include "basename.h"
#include "scene.h"
#include "cmdline.h"


#include <libcgl/libcgl.h>

#include <stdlib.h>

#include <stdio.h>

// objectname may be 0, in which case the filename will be used to prefix the generated objects.
void load_objfile_and_create_objects_with_separate_vbos(const char *filename, const char *object_name, vec3f *bb_min, vec3f *bb_max, 
                                                        void (*make_drawelem)(const char*, mesh_ref, material_ref), material_ref fallback_material) {
	obj_data objdata;
	const char *modelname = object_name ? object_name : filename;
	load_objfile(modelname, filename, &objdata);

	// convert the materials
	// note: the material names are already prefixed with the model's base name.
	for (int i = 0; i < objdata.number_of_materials; ++i) {
		obj_mtl *m = objdata.materials+i;
		material_ref mat = make_material(m->name, &m->col_amb, &m->col_diff, &m->col_spec);
		tex_params_t p = default_tex_params();
		if (m->tex_a) material_add_texture(mat, make_texture(basename(m->tex_a), m->tex_a, GL_TEXTURE_2D, &p));
		if (m->tex_d) material_add_texture(mat, make_texture(basename(m->tex_d), m->tex_d, GL_TEXTURE_2D, &p));
		if (m->tex_s) material_add_texture(mat, make_texture(basename(m->tex_s), m->tex_s, GL_TEXTURE_2D, &p));
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
		make_drawelem(modelname, m, mat);
	
		free(v);
		free(n);
		free(t);
		free(indices);

		// - create material
		// - call mesh created handler (with mesh and mat, should create shader and de)
	}
	
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
}

#ifdef WITH_GUILE
#include <libguile.h>

bool custom_light_handler(drawelement_ref ref, const char *uniform, int location);

SCM_DEFINE(s_load_objfile_and_create_objects_with_separate_vbos,
           "load-objfile-and-create-objects-with-separate-vbos", 4, 0, 0, (SCM filename, SCM object_name, SCM callback, SCM fallback_mat), "") {
	printf("-------loading obj from scheme\n");
	char *f = scm_to_locale_string(filename);
	char *o = scm_to_locale_string(object_name);
	void create_drawelement_forwarder(const char *modelname, mesh_ref mesh, material_ref mat) {
		/*
		shader_ref s;
		if (cmdline.hemi)
			if (material_textures(mat)) s = find_shader("diffuse-hemi+tex");
			else                        s = find_shader("diffuse-hemi");
		else
			if (material_textures(mat)) s = find_shader("diffuse-dl+tex");
			else                        s = find_shader("diffuse-dl");
		drawelement_ref de = make_drawelement(modelname, mesh, s, mat);
		prepend_uniform_handler(de, default_material_uniform_handler);
		prepend_uniform_handler(de, default_matrix_uniform_handler);
		prepend_uniform_handler(de, custom_light_handler);
		scene_add_drawelement(scene, de);
		*/
		scm_call_3(callback, scm_from_locale_string(modelname), scm_from_int(mesh.id), scm_from_int(mat.id));
	}
	vec3f min, max;
	vec4f amb = {1,0,0,1}, diff = {1,0,0,1}, spec = {1,0,0,1};
	material_ref fallback = { scm_to_int(fallback_mat) };
	load_objfile_and_create_objects_with_separate_vbos(f, o, &min, &max, create_drawelement_forwarder, fallback);
	return scm_values(scm_list_2(vec3f_to_list(&min), vec3f_to_list(&max)));
}

void register_scheme_functions_for_cgls_objloader() {
#ifndef SCM_MAGIC_SNARFER
#include "objloader.x"
#endif
}

#endif
