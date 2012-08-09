#include <libcgl/libcgl.h>

#include <stdlib.h>

#include <stdio.h>

// objectname may be 0, in which case the filename will be used to prefix the generated objects.
void load_objfile_and_create_objects_with_separate_vbos(const char *filename, const char *object_name) {
	obj_data objdata;
	const char *modelname = object_name ? object_name : filename;
	load_objfile(modelname, filename, &objdata);

	// todo: vertex-buffer sharing
	for (int i = 0; i < objdata.number_of_groups; ++i) {
		obj_group *group = objdata.groups + i;
		printf("group %d: %s.\n", i, group->name);

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

		shader_ref s = find_shader("diffuse-dl");
		make_drawelement(modelname, m, s);
	
		free(v);
		free(n);
		free(t);
		free(indices);

		// - return bb
		// - create material
		// - call mesh created handler (with mesh and mat, should create shader and de)
	}

}
