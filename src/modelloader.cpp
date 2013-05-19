#include "modelloader.h"

#include "material.h"
#include "basename.h"
#include <libgen.h>

#include <libcgl/impex.h>

#include <iostream>
#include <sstream>
#include <iomanip>

#include <assimp/assimp.hpp>
#include <assimp/aiScene.h>
#include <assimp/aiPostProcess.h>
#include <assimp/types.h>

#include "skeletal.h"
#include "stock-shader.h"

using namespace std;
using namespace Assimp;

extern "C" {
	void add_texture_if_found(material_ref mat, const char *filename, tex_params_t *p, const char *texname);
}

vec4f col_to_vec4f(const aiColor4D &c) {
	vec4f col = { c.r, c.g, c.b, c.a };
	return col;
}

vec3f ass_imp_vec3_to_vec3f(const aiVector3D &v) {
	vec3f ret = { v.x, v.y, v.z };
	return ret;
}

void ass_imp_mat4_to_matrix4x4f(matrix4x4f *to, const aiMatrix4x4 &from) {
	to->col_major[0] = from.a1; to->col_major[4] = from.a2; to->col_major[8]  = from.a3; to->col_major[12] = from.a4;
	to->col_major[1] = from.b1; to->col_major[5] = from.b2; to->col_major[9]  = from.b3; to->col_major[13] = from.b4;
	to->col_major[2] = from.c1; to->col_major[6] = from.c2; to->col_major[10] = from.c3; to->col_major[14] = from.c4;
	to->col_major[3] = from.d1; to->col_major[7] = from.d2; to->col_major[11] = from.d3; to->col_major[15] = from.d4;
}

aiBone* find_bone(aiScene const *model_scene, aiString name) {
	for (int m = 0; m < model_scene->mNumMeshes; ++m)
		if (model_scene->mMeshes[m]->HasBones())
			for (int b = 0; b < model_scene->mMeshes[m]->mNumBones; ++b)
				if (model_scene->mMeshes[m]->mBones[b]->mName == name)
					return model_scene->mMeshes[m]->mBones[b];
	return 0;
}

bone_list* traverse_nodes(aiScene const *model_scene, aiNode *node, aiMatrix4x4 curr_trafo) {
	curr_trafo = curr_trafo * node->mTransformation;
	bone_list *ret = 0;
	for (int c = 0; c < node->mNumChildren; ++c) {
		bone_list *childs_bones = traverse_nodes(model_scene, node->mChildren[c], curr_trafo);
		bone_list *tmp = childs_bones;
		while (childs_bones) {
			tmp = childs_bones;
			childs_bones = childs_bones->next;
			tmp->next = ret;
			ret = tmp;
		}
	}
	aiBone *found_bone = find_bone(model_scene, node->mName);
	if (found_bone) {
		struct bone *bone = (struct bone*)malloc(sizeof(struct bone));
		ass_imp_mat4_to_matrix4x4f(&bone->rest_trafo, curr_trafo);
		bone->children = ret;
		bone->name = strdup((char*)found_bone->mName.data);
		ret = (bone_list*)malloc(sizeof(bone_list));
		ret->bone = bone;
		ret->next = 0;
	}
	return ret;
}

skeletal_animation_ref generate_skeletal_anim(aiScene const* model_scene) {
	aiNode *root = model_scene->mRootNode;
	aiMatrix4x4 curr_trafo;
	bone_list *bones = traverse_nodes(model_scene, root, curr_trafo);
	skeletal_animation_ref ref = make_skeletal_animation("test", bones);
	return ref;
}

void load_model_and_create_objects_with_separate_vbos(const char *filename, const char *object_name, vec3f *bb_min, vec3f *bb_max,
                                                      drawelement_ref (*make_drawelem)(const char*, mesh_ref, material_ref, vec3f *bbmin, vec3f *bbmax), material_ref fallback_material) {
	const char *modelname = object_name ? object_name : filename;

	char *dirname_tmp = strdup(filename);
	prepend_image_path(dirname(dirname_tmp));
	free(dirname_tmp);

	Assimp::Importer importer;
	aiScene const *model_scene;

	model_scene = importer.ReadFile(filename, aiProcessPreset_TargetRealtime_Quality); // includes triangulation
	
	// convert the materials
	// note: the material names are not prefixed until now.
	for (int i = 0; i < model_scene->mNumMaterials; ++i) {
		aiMaterial *mtl = model_scene->mMaterials[i];
		aiColor4D aa, ad, as;
		mtl->Get(AI_MATKEY_COLOR_AMBIENT, aa);
		mtl->Get(AI_MATKEY_COLOR_DIFFUSE, ad);
		mtl->Get(AI_MATKEY_COLOR_SPECULAR, as);
		vec4f col_amb = col_to_vec4f(aa);
		vec4f col_diff = col_to_vec4f(ad);
		vec4f col_spec = col_to_vec4f(as);
		std::string n;
		aiString an;
		mtl->Get(AI_MATKEY_NAME, an);	// nice to simply segfault on std::string... :/
		n = (const char*)an.data;
		material_ref mat = make_material((string(modelname) + "/" + n).c_str(), &col_amb, &col_diff, &col_spec);

		std::string texfile;
		tex_params_t p = default_tex_params();
		aiString doh;
		if (mtl->GetTexture(aiTextureType_AMBIENT, 0, &doh) == AI_SUCCESS)  add_texture_if_found(mat, (const char*)doh.data, &p, "ambient_tex");
		if (mtl->GetTexture(aiTextureType_DIFFUSE, 0, &doh) == AI_SUCCESS)  add_texture_if_found(mat, (const char*)doh.data, &p, "diffuse_tex");
		if (mtl->GetTexture(aiTextureType_SPECULAR, 0, &doh) == AI_SUCCESS) add_texture_if_found(mat, (const char*)doh.data, &p, "specular_tex");
		if (mtl->GetTexture(aiTextureType_OPACITY, 0, &doh) == AI_SUCCESS)  add_texture_if_found(mat, (const char*)doh.data, &p, "mask_tex");
		float s;
		mtl->Get(AI_MATKEY_SHININESS, s);
		material_set_specular_exponent(mat, s);
	}

	vec3f model_bbmi, model_bbma;

	skeletal_animation_ref anim = generate_skeletal_anim(model_scene);

	for (int i = 0; i < model_scene->mNumMeshes; ++i) {
		aiMesh *group = model_scene->mMeshes[i];
		int pos = 1;
		int norm = group->HasNormals() ? 1 : 0;
		int tc = group->HasTextureCoords(0) ? 1 : 0;
		int bones = group->HasBones() ? group->mNumBones : 0;
		bone **found_bones = new bone*[bones];
		for (int i = 0; i < bones; ++i)
			found_bones[i] = 0;

		mesh_ref m = make_mesh((string(modelname) + "/" + (const char*)group->mName.data).c_str(), pos+norm+tc+bones);
		bind_mesh_to_gl(m);

		int verts = group->mNumVertices;
		add_vertex_buffer_to_mesh(m, "in_pos", GL_FLOAT, verts, 3, group->mVertices, GL_STATIC_DRAW);
		if (norm)
			add_vertex_buffer_to_mesh(m, "in_norm", GL_FLOAT, verts, 3, group->mNormals, GL_STATIC_DRAW);
		if (tc) {
			vec2f *tc = new vec2f[group->mNumVertices];
			for (int i = 0; i < group->mNumVertices; ++i)
				tc[i].x = group->mTextureCoords[0][i].x,
				tc[i].y = group->mTextureCoords[0][i].y;
			add_vertex_buffer_to_mesh(m, "in_tc", GL_FLOAT, verts, 2, tc, GL_STATIC_DRAW);
			delete [] tc;
		}

		if (bones) {
			for (int b = 0; b < bones; ++b) {
				float *weights = new float[verts];
				aiBone *bone_data = group->mBones[b];
				cout << "BONE " << b << ": " << bone_data->mName.data << endl;
				if (bone_data->mNumWeights != verts)
					cerr << "   - - - >  number bone weights does not match vertex number!" << endl;
				for (int i = 0; i < verts; ++i)
					weights[i] = 0;
				for (int i = 0; i < bone_data->mNumWeights; ++i)
					weights[bone_data->mWeights[i].mVertexId] = bone_data->mWeights[i].mWeight;
				ostringstream oss; oss << "bone_weights_" << setw(2) << setfill('0') << b;
				add_vertex_buffer_to_mesh(m, oss.str().c_str(), GL_FLOAT, verts, 1, weights, GL_STATIC_DRAW);
				found_bones[b] = find_bone_in_skeletal_animation(anim, (char*)bone_data->mName.data);
			}
		}

		unsigned int *ids = new unsigned int[3 * group->mNumFaces];
		for (int i = 0; i < group->mNumFaces; ++i) {
			ids[3*i+0] = group->mFaces[i].mIndices[0];
			ids[3*i+1] = group->mFaces[i].mIndices[1];
			ids[3*i+2] = group->mFaces[i].mIndices[2];
		}
		add_index_buffer_to_mesh(m, group->mNumFaces*3, ids, GL_STATIC_DRAW);
		unbind_mesh_from_gl(m);


		aiMaterial *mm = model_scene->mMaterials[group->mMaterialIndex];
		std::string n;
		aiString doh;
		mm->Get(AI_MATKEY_NAME, doh);
		n = (const char*)doh.data;
		string addendum = bones ? string("+B-hack") : string("");	// This should really be done differently!
		std::string mat_name = string(modelname) + "/" + n;
		material_ref mat = find_material(mat_name.c_str());

		aiVector3D *v = group->mVertices;
		vec3f bbmi; vec3f *bb_min = &bbmi; *bb_min = ass_imp_vec3_to_vec3f(v[0]);
		vec3f bbma; vec3f *bb_max = &bbma; *bb_max = ass_imp_vec3_to_vec3f(v[0]);
		for (int i = 0; i < verts; ++i) {
			if (v[i].x < bb_min->x) bb_min->x = v[i].x;
			if (v[i].y < bb_min->y) bb_min->y = v[i].y;
			if (v[i].z < bb_min->z) bb_min->z = v[i].z;
			if (v[i].x > bb_max->x) bb_max->x = v[i].x;
			if (v[i].y > bb_max->y) bb_max->y = v[i].y;
			if (v[i].z > bb_max->z) bb_max->z = v[i].z;
		}
		if (i == 0) {
			model_bbmi = bbmi;
			model_bbma = bbma;
		}
		else {
			if (bbmi.x < model_bbmi.x) model_bbmi.x = bbmi.x;
			if (bbmi.y < model_bbmi.y) model_bbmi.y = bbmi.y;
			if (bbmi.z < model_bbmi.z) model_bbmi.z = bbmi.z;
			if (bbma.x > model_bbma.x) model_bbma.x = bbma.x;
			if (bbma.y > model_bbma.y) model_bbma.y = bbma.y;
			if (bbma.z > model_bbma.z) model_bbma.z = bbma.z;
		}

		drawelement_ref de = make_drawelem(mesh_name(m), m, mat, bb_min, bb_max);
		if (!valid_drawelement_ref(de))
			continue;

		make_drawelement_part_of_skeletal_animation(de, anim);
		assign_bones_to_drawelement(de, bones, found_bones);

		if (!valid_shader_ref(drawelement_shader(de))) {
			shader_ref shader = make_stock_shader(0, de, 0, true);
			drawelement_change_shader(de, shader);
		}
	}

	*bb_min = model_bbmi;
	*bb_max = model_bbma;

	pop_image_path_front();
}

/* vim: set foldmethod=marker: */

