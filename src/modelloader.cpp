#include "modelloader.h"

#include "material.h"
#include "cgls-config.h"

#include "basename.h"
#include <libgen.h>

#include <libcgl/impex.h>

#include <iostream>
#include <sstream>
#include <iomanip>
#include <vector>

#if LIBCGLS_HAVE_ASSIMP_2 == 1
#include <assimp/assimp.hpp>
#include <assimp/aiScene.h>
#include <assimp/aiPostProcess.h>
#include <assimp/aiTypes.h>
#endif

#if LIBCGLS_HAVE_ASSIMP_3 == 1
#include <assimp/Importer.hpp>
#include <assimp/postprocess.h>
#include <assimp/scene.h>
#endif

#include "skeletal.h"
#include "stock-shader.h"

using namespace std;
#if LIBCGLS_HAVE_ASSIMP_2 == 1
using namespace Assimp;
#endif

#ifndef LIBCGLS_HAVE_ASSIMP_2
#ifndef LIBCGLS_HAVE_ASSIMP_3
#error NO ASS IMP.
#endif
#endif

extern "C" {
	void add_texture_if_found(material_ref mat, const char *filename, tex_params_t *p, const char *texname);
}

void show_anims(aiScene const *scene);
void add_animations(aiScene const *scene, skeletal_animation_ref skel_anim);

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

quaternionf ass_imp_quat_to_quatf(const aiQuaternion &in) {
	quaternionf q;
	q.v.x = in.x;
	q.v.y = in.y;
	q.v.z = in.z;
	q.w = in.w;
	return q;
}

aiBone* find_bone(aiScene const *model_scene, aiString name) {
	for (int m = 0; m < model_scene->mNumMeshes; ++m)
		if (model_scene->mMeshes[m]->HasBones())
			for (int b = 0; b < model_scene->mMeshes[m]->mNumBones; ++b)
				if (model_scene->mMeshes[m]->mBones[b]->mName == name)
					return model_scene->mMeshes[m]->mBones[b];
// 	for (int a = 0; a < model_scene->mNumAnimations; ++a)
// 		for (int c = 0; c < model_scene->mAnimations[a]->mNumChannels; ++c)
// 			if (model_scene->mAnimations[a]->mChannels[c]->mNodeName == name)
// 				return 
	return 0;
}

void indent(int d) {
	for (int i = 0; i < d; ++i) cout << "    ";
}

void print_matrix(const aiMatrix4x4 &m, int d) {
	indent(d);	cout << m.a1 << "\t" << m.a2 << "\t" << m.a3 << "\t" << m.a4 << endl;
	indent(d);	cout << m.b1 << "\t" << m.b2 << "\t" << m.b3 << "\t" << m.b4 << endl;
	indent(d);	cout << m.c1 << "\t" << m.c2 << "\t" << m.c3 << "\t" << m.c4 << endl;
	indent(d);	cout << m.d1 << "\t" << m.d2 << "\t" << m.d3 << "\t" << m.d4 << endl;
}

extern "C" {
	matrix4x4f tmp_root_trafo;
}
bone_list* traverse_nodes(aiScene const *model_scene, aiNode *node, aiMatrix4x4 curr_trafo, int d) {
	static bool first_bone = true;
	indent(d);
	aiBone *b = find_bone(model_scene, node->mName);
	cout << node->mName.data;
	if (b) cout << "(BONE)";
	else cout << "(no bone)";
	cout << endl;
// 	print_matrix(curr_trafo, d);

	curr_trafo = curr_trafo * node->mTransformation;
	if (string(node->mName.data) == "root") {
// 		node->mTransformation = curr_trafo;
		first_bone = false;
		ass_imp_mat4_to_matrix4x4f(&tmp_root_trafo, curr_trafo);
		indent(d);
		cout << "SETTING ROOT" << endl;
	}
// 	if (!b) node->mTransformation = aiMatrix4x4();


	bone_list *ret = 0;
	for (int c = 0; c < node->mNumChildren; ++c) {
		bone_list *childs_bones = traverse_nodes(model_scene, node->mChildren[c], curr_trafo, d+1);
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
		ass_imp_mat4_to_matrix4x4f(&bone->rest_trafo_relative, node->mTransformation);
		ass_imp_mat4_to_matrix4x4f(&bone->rest_trafo, curr_trafo);
		ass_imp_mat4_to_matrix4x4f(&bone->offset_trafo, found_bone->mOffsetMatrix);
		copy_matrix4x4f(&bone->current_trafo, &bone->rest_trafo);
		bone->children = ret;
		bone->name = strdup((char*)found_bone->mName.data);
		bone->local_id = -1;
		ret = (bone_list*)malloc(sizeof(bone_list));
		ret->bone = bone;
		ret->next = 0;
	}
	return ret;
}

skeletal_animation_ref generate_skeletal_anim(aiScene const* model_scene, const char *name) {
	aiNode *root = model_scene->mRootNode;
	aiMatrix4x4 curr_trafo;
	bone_list *bones = traverse_nodes(model_scene, root, curr_trafo, 0);
	skeletal_animation_ref ref = make_skeletal_animation(name, bones);
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

	if (!model_scene) {
		cerr << endl;
		cerr << endl;
		cerr << "could not load model " << filename << endl;
		cerr << importer.GetErrorString() << endl;
		cerr << endl;
		cerr << endl;
		exit(0);
	}
	
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
			
		{
			material_ref mat = make_material((string(modelname) + "/" + n + "+B-hack").c_str(), &col_amb, &col_diff, &col_spec);

			if (mtl->GetTexture(aiTextureType_AMBIENT, 0, &doh) == AI_SUCCESS)  add_texture_if_found(mat, (const char*)doh.data, &p, "ambient_tex");
			if (mtl->GetTexture(aiTextureType_DIFFUSE, 0, &doh) == AI_SUCCESS)  add_texture_if_found(mat, (const char*)doh.data, &p, "diffuse_tex");
			if (mtl->GetTexture(aiTextureType_SPECULAR, 0, &doh) == AI_SUCCESS) add_texture_if_found(mat, (const char*)doh.data, &p, "specular_tex");
			if (mtl->GetTexture(aiTextureType_OPACITY, 0, &doh) == AI_SUCCESS)  add_texture_if_found(mat, (const char*)doh.data, &p, "mask_tex");
			float s;
			mtl->Get(AI_MATKEY_SHININESS, s);
			material_set_specular_exponent(mat, s);
		}
	}

	vec3f model_bbmi, model_bbma;

	skeletal_animation_ref anim = generate_skeletal_anim(model_scene, object_name);

	for (int mid = 0; mid < model_scene->mNumMeshes; ++mid) {
		aiMesh *group = model_scene->mMeshes[mid];
		int pos = 1;
		int norm = group->HasNormals() ? 1 : 0;
		int tc = group->HasTextureCoords(0) ? 1 : 0;
		int bones = group->HasBones() ? group->mNumBones : 0;
		bone **found_bones = new bone*[bones];
		for (int i = 0; i < bones; ++i)
			found_bones[i] = 0;

		cout << "mesh " << mid << ": " << group->mName.data << endl;
		mesh_ref m = make_mesh((string(modelname) + "/" + (const char*)group->mName.data).c_str(), pos+norm+tc+(bones/4 + ((bones%4)?1:0)));
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
			int components_in_last_buffer = bones % 4;
			int vertex_buffers = bones / 4 + (components_in_last_buffer!=0 ? 1 : 0);

			cout << " V V V V " << (bones/4 + ((bones%4)?1:0)) << " V V V " << vertex_buffers << endl;
			vector<float*> bufs;
			for (int buf = 0; buf < vertex_buffers; ++buf) {
				float *weights = new float[4*verts];
				for (int i = 0; i < 4*verts; ++i)
					weights[i] = 0;
				for (int b = 0; b < 4; ++b) {
					int bone_id = 4*buf + b;
					if (bone_id >= bones)
						continue;
					aiBone *bone_data = group->mBones[bone_id];
					cout << "BONE " << bone_id << ": " << bone_data->mName.data << endl;
					for (int i = 0; i < bone_data->mNumWeights; ++i)
						weights[4*bone_data->mWeights[i].mVertexId+b] = bone_data->mWeights[i].mWeight;
					found_bones[bone_id] = find_bone_in_skeletal_animation(anim, (char*)bone_data->mName.data);
				}
				ostringstream oss; oss << "bone_weights_" << setw(2) << setfill('0') << buf;
				add_vertex_buffer_to_mesh(m, oss.str().c_str(), GL_FLOAT, verts, 4, weights, GL_STATIC_DRAW);
				bufs.push_back(weights);
			}

			for (int v = 0; v < verts; ++v) {
				float sum = 0;
				for (int buf = 0; buf < bufs.size(); ++buf) {
					for (int bone = 0; bone < 4; ++bone)
						sum += bufs[buf][4*v + bone];
				}
				if (sum < 0.999) {
					cout << "at " << v << ": sum = " << sum << endl;
				}
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
		if (mid == 0) {
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
		prepend_drawelement_uniform_handler(de, (uniform_setter_t)bone_matrix_uniform_handler);

		if (!valid_shader_ref(drawelement_shader(de))) {
			shader_ref shader = make_stock_shader(0, de, 0, true);
			drawelement_change_shader(de, shader);
		}
	}

	*bb_min = model_bbmi;
	*bb_max = model_bbma;

	show_anims(model_scene);
	add_animations(model_scene, anim);

	start_skeletal_animation(anim, "unnamed animation 0");
	evaluate_skeletal_animation_at(anim, 0);

	pop_image_path_front();
}

void show_anim(aiAnimation *anim) {
	cout << "\tName: " << anim->mName.data << endl;
	cout << "\tDuration: " << anim->mDuration << endl;
	cout << "\tChannels: " << anim->mChannels << endl;
	for (int i = 0; i < anim->mNumChannels; ++i) {
		aiNodeAnim *na = anim->mChannels[i];
		cout << "\tChannel " << i << endl;
		cout << "\t\tNode: " << na->mNodeName.data << endl;
		cout << "\t\t#S #R #T: " << na->mNumScalingKeys << " " << na->mNumRotationKeys << " " << na->mNumPositionKeys << endl;
	}
}

void show_anims(aiScene const *scene) {
	for (int i = 0; i < scene->mNumAnimations; ++i) {
		cout << "Animation " << i << "." << endl;
		show_anim(scene->mAnimations[i]);
		cout << endl;
	}
}

struct single_bone_animation_list*  convert_single_bone_animation(aiNodeAnim *channel, skeletal_animation_ref skel_anim) {
	struct single_bone_animation_list *bone_frames = (single_bone_animation_list*)malloc(sizeof(single_bone_animation_list));
	bone_frames->bone = find_bone_in_skeletal_animation(skel_anim, channel->mNodeName.data);
	if (!bone_frames->bone) {
		cerr << "cannot find bone " << channel->mNodeName.data << " -> invalid animation part!" << endl;
		return 0;
	}

	bone_frames->keyframes = 0;
	std::sort(channel->mPositionKeys, channel->mPositionKeys+channel->mNumPositionKeys);
	std::sort(channel->mRotationKeys, channel->mRotationKeys+channel->mNumRotationKeys);
	std::sort(channel->mScalingKeys,  channel->mScalingKeys +channel->mNumScalingKeys);

	bool first = true;
	aiQuatKey *first_quat = 0;
	aiVectorKey *first_scale = 0;
	aiVectorKey *first_trans = 0;

	// collect animation keys of same time step into single entries
	int pi = 0, ri = 0, si = 0;
	int pn = channel->mNumPositionKeys, rn = channel->mNumRotationKeys, sn = channel->mNumScalingKeys;
	while (pi < pn || ri < rn || si < sn) {
// 		cout << "pi = " << pi << endl;
// 		cout << "ri = " << ri << endl;
// 		cout << "si = " << si << endl;
		aiVectorKey *found_pos = 0;
		aiQuatKey *found_rot = 0;
		aiVectorKey *found_scale = 0;
		float tmin = FLT_MAX;
		// find next time position
		if (pi < pn)	tmin = min(tmin, (float)channel->mPositionKeys[pi].mTime);
		if (ri < rn)	tmin = min(tmin, (float)channel->mRotationKeys[ri].mTime);
		if (si < sn)	tmin = min(tmin, (float)channel->mScalingKeys[si].mTime);
// 		cout << "tmin = " << tmin << endl;
		// select components for that time position
		if (pi < pn)	if (channel->mPositionKeys[pi].mTime == tmin)	found_pos = channel->mPositionKeys+pi;
		if (ri < rn)	if (channel->mRotationKeys[ri].mTime == tmin)	found_rot = channel->mRotationKeys+ri;
		if (si < sn)	if (channel->mScalingKeys[si].mTime == tmin)	found_scale = channel->mScalingKeys+si;
		// make entry
		bone_frame_list *curr = (bone_frame_list*)malloc(sizeof(bone_frame_list));
		make_vec3f(&curr->keyframe.translation, 0, 0, 0);
		make_quaternion4f(&curr->keyframe.rotation, 0, 0, 0, 1);
		make_vec3f(&curr->keyframe.scale, 1, 1, 1);
// 		if (first) {
// 			if (found_pos)   first_trans = found_pos;//new aiVector3D(found_pos->mValue);
// 			else             first_trans = 0;
// 			if (found_rot)   first_quat = found_rot;
// 			else             first_quat = 0;
// 			if (found_scale) first_scale = found_scale;
// 			else             first_scale = 0;
// 			first = false;
// 		}
// 		found_pos = first_trans;
// 		found_rot = first_quat;
// 		found_scale = first_scale;

// 		cout << "bone " << bone_frames->bone->name << " t=" << tmin << "\t";
		bone_frame_list *old_head = bone_frames->keyframes;
		if (found_pos)   make_vec3f(&curr->keyframe.translation, found_pos->mValue.x, found_pos->mValue.y, found_pos->mValue.z);
		else             copy_vec3f(&curr->keyframe.translation, &old_head->keyframe.translation);
		if (found_rot)   make_quaternion4f(&curr->keyframe.rotation, found_rot->mValue.x, found_rot->mValue.y, found_rot->mValue.z, found_rot->mValue.w);
		else             copy_quaternion4f(&curr->keyframe.rotation, &old_head->keyframe.rotation);
		if (found_scale) make_vec3f(&curr->keyframe.scale, found_scale->mValue.x, found_scale->mValue.y, found_scale->mValue.z);
		else             copy_vec3f(&curr->keyframe.scale, &old_head->keyframe.scale);
		cout << endl;
		curr->keyframe.time = tmin;
		// go forward
		if (pi < pn && found_pos) ++pi;
		if (ri < rn && found_rot) ++ri;
		if (si < sn && found_scale) ++si;
		bone_frames->keyframes = curr;
		curr->next = old_head;
	}

	cout << "frames for bone " << bone_frames->bone->name << ":\t ";
	for (bone_frame_list *run = bone_frames->keyframes; run; run = run->next)
		cout << run->keyframe.time << "\t";
	cout << endl;

	// reverse list
	struct bone_frame_list *rev_list = 0;
	bone_frame_list *run = bone_frames->keyframes;
	while (run) {
		bone_frame_list *prev_head = rev_list;
		bone_frame_list *copy = run;
		run = run->next;
		rev_list = copy;
		copy->next = prev_head;
	}
	bone_frames->keyframes = rev_list;

	return bone_frames;
}

void add_animation(aiAnimation *anim, skeletal_animation_ref skel_anim, int nr) {
	animation_sequence *seq = (animation_sequence*)malloc(sizeof(animation_sequence));
	if (anim->mName.length > 0)
		seq->name = strdup(anim->mName.data);
	else {
		ostringstream oss; oss << "unnamed animation " << nr;
		seq->name = strdup(oss.str().c_str());
	}
	seq->bone_animations = 0;
	for (int i = 0; i < anim->mNumChannels; ++i) {
		single_bone_animation_list *new_entry = convert_single_bone_animation(anim->mChannels[i], skel_anim);
		if (!new_entry)
			continue;
		single_bone_animation_list *old = seq->bone_animations;	
		seq->bone_animations = new_entry;
		seq->bone_animations->next = old;
	}
	add_animation_to_skeleton(skel_anim, seq);
}

void add_animations(aiScene const *scene, skeletal_animation_ref skel_anim) {
	for (int i = 0; i < scene->mNumAnimations; ++i) {
		add_animation(scene->mAnimations[i], skel_anim, i);
	}
}

/* vim: set foldmethod=marker: */

