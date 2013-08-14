#include "skeletal.h"

#include <libmcm/matrix.h>
void make_translation_matrix4x4f(matrix4x4f *mat, vec3f *transl); // defined in interaction.c...

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "drawelement.h"

struct skeletal_animation {
	const char *name;
	struct bone_list *bones;
	struct bone **all_bones;
	struct animation_list *animations;
	struct animation_sequence *current_animation;
	animation_time_t current_animation_start_time;
	float animation_speed;
};

#include <libcgl/mm.h>
define_mm(skeletal_animation, skeletal_animations, skeletal_animation_ref);
#include "skeletal.xx"

typedef bool (*bone_trav_handler_t)(struct bone *bone, int depth, void *data);
bool traverse_bone_hierarchy(struct bone_list *list, bone_trav_handler_t handler, int depth, void *data);
bool count_bones(struct bone *bone, int depth, int *counter);
struct bone_id_state {
	struct bone **array;
	int id;
};
bool assign_ids(struct bone *bone, int depth, struct bone_id_state *state);

skeletal_animation_ref make_skeletal_animation(const char *name, struct bone_list *bones) {
	skeletal_animation_ref ref = allocate_skeletal_animation_ref();
	struct skeletal_animation *sa = skeletal_animations+ref.id;
	sa->name = strdup(name);
	sa->bones = bones;

	int counter = 0;
	traverse_bone_hierarchy(sa->bones, (bone_trav_handler_t)count_bones, 0, &counter);
	sa->all_bones = malloc(sizeof(struct bone*)*counter);

	struct bone_id_state bis;
	bis.array = sa->all_bones;
	bis.id = 0;
	traverse_bone_hierarchy(sa->bones, (bone_trav_handler_t)assign_ids, 0, &bis);

	sa->animations = 0;
	sa->current_animation = 0;
	sa->current_animation_start_time = 0;
	sa->animation_speed = 1;
	
	return ref;
}

struct bone* find_bone(struct bone *tree, const char *name) {
	if (strcmp(tree->name, name) == 0)
		return tree;
	for (struct bone_list *run = tree->children; run; run = run->next) {
		struct bone *found = find_bone(run->bone, name);
		if (found)
			return found;
	}
}

bool traverse_bone_hierarchy(struct bone_list *list, bool (*handler)(struct bone *bone, int depth, void *data), int depth, void *data) {
	while (list) {
// 		for (int i = 0; i < depth; ++i) printf("  ");
// 		printf("at bone %s\n", list->bone->name);
		if (!handler(list->bone, depth, data))
			list = list->next;
		else
			return true;
	}
	return false;
}

bool count_bones(struct bone *bone, int depth, int *counter) {
	(*counter)++;
	return traverse_bone_hierarchy(bone->children, (bone_trav_handler_t)count_bones, depth+1, counter);
}

bool assign_ids(struct bone *bone, int depth, struct bone_id_state *state) {
	state->array[state->id] = bone;
	state->id++;
	return traverse_bone_hierarchy(bone->children, (bone_trav_handler_t)assign_ids, depth+1, state);
}

struct bone* find_bone_in_skeletal_animation(skeletal_animation_ref ref, const char *bone_name) {
	struct skeletal_animation *sa = skeletal_animations+ref.id;
	for (struct bone_list *run = sa->bones; run; run = run->next) {
		struct bone *found = find_bone(run->bone, bone_name);
		if (found)
			return found;
	}
}

void add_animation_to_skeleton(skeletal_animation_ref ref, struct animation_sequence *seq) {
	struct skeletal_animation *sa = skeletal_animations+ref.id;
	struct animation_list *old = sa->animations;
	sa->animations = malloc(sizeof(struct animation_list));
	sa->animations->animation = seq;
	sa->animations->next = old;
	// normalize animation time to start at 0.
	float start = FLT_MAX, end = -FLT_MAX;
	for (struct single_bone_animation_list *bone = seq->bone_animations; bone; bone = bone->next)
		for (struct bone_frame_list *frame = bone->keyframes; frame; frame = frame->next) {
			if (frame->keyframe.time < start) start = frame->keyframe.time;
			if (frame->keyframe.time > end) end = frame->keyframe.time;
		}
	for (struct single_bone_animation_list *bone = seq->bone_animations; bone; bone = bone->next)
		for (struct bone_frame_list *frame = bone->keyframes; frame; frame = frame->next)
			frame->keyframe.time -= start;
	// store last time step to wrap animations.
	seq->last_time_step = end-start;
}

struct animation_sequence* find_animation_in_skeletal_animation(skeletal_animation_ref ref, const char *name) {
	struct skeletal_animation *sa = skeletal_animations+ref.id;
	for (struct animation_list *run = sa->animations; run; run = run->next)
		if (strcmp(run->animation->name, name) == 0)
			return run->animation;
	return 0;
}

//! pass name == 0 to deactivate.
void start_skeletal_animation(skeletal_animation_ref ref, const char *name) {
	struct skeletal_animation *sa = skeletal_animations+ref.id;
	sa->current_animation = find_animation_in_skeletal_animation(ref, name);
	sa->current_animation_start_time = animation_time_stamp();
	if (!sa->current_animation && name)
		fprintf(stderr, "cannot find animation %s.\n", name);
}

skeletal_animation_ref find_skeletal_animation(const char *name) {
	skeletal_animation_ref ref = { -1 };
	if (strlen(name) == 0) return ref;
    for (int i = 0; i < next_skeletal_animation_index; ++i) {
        if (strcmp(skeletal_animations[i].name, name) == 0) {
			ref.id = i;
			return ref;
		}
	}
	return ref;
}

struct skeletal_animation_list* list_skeletal_animations() {
	if (next_skeletal_animation_index == 0)
		return 0;
	struct skeletal_animation_list *head = malloc(sizeof(struct skeletal_animation_list));
	struct skeletal_animation_list *run = head;
	run->ref.id = 0;
	for (int i = 1; i < next_skeletal_animation_index; ++i) {
		run = run->next = malloc(sizeof(struct skeletal_animation_list));
		run->ref.id = i;
	}
	run->next = 0;
	return head;
}

void change_skeletal_animation_speed(skeletal_animation_ref ref, float factor) {
	struct skeletal_animation *sa = skeletal_animations+ref.id;
	sa->animation_speed = factor;
}

float animation_skeletal_speed(skeletal_animation_ref ref, float factor) {
	struct skeletal_animation *sa = skeletal_animations+ref.id;
	return sa->animation_speed;
}

extern matrix4x4f tmp_root_trafo;

void evaluate_bone_animation_at(struct bone *bone, struct bone_key_frame *this, struct bone_key_frame *next, float time) {

	vec3f translation;
	quaternionf rotation;
	vec3f scale;
	if (!next) {
		copy_vec3f(&translation, &this->translation);
		copy_quaternion4f(&rotation, &this->rotation);
		copy_vec3f(&scale, &this->scale);
	}
	else {
		// interpolate
		float t = (time - this->time) / (next->time - this->time);
		if (t < 0) t = 0;
		if (t > 1) t = 1;
		// printf("interpolation t:   prev-time=%6.6f   next-time=%6.6f   time=%6.6f   --> t = %6.6f.\n", this->time, next->time, time, t);
		lerp_vec3f(&translation, &this->translation, &next->translation, t);
		copy_quaternion4f(&rotation, &this->rotation);
		slerp_quaterionf(&rotation, &this->rotation, &/*!!*/next->rotation, t);
		lerp_vec3f(&scale, &this->scale, &next->scale, t);
	}

	matrix4x4f T, R, S, tmp;
	make_translation_matrix4x4f(&T, &translation);
	quaternionf_to_matrix4x4f(&R, &rotation);
	make_scale_matrix4x4f(&S, &scale);

// 	printf("bone %s:\n", bone->name);
// 	printf("\tT = %6.6f %6.6f %6.6f\n", this->translation.x, this->translation.y, this->translation.z);
// 	printf("\tR = %6.6f %6.6f %6.6f %6.6f\n", this->rotation.v.x, this->rotation.v.y, this->rotation.v.z, this->rotation.w);
// 	printf("\tS = %6.6f %6.6f %6.6f\n", this->scale.x, this->scale.y, this->scale.z);

	// T*R*S
	multiply_matrices4x4f(&tmp, &R, &S);
	multiply_matrices4x4f(&bone->current_trafo, &T, &tmp);
		
// 	copy_matrix4x4f(&bone->current_trafo, &bone->rest_trafo_relative);
}

void update_bones_using_matrix_stack(struct bone *bone, int depth, matrix4x4f *stack) {
	matrix4x4f tmp;
	multiply_matrices4x4f(stack+depth+1, stack+depth, &bone->current_trafo);
	copy_matrix4x4f(&bone->current_trafo, stack+depth+1);
	traverse_bone_hierarchy(bone->children, (bone_trav_handler_t)update_bones_using_matrix_stack, depth+1, stack);
}

/*! does nothing if no animation sequence is active (not even complain).
 *  \c time is supposed to be obtained via \ref animation_time_stamp().
 */
void evaluate_skeletal_animation_at(skeletal_animation_ref ref, animation_time_t t) {
	struct skeletal_animation *sa = skeletal_animations+ref.id;
	if (!sa->current_animation)
		return;
	t -= sa->current_animation_start_time;
	t /= 1000;
	t *= sa->animation_speed;
	while (t > sa->current_animation->last_time_step) {
		t -= sa->current_animation->last_time_step;
		sa->current_animation_start_time += sa->current_animation->last_time_step * 1000 / sa->animation_speed;
	}
	// printf("eval %f\n", t);
	int updated = 0;
	// for each bone in this animation
	for (struct single_bone_animation_list *bone_anim = sa->current_animation->bone_animations; bone_anim; bone_anim = bone_anim->next) {
		// find the keyframes to interpolate between (or a single border one).
		struct bone_frame_list *prev =  bone_anim->keyframes;
		for (struct bone_frame_list *frame = bone_anim->keyframes; frame; frame = frame->next) {
			if (frame->keyframe.time >= t || !frame->next) {
				evaluate_bone_animation_at(bone_anim->bone, &prev->keyframe, &frame->keyframe, t);
				++updated;
				break;
			}
			prev = frame;
		}
	}
	// printf("udpated %d bones.\n", updated);
	// update the bone matrices recursively, makeing use of the interpolated key frame matrices computed above.
	matrix4x4f *stack = malloc(sizeof(matrix4x4f)*32);
	make_unit_matrix4x4f(stack);
	copy_matrix4x4f(stack, &tmp_root_trafo);
	traverse_bone_hierarchy(sa->bones, (bone_trav_handler_t)update_bones_using_matrix_stack, 0, stack);
	free(stack);
}

bool bone_matrix_uniform_handler(drawelement_ref *ref, const char *uniform, int location) {
	if (strcmp(uniform, "bone_matrices") == 0) {
		int bones = drawelement_number_of_bones(*ref);
		struct bone **de_bones = drawelement_bones(*ref);
		matrix4x4f *matrices = drawelement_bone_matrix_area(*ref);
		matrix4x4f tmp, tmp2;
		static bool first = true;
		for (int i = 0; i < bones; ++i) {
			if (first)
				printf("bone %d: %s.\n", i, de_bones[i]->name);
			multiply_matrices4x4f(matrices+i, &de_bones[i]->current_trafo, &de_bones[i]->offset_trafo);
// 			multiply_matrices4x4f(matrices+i, &de_bones[i]->rest_trafo, &de_bones[i]->offset_trafo);
		}
		first = false;
		glUniformMatrix4fv(location, bones, GL_FALSE, (float*)matrices);
	}
	else if (strcmp(uniform, "bones") == 0) {
		int bones = drawelement_number_of_bones(*ref);
		glUniform1i(location, bones);
	}
	else
		return false;
	return true;
}

void quaternionf_to_matrix4x4f(matrix4x4f *mat, quaternionf *q) {
	#define sq(X) ((X)*(X))
	#define qx (q->v.x)
	#define qy (q->v.y)
	#define qz (q->v.z)
	#define qw (q->w)

	// 	1 - 2*qy2 - 2*qz2 	2*qx*qy - 2*qz*qw 	2*qx*qz + 2*qy*qw
	// 	2*qx*qy + 2*qz*qw 	1 - 2*qx2 - 2*qz2 	2*qy*qz - 2*qx*qw
	// 	2*qx*qz - 2*qy*qw 	2*qy*qz + 2*qx*qw 	1 - 2*qx2 - 2*qy2

	// col 1
	mat->col_major[0] = 1 - 2*sq(qy) - 2*sq(qz);
	mat->col_major[1] = 2*qx*qy + 2*qz*qw;
	mat->col_major[2] = 2*qx*qz - 2*qy*qw;
	mat->col_major[3] = 0;

	// col 2
	mat->col_major[4] = 2*qx*qy - 2*qz*qw;
	mat->col_major[5] = 1 - 2*sq(qx) - 2*sq(qz);
	mat->col_major[6] = 2*qy*qz + 2*qx*qw;
	mat->col_major[7] = 0;

	// col 3
	mat->col_major[8]  = 2*qx*qz + 2*qy*qw;
	mat->col_major[9]  = 2*qy*qz - 2*qx*qw;
	mat->col_major[10] = 1 - 2*sq(qx) - 2*sq(qy);
	mat->col_major[11] = 0;

	// col 4
	mat->col_major[12] = mat->col_major[13] = mat->col_major[14] = 0;
	mat->col_major[15] = 1;
}

// SCM

#ifdef WITH_GUILE


SCM_DEFINE(s_list_anims, "list-skeletal-animations", 0, 0, 0, (), "") {
    SCM list = scm_list_1(scm_from_locale_string(skeletal_animations[0].name));
    for (int i = 1; i < next_skeletal_animation_index; ++i)
        list = scm_cons(scm_from_locale_string(skeletal_animations[i].name), list);
    return list;
}

SCM_DEFINE(s_change_anim_speed, "change-speed-of-skeletal-animation", 2, 0, 0, (SCM anim, SCM factor), "") {
    skeletal_animation_ref ref = { scm_to_int(anim) };
	change_skeletal_animation_speed(ref, scm_to_double(factor));
	return SCM_BOOL_T;
}

void register_scheme_functions_for_skeletal_animation() {
#include "skeletal.x"
}


#endif

