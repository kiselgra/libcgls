#ifndef __CGLS_SKELETAL_H__ 
#define __CGLS_SKELETAL_H__ 

#include <libmcm/matrix.h>

#include "c-utils.h"

#ifdef __cplusplus
extern "C" {
#endif

struct bone;

define_slist(bone_list, struct bone *bone);

struct bone {
	char *name;
	matrix4x4f rest_trafo_relative;
	matrix4x4f rest_trafo;
	matrix4x4f offset_trafo;
	matrix4x4f current_trafo;
	struct bone_list *children;
	int local_id;
};

typedef struct {
	vec3f v;
	float w;
} quaternionf;

void make_quaternionf(quaternionf *q, const vec3f *v, float w);
void make_quaternion4f(quaternionf *q, float x, float y, float z, float w);
void copy_quaternion4f(quaternionf *to, const quaternionf *from);
void quaternionf_to_matrix4x4f(matrix4x4f *mat, quaternionf *q);

struct bone_key_frame {
	float time;
	vec3f translation;
	quaternionf rotation;
	vec3f scale;
};

define_slist(bone_frame_list, struct bone_key_frame keyframe);
define_slist(single_bone_animation_list, struct bone *bone; struct bone_frame_list *keyframes);

struct animation_sequence {
	char *name;
	struct single_bone_animation_list *bone_animations;
};

define_slist(animation_list, struct animation_sequence *animation);

typedef struct {
	int id;
} skeletal_animation_ref;

skeletal_animation_ref make_skeletal_animation(const char *name, struct bone_list *bones);
struct bone* find_bone_in_skeletal_animation(skeletal_animation_ref ref, const char *bone_name);
void add_animation_to_skeleton(skeletal_animation_ref ref, struct animation_sequence *seq);

void start_skeletal_animation(skeletal_animation_ref ref, const char *name);
void evaluate_skeletal_animation_at(skeletal_animation_ref ref, float t);

#ifdef __cplusplus
}
#endif

#endif

