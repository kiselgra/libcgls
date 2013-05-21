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
	matrix4x4f rest_trafo;
	matrix4x4f offset_trafo;
	struct bone_list *children;
	int local_id;
};

typedef struct {
	int id;
} skeletal_animation_ref;

skeletal_animation_ref make_skeletal_animation(const char *name, struct bone_list *bones);
struct bone* find_bone_in_skeletal_animation(skeletal_animation_ref ref, const char *bone_name);

#ifdef __cplusplus
}
#endif

#endif

