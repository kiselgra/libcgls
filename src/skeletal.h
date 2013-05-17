#ifndef __CGLS_SKELETAL_H__ 
#define __CGLS_SKELETAL_H__ 

#include <libmcm/matrix.h>

#include "c-utils.h"

struct bone;

define_slist(bone_list, struct bone *bone);

struct bone {
	matrix4x4f rest_trafo;
	bone_list *children;
};

struct skeletal {
	struct bone_list *bones;
};

#endif

