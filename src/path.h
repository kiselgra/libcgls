#ifndef __CGLS_PATH_H__ 
#define __CGLS_PATH_H__ 

#include <libmcm/vectors.h>
#include <libmcm/matrix.h>

#include "c-utils.h"
#include "animation.h"

typedef struct {
	int id;
} path_animation_ref;

path_animation_ref make_path_animation(const char *name, int nodes);
void add_node_to_path_animation(path_animation_ref ref, vec3f *p, float t);
matrix4x4f* path_matrix_of_animation(path_animation_ref ref);
void evaluate_path_animation_at(path_animation_ref ref, animation_time_t time);

define_slist(path_animation_list, path_animation_ref ref);

path_animation_ref find_path_animation(const char *name);
struct path_animation_list* list_path_animations();
bool valid_path_animation_ref(path_animation_ref ref);

#endif

