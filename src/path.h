#ifndef __CGLS_PATH_H__ 
#define __CGLS_PATH_H__ 

#include "drawelement.h"

#include <libmcm/vectors.h>

typedef struct {
	int id;
} path_animation_ref;

path_animation_ref make_path_animation(const char *name, int nodes, drawelement_ref de);
void add_node_to_path_animation(path_animation_ref ref, vec3f *p, float t);

define_slist(path_animation_list, path_animation_ref ref);

path_animation_ref find_path_animation(const char *name);
struct path_animation_list* list_path_animations();

#endif

