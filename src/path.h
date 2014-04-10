#ifndef __CGLS_PATH_H__ 
#define __CGLS_PATH_H__ 

#include "refs.h"

#include <libmcm/vectors.h>
#include <libmcm/matrix.h>

#include <libcgl/cgl.h>

#include "c-utils.h"
#include "animation.h"

#ifdef __cplusplus
extern "C" {
#endif

path_animation_ref make_path_animation(const char *name);
void add_node_to_path_animation(path_animation_ref ref, vec3f *p, vec3f *up, float t);
matrix4x4f* path_matrix_of_animation(path_animation_ref ref);

void start_path_animation(path_animation_ref ref);
void stop_path_animation(path_animation_ref ref);
void change_path_animation_speed(path_animation_ref ref, float factor);
float path_animation_speed(path_animation_ref ref, float factor);
void evaluate_path_animation_at(path_animation_ref ref, animation_time_t time);

void change_path_node_tension(path_animation_ref ref, int node, float tension);
void normalize_speed_along_path(path_animation_ref ref);

define_array(path_animation);

path_animation_ref find_path_animation(const char *name);
struct path_animation_list* list_path_animations();
bool valid_path_animation_ref(path_animation_ref ref);

// internal info
int path_animation_nodes(path_animation_ref);
float path_animation_node_time(path_animation_ref, int n);
vec3f path_animation_node_position(path_animation_ref, int n);

// console.
void add_path_commands_to_viconsole(console_ref console);

#ifdef __cplusplus
}
#endif

#endif

