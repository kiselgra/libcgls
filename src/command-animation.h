#ifndef __COMMAND_ANIMATION_H__ 
#define __COMMAND_ANIMATION_H__ 

#include "refs.h"

#include "animation.h"

#include <libcgl/cgl.h>

#ifdef __cplusplus
extern "C" {
#endif

define_array(command_animation)

command_animation_ref make_command_animation(const char *name);
void add_node_to_command_animation(command_animation_ref ref, const char *command, float t);
command_animation_ref find_command_animation(const char *name);
struct command_animation_list* list_command_animations();
void change_command_animation_speed(command_animation_ref ref, float factor);
float command_animation_speed(command_animation_ref ref, float factor);
void start_command_animation(command_animation_ref ref);
void stop_command_animation(command_animation_ref ref);
void evaluate_command_animation_at(command_animation_ref ref, animation_time_t time);

#ifdef __cplusplus
}
#endif

#endif

