#ifndef __CGLS_CAMERA_ANIMATION_H__ 
#define __CGLS_CAMERA_ANIMATION_H__ 

#include "refs.h"

#include "animation.h"

#include <libcgl/cgl.h>
#include <libcgl/camera.h>

define_array(camera_animation);

camera_animation_ref make_camera_animation(const char *name, camera_ref cam);
void add_current_view_to_camera_animation(camera_animation_ref ref, float t);

camera_animation_ref find_camera_animation(const char *name);
struct camera_animation_list* list_camera_animations();

void change_camera_animation_speed(camera_animation_ref ref, float factor);

float camera_animation_speed(camera_animation_ref ref, float factor);
void start_camera_animation(camera_animation_ref ref);
void stop_camera_animation(camera_animation_ref ref);
void evaluate_camera_animation_at(camera_animation_ref ref, animation_time_t time);

// console

void add_cam_anim_commands_to_viconsole(console_ref console);

#endif

