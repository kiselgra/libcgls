#ifndef __LIGHT_H__ 
#define __LIGHT_H__ 

#include "c-utils.h"
#include "uniforms.h"
#include "drawelement.h"
#include <libcgl/framebuffer.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	int id;
} light_ref;

enum built_in_light_types { wrong_light_t, spot_light_t, hemi_light_t };

define_slist(light_uniform_handler_node, uniform_setter_t handler);
define_slist(light_list, light_ref ref);



bool valid_light_ref(light_ref ref);
vec3f* light_color(light_ref ref);
void change_light_color3f(light_ref ref, float r, float g, float b);
void change_light_color(light_ref ref, vec3f *c);
void add_light_uniform_handler(light_ref ref, bool (*handler)(light_ref *, const char *, int));
drawelement_ref light_representation(light_ref ref);



void apply_deferred_lights(struct light_list *lights);

light_ref make_headmounted_spotlight(const char *name, framebuffer_ref gbuffer, float cutoff);
light_ref make_hemispherical_light(const char *name, framebuffer_ref gbuffer, vec3f *up);
light_ref make_spotlight(const char *name, framebuffer_ref gbuffer, vec3f *pos, vec3f *dir, vec3f *up, float cutoff);


#ifdef __cplusplus
}
#endif

#endif

