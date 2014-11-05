#ifndef __LIGHT_H__ 
#define __LIGHT_H__ 

#include "refs.h"

#include "c-utils.h"
#include "uniforms.h"
#include "drawelement.h"
#include <libcgl/framebuffer.h>

#ifdef __cplusplus
extern "C" {
#endif

enum built_in_light_types { wrong_light_t, spot_light_t, hemi_light_t, rect_light_t };

define_slist(light_uniform_handler_node, uniform_setter_t handler);
define_array(light);


// actual light functions
bool valid_light_ref(light_ref ref);
light_ref make_light(const char *name);
const char* light_name(light_ref ref);
void light_use_deferred_drawelement(light_ref ref, drawelement_ref de);
drawelement_ref light_deferred_drawelement(light_ref ref);
void light_use_as_representation(light_ref ref, drawelement_ref de);
drawelement_ref light_representation(light_ref ref);
matrix4x4f* light_trafo(light_ref ref);
void change_light_color3f(light_ref ref, float r, float g, float b);
void change_light_color(light_ref ref, vec3f *c);
vec3f* light_color(light_ref ref);
void* light_aux(light_ref ref);
unsigned int light_type(light_ref ref);
void set_light_aux(light_ref ref, unsigned int new_type, void *p);
void replace_light_trafo(light_ref ref, matrix4x4f *mat);
struct light_uniform_handler_node* light_uniform_handlers(light_ref ref);
void add_light_uniform_handler(light_ref ref, bool (*handler)(light_ref *, const char *, int));
light_ref find_light_by_representation(drawelement_ref rep);

bool light_is_on(light_ref ref);
void light_on(light_ref ref);
void light_off(light_ref ref);
bool show_light_representation_if_off(light_ref ref);
void light_representation_mode_if_off(light_ref ref, bool render);
bool dim_light_representation_if_off(light_ref ref);
void light_representation_dim_mode_if_off(light_ref ref, bool render);

light_ref find_light(const char *name);


// deferred
void apply_deferred_lights(framebuffer_ref gbuffer, struct light_list *lights);
void apply_single_deferred_light(light_ref ref);
void render_light_representation_with_shader(light_ref ref, shader_ref shader, uniform_setter_t handler);
void render_light_representation(light_ref ref);

// forward
void stock_forward_shading_light_setup(struct light_list *lights);
void stock_forward_shading_light_cleanup(struct light_list *lights);

// derived (usable) light types
light_ref make_headmounted_spotlight(const char *name, framebuffer_ref gbuffer, float cutoff);
light_ref make_hemispherical_light(const char *name, framebuffer_ref gbuffer, vec3f *up);
light_ref make_ambient_light(const char *name, framebuffer_ref gbuffer, vec3f *color);
light_ref make_spotlight(const char *name, framebuffer_ref gbuffer, vec3f *pos, vec3f *dir, vec3f *up, float cutoff);
light_ref make_spotlight_from_camera(const char *name, framebuffer_ref gbuffer, camera_ref cam);
drawelement_ref build_spot_light_representation_drawelement(const char *lightname, light_ref ref, float size_scale, float cutoff);
light_ref make_rectangular_light(const char *name, framebuffer_ref gbuffer, vec3f *pos, vec3f *dir, vec3f *up, float width, float height);

// uniform handlers for lights
bool basic_light_uniform_handler(light_ref *ref, const char *uniform, int location);
bool stock_spotlight_uniform_handler(light_ref *ref, const char *uniform, int location);
bool stock_hemilight_uniform_handler(light_ref *ref, const char *uniform, int location);

#ifdef __cplusplus
}
#endif

#endif

