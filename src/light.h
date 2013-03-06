#ifndef __LIGHT_H__ 
#define __LIGHT_H__ 

#include "c-utils.h"
#include "uniforms.h"
#include <libcgl/framebuffer.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	int id;
} light_ref;

enum built_in_light_types { wrong_light_t, spot_light_t };

define_slist(light_uniform_handler_node, uniform_setter_t handler);
define_slist(light_list, light_ref ref);

void apply_deferred_lights(struct light_list *lights);

light_ref make_headmounted_spotlight(const char *name, framebuffer_ref gbuffer, float cutoff);


#ifdef __cplusplus
}
#endif

#endif

