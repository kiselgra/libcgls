#ifndef __STOCK_SHADER_H__ 
#define __STOCK_SHADER_H__ 

#ifdef __cplusplus
extern "C" {
#endif

#include "drawelement.h"

#include <libcgl/shader.h>
#include <libcgl/framebuffer.h>

#include "c-utils.h"

define_slist(stockshader_stringlist, char *data; bool malloced);

struct stockshader_fragments {
	struct stockshader_stringlist *inputs,
	                              *vertex_sources,
	                              *fragment_sources,
	                              *uniforms;
};

void init_stockshader_fragments(struct stockshader_fragments *ssf);
void free_stockshader_fragments(struct stockshader_fragments *ssf);
void stockshader_add_input(struct stockshader_fragments *ssf, const char *name);
void stockshader_add_vsource(struct stockshader_fragments *ssf, const char *code);
void stockshader_add_fsource(struct stockshader_fragments *ssf, const char *code);
void stockshader_add_uniform(struct stockshader_fragments *ssf, const char *name);
int stockshader_inputs(struct stockshader_fragments *ssf);
int stockshader_vsources(struct stockshader_fragments *ssf);
int stockshader_fsources(struct stockshader_fragments *ssf);
int stockshader_uniforms(struct stockshader_fragments *ssf);
void populate_shader_with_fragments(shader_ref shader, struct stockshader_fragments *ssf);
void stockshader_clear_fsource(struct stockshader_fragments *ssf);

void stock_shader(struct stockshader_fragments *ssf, bool ambient_tex, bool diffuse_tex, bool specular_tex, bool mask_tex);

bool compile_and_link_shader_showing_log_on_error(shader_ref shader);

// deferred part

enum {
    stock_ds_flag_position_in_world = 01,
};

framebuffer_ref make_stock_deferred_buffer(const char *name, unsigned int width, unsigned int height, 
                                           GLenum diffuse_format, GLenum specular_format, GLenum normal_format, 
                                           GLenum position_format, GLenum depth_format);
drawelement_ref make_stock_gbuffer_default_drawelement(framebuffer_ref fbo, const char *effect_name, const char *fragment_source);


const char* stock_gbuffer_using_vertex_shader();

const char* stock_effect_copy_depthbuffer();
const char* stock_effect_headmounted_spot();
const char* stock_effect_hemisphere_lighting();
const char* stock_effect_spot();
 
const char* stock_light_representation_shader();

extern drawelement_ref stock_deferred_copydepth;


#ifdef __cplusplus
}
#endif

#endif

