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

// void stock_shader(struct stockshader_fragments *ssf, bool ambient_tex, bool diffuse_tex, bool specular_tex, bool mask_tex, int bones);
void add_stock_vertex_shader_part(struct stockshader_fragments *ssf, bool use_normals, bool use_tc, int bones, bool path, bool instanced);
void add_stock_fragment_shader_part(struct stockshader_fragments *ssf, bool use_normals, bool ambient_tex, bool diffuse_tex, bool specular_tex, bool mask_tex);
struct stockshader_fragments* make_stock_shader_fragments(drawelement_ref de, struct stockshader_fragments *ssf, bool consider_material, const char *frag_source);
shader_ref make_stock_shader(const char *name, drawelement_ref de, struct stockshader_fragments *ssf, bool consider_material, const char *fragment_base);

bool compile_and_link_shader_showing_log_on_error(shader_ref shader);

void upload_instance_matrices(texture_ref target, matrix4x4f *mat, int n);
texture_ref make_buffer_for_instance_trafos(const char *name, int n);

// deferred part

enum {
    stock_ds_flag_position_in_world = 01,
};

framebuffer_ref make_stock_deferred_buffer(const char *name, unsigned int width, unsigned int height, 
                                           GLenum diffuse_format, GLenum specular_format, GLenum normal_format, 
                                           GLenum position_format, GLenum depth_format);
drawelement_ref make_stock_gbuffer_default_drawelement(framebuffer_ref fbo, const char *effect_name, const char *fragment_source);
drawelement_ref make_stock_gbuffer_default_drawelement_with_shader(framebuffer_ref fbo, const char *effect_name, shader_ref shader);


const char* stock_gbuffer_using_vertex_shader(void);

const char* stock_effect_copy_depthbuffer(void);
const char* stock_effect_headmounted_spot(void);
const char* stock_effect_hemisphere_lighting(void);
const char* stock_effect_ambient_light(void);
const char* stock_effect_spot(void);
 
const char* stock_light_representation_shader(void);

extern drawelement_ref stock_deferred_copydepth;


#ifdef __cplusplus
}
#endif

#endif

