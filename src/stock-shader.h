#ifndef __STOCK_SHADER_H__ 
#define __STOCK_SHADER_H__ 

#include <libcgl/shader.h>

#include "slist.h"

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

void stock_shader(struct stockshader_fragments *ssf, bool ambient_tex, bool diffuse_tex, bool specular_tex, bool mask_tex);

#endif

