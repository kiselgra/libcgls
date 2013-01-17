#ifndef __CGLS_UNIFORMS_H__ 
#define __CGLS_UNIFORMS_H__ 

#include <libcgl/shader.h>

#ifdef WITH_GUILE
#include <libguile.h>
#endif

#include <stdbool.h>

typedef bool (*uniform_setter_t)(void *ref, const char *uniform, int location);

struct uniform_handler_node {
	struct uniform_handler_node *next;
	uniform_setter_t handler;
#ifdef WITH_GUILE
	SCM scheme_handler;
#endif
};

void prepend_uniform_handler(struct uniform_handler_node **chain, uniform_setter_t handler);
void append_uniform_handler(struct uniform_handler_node **chain, uniform_setter_t handler);

#ifdef WITH_GUILE
void prepend_scheme_uniform_handler(struct uniform_handler_node **chain, SCM handler);
#endif

void bind_handled_uniforms(struct uniform_handler_node *chain, shader_ref shader, void *thing, const char *entity_type, const char *entity_name);

#endif

