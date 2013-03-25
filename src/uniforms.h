#ifndef __CGLS_UNIFORMS_H__ 
#define __CGLS_UNIFORMS_H__ 

#include <libcgl/shader.h>

#ifdef WITH_GUILE
#include <libguile.h>
#endif

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef bool (*uniform_setter_t)(void *ref, const char *uniform, int location);

struct uniform_handler_node {
	struct uniform_handler_node *next;
	uniform_setter_t handler;
#ifdef WITH_GUILE
	SCM scheme_handler;
#endif
};

void prepend_uniform_handler(struct uniform_handler_node **chain, uniform_setter_t handler); //!< prepend == push
void append_uniform_handler(struct uniform_handler_node **chain, uniform_setter_t handler);

#ifdef WITH_GUILE
void prepend_scheme_uniform_handler(struct uniform_handler_node **chain, SCM handler);
void append_scheme_uniform_handler(struct uniform_handler_node **chain, SCM handler);
#endif

void pop_uniform_handler(struct uniform_handler_node **chain);

void bind_handled_uniforms(struct uniform_handler_node *chain, shader_ref shader, void *thing, const char *entity_type, const char *entity_name);


// - - -


void push_global_uniform_handler(void *ref, uniform_setter_t handler);
void* pop_global_uniform_handler();


#ifdef __cplusplus
}
#endif

#endif

