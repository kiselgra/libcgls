#include "uniforms.h"

#include "drawelement.h"

#include <stdlib.h>

//
void prepend_uniform_handler(struct uniform_handler_node **chain, uniform_setter_t handler) {
	struct uniform_handler_node *cdr = *chain;
	*chain = malloc(sizeof(struct uniform_handler_node));
	(*chain)->handler = handler;
#ifdef WITH_GUILE
	(*chain)->scheme_handler = SCM_BOOL_F;
#endif
	(*chain)->next = cdr;
}

#ifdef WITH_GUILE
void prepend_scheme_uniform_handler(struct uniform_handler_node **chain, SCM handler) {
	struct uniform_handler_node *cdr = *chain;
	*chain = malloc(sizeof(struct uniform_handler_node));
	(*chain)->handler = 0;
	(*chain)->scheme_handler = handler;
	(*chain)->next = cdr;
}
#endif

//! a little slower than \ref prepend_uniform_handler
void append_uniform_handler(struct uniform_handler_node **chain, uniform_setter_t handler) {
	struct uniform_handler_node *node = malloc(sizeof(struct uniform_handler_node));
	node->handler = handler;
#ifdef WITH_GUILE
	node->scheme_handler = SCM_BOOL_F;
#endif
	node->next = 0;
	if (*chain) {
		struct uniform_handler_node *cdr = *chain;
		while (cdr->next) cdr = cdr->next;
		cdr->next = node;
	}
	else
		*chain = node;
}

void bind_handled_uniforms(struct uniform_handler_node *chain, shader_ref shader, void *thing, const char *entity_type, const char *entity_name) {
	for (int i = 0; i < shader_uniforms(shader); ++i) {
		const char *name = shader_uniform_name_by_id(shader, i);
#ifdef WITH_GUILE
		SCM s_name = scm_from_locale_string(name);
#endif
		int loc = shader_uniform_location_by_id(shader, i);
		struct uniform_handler_node *run = chain;
		while (run) {// && !run->handler(ref, name, loc))
			if (run->handler) {
				if (run->handler(thing, name, loc))
					break;
			}
#ifdef WITH_GUILE
			else
				if (scm_is_true(scm_call_3(run->scheme_handler,
								scm_from_int(((drawelement_ref*)thing)->id), 
								s_name, scm_from_int(loc))))	//!< \attention this assumes the same layout for all refs which pass through this!
					break;
#endif
			run = run->next;
		}
		if (!run)
			printf("WARNING: cannot find a handler for uniform %s of shader %s when attached to %s %s.\n", 
					name, shader_name(shader), entity_type, entity_name);
	}

}
