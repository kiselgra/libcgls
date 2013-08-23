#include "uniforms.h"

#include "drawelement.h"
#include "c-utils.h"

#include <stdlib.h>

/*! \defgroup uniforms Uniform Handling
 *
 * 	Uniforms are handled via `handler chains'; linked lists containing functions (C or Scheme) 
 * 		which check if they can provide a given uniform, and set its value accordingly.
 * 	Priority can be established by prepending or appending to a chain.
 *
 * 	Currently, only \ref drawelements do have handler chains.
 * 	But this might change in a later redesign for more general uniform handling.
 */

/*! \file uniforms.h
 *  \ingroup uniforms
 */

//! debugging only.
static void print_chain(struct uniform_handler_node *chain) {
	int i = 0;
	bool flag = false;
	for (struct uniform_handler_node *run = chain; run; run = run->next) {
		printf("node %d:    c: %d   scm: %d\n", i++, run->handler==0 ? 0 : 1, scm_is_false(run->scheme_handler) ? 0 : 1);
		if (run->handler == 0 && scm_is_false(run->scheme_handler))
			flag = true;
	}
	if (flag) {
		printf("bad!\n");
		((int*)0)[0] = 9;
		exit(-1);
	}
}

/*! \addtogroup uniforms
 * 	@{
 */

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

void pop_uniform_handler(struct uniform_handler_node **chain) {
	struct uniform_handler_node *head = *chain;
	*chain = head->next;
	free(head);
}

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

#ifdef WITH_GUILE
//! a little slower than \ref prepend_uniform_handler
void append_scheme_uniform_handler(struct uniform_handler_node **chain, SCM handler) {
	struct uniform_handler_node *node = malloc(sizeof(struct uniform_handler_node));
	node->handler = 0;
	node->scheme_handler = handler;
	node->next = 0;
	if (*chain) {
		struct uniform_handler_node *cdr = *chain;
		while (cdr->next) cdr = cdr->next;
		cdr->next = node;
	}
	else
		*chain = node;
}
#endif

define_slist(global_uniform_handler_node, void *ref; uniform_setter_t handler);
struct global_uniform_handler_node *global_handlers = 0;

void bind_handled_uniforms(struct uniform_handler_node *chain, shader_ref shader, void *thing, const char *entity_type, const char *entity_name) {
	for (int i = 0; i < shader_uniforms(shader); ++i) {
		const char *name = shader_uniform_name_by_id(shader, i);
#ifdef WITH_GUILE
		SCM s_name;
		if (cgl_use_guile) 
			s_name = scm_from_locale_string(name);
#endif
		int loc = shader_uniform_location_by_id(shader, i);
		struct uniform_handler_node *run = chain;
		while (run) {// && !run->handler(ref, name, loc))
			if (run->handler) {
				if (run->handler(thing, name, loc))
					break;
			}
#ifdef WITH_GUILE
			else if (cgl_use_guile)
				if (scm_is_true(scm_call_3(run->scheme_handler,
								scm_from_int(((drawelement_ref*)thing)->id), 
								s_name, scm_from_int(loc))))	//!< \attention this assumes the same layout for all refs which pass through this!
					break;
#endif
			run = run->next;
		}
		if (!run) {
			struct global_uniform_handler_node *node = global_handlers; 
			while (node) {
				if ((node->handler)(node->ref, name, loc))
					break;
				node = node->next;
			}
			if (!node)
				printf("WARNING: cannot find a handler for uniform %s of shader %s when attached to %s %s.\n", 
						name, shader_name(shader), entity_type, entity_name);
		}
	}
}

//! @}

/*! \defgroup globaluniforms Global Uniform Handling
 * 	\ingroup uniforms
 *
 * 	To support handling uniforms which are not associated with a drawelement,
 * 	but some other element of the scene we provide `global uniform handlers'.
 *
 * 	They are global in the sense that drawelement uniform values (as well as the
 * 	set of handled uniforms itself) varies between different drawelements:
 * 	global uniform handling does not change by drawelements.
 *
 * 	An example, and the original motivation for this, is handling of lights.
 * 	To set uniforms depending on a light's properties, the light itself
 * 	has to be accessible (via the void *ref).
 * 	So when rendering a deferred pass to apply a specific light effect, the 
 * 	drawelement uniform handling is extended by light specific uniform handling.
 *
 * 	\note Any global setting can be overridden by specifying a drawelement uniform
 * 			handler that accepts a uniform of the given name.
 *
 * 	\note We don't have global scheme handlers just yet.
 */


/*! \addtogroup globaluniforms
 * 	@{
 */

void push_global_uniform_handler(void *ref, uniform_setter_t handler) {
	struct global_uniform_handler_node *node = malloc(sizeof(struct global_uniform_handler_node));
	node->next = global_handlers;
	node->ref = ref;
	node->handler = handler;
	global_handlers = node;
}

void* pop_global_uniform_handler() {
	if (global_handlers) {
		struct global_uniform_handler_node *old = global_handlers;
		void *ref = old->ref;
		global_handlers = old->next;
		free(old);
		return ref;
	}
	return 0;
}

//! @}


