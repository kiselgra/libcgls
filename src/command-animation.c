#include "command-animation.h"

#include <libcgl/scheme.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

// well, it's not really an animation, but sequence does not sound right, either.

typedef struct {
	char *command;
	float time;
} command_node;

struct command_animation {
	char *name;
	int nodes;
	command_node *node;
	animation_time_t animation_start_time;
	float animation_speed;
	float last_time_evaluated;	//!< stores node-time, not global time.
	bool running;
};

#include <libcgl/mm.h>
define_mm(command_animation, command_animations, command_animation_ref);
#include "command-animation.xx"

command_animation_ref make_command_animation(const char *name) {
	command_animation_ref ref = allocate_command_animation_ref();
	struct command_animation *pa = command_animations + ref.id;
	pa->name = strdup(name);
	pa->nodes = 0;
	pa->node = 0;
	pa->animation_start_time = 0;
	pa->animation_speed = 1;
	pa->last_time_evaluated = -1;
	pa->running = false;
	return ref;
}

void add_node_to_command_animation(command_animation_ref ref, const char *command, float t) {
	struct command_animation *pa = command_animations + ref.id;
	pa->node = realloc(pa->node, sizeof(command_node)*(pa->nodes+1));
	pa->node[pa->nodes].command = strdup(command);
	pa->node[pa->nodes].time = t;
	pa->nodes++;
}

command_animation_ref find_command_animation(const char *name) {
	command_animation_ref ref = { -1 };
	if (strlen(name) == 0) return ref;
    for (int i = 0; i < next_command_animation_index; ++i) {
        if (strcmp(command_animations[i].name, name) == 0) {
			ref.id = i;
			return ref;
		}
	}
	return ref;
}

struct command_animation_list* list_command_animations() {
	if (next_command_animation_index == 0)
		return 0;
	struct command_animation_list *head = malloc(sizeof(struct command_animation_list));
	struct command_animation_list *run = head;
	run->ref.id = 0;
	for (int i = 1; i < next_command_animation_index; ++i) {
		run = run->next = malloc(sizeof(struct command_animation_list));
		run->ref.id = i;
	}
	run->next = 0;
	return head;
}

void change_command_animation_speed(command_animation_ref ref, float factor) {
	struct command_animation *pa = command_animations+ref.id;
	pa->animation_speed = factor;
}

float command_animation_speed(command_animation_ref ref, float factor) {
	struct command_animation *pa = command_animations+ref.id;
	return pa->animation_speed;
}

void start_command_animation(command_animation_ref ref) {
	struct command_animation *pa = command_animations + ref.id;
	pa->animation_start_time = animation_time_stamp();
	pa->running = true;
}

void stop_command_animation(command_animation_ref ref) {
	struct command_animation *pa = command_animations + ref.id;
	pa->running = false;
}

/*! \attention \c time is not the global timer but the animation time value stored in the nodes.
 *  \note the range is (start,end].
 */
static void evaluate_commands_between(command_animation_ref ref, animation_time_t start, animation_time_t end) {
#ifdef WITH_GUILE
	struct command_animation *ca = command_animations + ref.id;
	for (int i = 0; i < ca->nodes; ++i)
		if (ca->node[i].time > start && ca->node[i].time <= end)
			scm_c_eval_string(ca->node[i].command);
#endif
}

void evaluate_command_animation_at(command_animation_ref ref, animation_time_t time) {
	struct command_animation *ca = command_animations + ref.id;
	if (!ca->running)
		return;
	
	time -= ca->animation_start_time;
	time /= 1000;
	time *= ca->animation_speed;

	evaluate_commands_between(ref, ca->last_time_evaluated, time);
	ca->last_time_evaluated = time;
}


#ifdef WITH_GUILE

SCM_DEFINE(s_make_com_anim, "make-command-animation", 1, 0, 0, (SCM name), "") {
	char *n = scm_to_locale_string(name);
	command_animation_ref ref = make_command_animation(n);
	free(n);
	return scm_from_int(ref.id);
}

SCM_DEFINE(s_find_command_anim, "find-command-animation", 1, 0, 0, (SCM name), "") {
    char *n = scm_to_locale_string(name);
    command_animation_ref ref = find_command_animation(n);
    free(n);
	return scm_from_int(ref.id);
}

SCM_DEFINE(s_add_node_to_comm_a, "add-node-to-command-animation", 3, 0, 0, (SCM ca, SCM comm, SCM t), "") {
	command_animation_ref ref = { scm_to_int(ca) };
	char *command = scm_to_locale_string(comm);
	float time = scm_to_double(t);
	add_node_to_command_animation(ref, command, time);
	free(command);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_start_comm_a, "start-command-animation", 1, 0, 0, (SCM ca), "") {
	command_animation_ref ref = { scm_to_int(ca) };
	start_command_animation(ref);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_stop_comm_a, "stop-command-animation", 1, 0, 0, (SCM ca), "") {
	command_animation_ref ref = { scm_to_int(ca) };
	stop_command_animation(ref);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_change_comm_a_speed, "change-command-animation-speed!", 2, 0, 0, (SCM ca, SCM factor), "") {
	command_animation_ref ref = { scm_to_int(ca) };
	float f = scm_to_double(factor);
	change_command_animation_speed(ref, f);
	return SCM_BOOL_T;
}


void register_scheme_functions_for_command_animation() {
#include "command-animation.x"
}

#endif


