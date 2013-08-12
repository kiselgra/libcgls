#include "path.h"

#include <libmcm/matrix.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
	vec3f pos;
	float time;
} path_node;

define_slist(path_node_list, path_node node);

struct path_animation {
	char *name;
	int nodes;
	path_node *node;
	int nodes_set;
	matrix4x4f trafo;
};

#include <libcgl/mm.h>
define_mm(path_animation, path_animations, path_animation_ref);
#include "path.xx"

path_animation_ref make_path_animation(const char *name, int nodes) {
	path_animation_ref ref = allocate_path_animation_ref();
	struct path_animation *pa = path_animations + ref.id;
	pa->name = strdup(name);
	pa->nodes = nodes;
	pa->node = malloc(sizeof(path_node)*nodes);
	pa->nodes_set = 0;
	make_unit_matrix4x4f(&pa->trafo);

	return ref;
}

void add_node_to_path_animation(path_animation_ref ref, vec3f *p, float t) {
	struct path_animation *pa = path_animations + ref.id;
	if (pa->nodes_set == pa->nodes) {
		fprintf(stderr, "The path animation '%s' is already full. Cannot add further nodes.\n", pa->name);
		return;
	}
	pa->node[pa->nodes_set].pos = *p;
	pa->node[pa->nodes_set].time = t;
	pa->nodes_set++;
}

path_animation_ref find_path_animation(const char *name) {
	path_animation_ref ref = { -1 };
	if (strlen(name) == 0) return ref;
    for (int i = 0; i < next_path_animation_index; ++i) {
        if (strcmp(path_animations[i].name, name) == 0) {
			ref.id = i;
			return ref;
		}
	}
	return ref;
}

struct path_animation_list* list_path_animations() {
	if (next_path_animation_index == 0)
		return 0;
	struct path_animation_list *head = malloc(sizeof(struct path_animation_list));
	struct path_animation_list *run = head;
	run->ref.id = 0;
	for (int i = 1; i < next_path_animation_index; ++i) {
		run = run->next = malloc(sizeof(struct path_animation_list));
		run->ref.id = i;
	}
	run->next = 0;
	return head;
}

void start_path_animation(path_animation_ref ref) {
}

matrix4x4f* path_matrix_of_animation(path_animation_ref ref) {
	return &path_animations[ref.id].trafo;
}

void evaluate_path_animation_at(path_animation_ref ref, animation_time_t time) {
	matrix4x4f *mat = path_matrix_of_animation(ref);
	mat->col_major[14] += 0.01;
}

