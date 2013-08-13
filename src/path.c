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
	animation_time_t animation_start_time;
	float animation_speed;
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
	pa->animation_start_time = 0;
	pa->animation_speed = 1;
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

void change_path_animation_speed(path_animation_ref ref, float factor) {
	struct path_animation *pa = path_animations+ref.id;
	pa->animation_speed = factor;
}

float path_animation_speed(path_animation_ref ref, float factor) {
	struct path_animation *pa = path_animations+ref.id;
	return pa->animation_speed;
}

void start_path_animation(path_animation_ref ref) {
	struct path_animation *pa = path_animations + ref.id;
	pa->animation_start_time = animation_time_stamp();
}

matrix4x4f* path_matrix_of_animation(path_animation_ref ref) {
	return &path_animations[ref.id].trafo;
}

void evaluate_path_animation_at(path_animation_ref ref, animation_time_t time) {
	struct path_animation *pa = path_animations + ref.id;
	matrix4x4f *mat = path_matrix_of_animation(ref);
	
	time -= pa->animation_start_time;
	printf("t1 = %f\n", time);
	time /= 1000;
	time *= pa->animation_speed;
	printf("t2 = %f\n", time);

	int next = -1, prev = 0;
	for (int i = 0; i < pa->nodes; ++i) {
		printf("i = %d, t = %f\n", i, pa->node[i].time);
		if (pa->node[i].time > time) {
			next = i;
			break;
		}
	}
	if (next > 0) prev = next-1;
	if (next < 0) return;

	/*
	// linear interpolation
	float alpha = (time - pa->node[prev].time) / (pa->node[next].time - pa->node[prev].time);
	printf("p = %d, n = %d, a = %f\n", prev, next, alpha);
	vec3f p = pa->node[prev].pos;
	vec3f q = pa->node[next].pos;
	mul_vec3f_by_scalar(&p, &p, (1-alpha));
	mul_vec3f_by_scalar(&q, &q, alpha);
	add_components_vec3f(&p, &p, &q);
	*/

	// hermite interpolation
	float t = (time - pa->node[prev].time) / (pa->node[next].time - pa->node[prev].time);
	float t2 = t*t;
	float t3 = t2*t;
	float h00 = 2*t3 - 3*t2 + 1;
	float h10 = -2*t3 + 3*t2;
	float h01 = t3 - 2*t2 + t;
	float h11 = t3 - t2;

	// positions
	vec3f p0 = pa->node[prev].pos;
	vec3f p1 = pa->node[next].pos;
	vec3f p_minus_one = p0;
	if (prev > 0) p_minus_one = pa->node[prev-1].pos;
	vec3f p_plus_2 = p1;
	if (next < pa->nodes-1) p_plus_2 = pa->node[next+1].pos;
	
	// tangents
	vec3f m0, m1;
	// m0
	sub_components_vec3f(&p_minus_one, &p0, &p_minus_one);
	sub_components_vec3f(&m0, &p1, &p0);
	add_components_vec3f(&m0, &m0, &p_minus_one);
	div_vec3f_by_scalar(&m0, &m0, 2);
	// m1
	sub_components_vec3f(&p_plus_2, &p_plus_2, &p1);
	sub_components_vec3f(&m1, &p1, &p0);
	add_components_vec3f(&m1, &m1, &p_plus_2);
	div_vec3f_by_scalar(&m1, &m1, 2);

	// combination
	vec3f p;
	mul_vec3f_by_scalar(&p0, &p0, h00);
	mul_vec3f_by_scalar(&p1, &p1, h10);
	mul_vec3f_by_scalar(&m0, &m0, h01);
	mul_vec3f_by_scalar(&m1, &m1, h11);
	add_components_vec3f(&p, &p0, &p1);
	add_components_vec3f(&p, &p, &m0);
	add_components_vec3f(&p, &p, &m1);

	mat->col_major[12] = p.x;
	mat->col_major[13] = p.y;
	mat->col_major[14] = p.z;
}

