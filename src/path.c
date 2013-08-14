#include "path.h"

#include <libmcm/matrix.h>
#include <libmcm/camera-matrices.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
	vec3f pos;
	vec3f up;
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
	bool track_direction;
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
	pa->track_direction = true;
	make_unit_matrix4x4f(&pa->trafo);

	return ref;
}

void add_node_to_path_animation(path_animation_ref ref, vec3f *p, vec3f *up, float t) {
	struct path_animation *pa = path_animations + ref.id;
	if (pa->nodes_set == pa->nodes) {
		fprintf(stderr, "The path animation '%s' is already full. Cannot add further nodes.\n", pa->name);
		return;
	}
	pa->node[pa->nodes_set].pos = *p;
	if (up)
		pa->node[pa->nodes_set].up = *up;
	else
		pa->track_direction = false;
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

static void find_nodes_for_path_animation(struct path_animation *pa, float time, int *next, int *prev) {
	*next = -1;
	for (int i = 0; i < pa->nodes; ++i)
		if (pa->node[i].time > time) {
			*next = i;
			break;
		}
	if (*next > 0) *prev = *next-1;
}

static void assign_hermite_points(struct path_animation *pa, vec3f *p_minus_1, vec3f *p0, vec3f *p1, vec3f *p_plus_2, int next, int prev) {
	*p0 = pa->node[prev].pos;
	*p1 = pa->node[next].pos;
	*p_minus_1 = *p0;
	if (prev > 0) *p_minus_1 = pa->node[prev-1].pos;
	*p_plus_2 = *p1;
	if (next < pa->nodes-1) *p_plus_2 = pa->node[next+1].pos;
}

void evaluate_path_animation_at(path_animation_ref ref, animation_time_t time) {
	struct path_animation *pa = path_animations + ref.id;
	matrix4x4f *mat = path_matrix_of_animation(ref);
	
	time -= pa->animation_start_time;
	time /= 1000;
	time *= pa->animation_speed;

	if (time > pa->node[pa->nodes-1].time) {
		time -= pa->node[pa->nodes-1].time;
		pa->animation_start_time += pa->node[pa->nodes-1].time * 1000 / pa->animation_speed;
	}
	int next = -1, prev = 0;
	find_nodes_for_path_animation(pa, time, &next, &prev);
	if (next < 0) 
		return;

	// interpolation parameter
	float t = (time - pa->node[prev].time) / (pa->node[next].time - pa->node[prev].time);

	// positions
	vec3f p0, p1, p_minus_1, p_plus_2;
	assign_hermite_points(pa, &p_minus_1, &p0, &p1, &p_plus_2, next, prev);

	// interpolate position
	vec3f p;
	hermite_interpolation(&p, &p_minus_1, &p0, &p1, &p_plus_2, t);
	
	if (pa->track_direction) {
		// interpolate slightly advanced position to obtain difference
		time += (pa->node[next].time - pa->node[prev].time)/16;
		float t = (time - pa->node[prev].time) / (pa->node[next].time - pa->node[prev].time);

		next = -1, prev = 0;
		find_nodes_for_path_animation(pa, time, &next, &prev);

		vec3f q;
		hermite_interpolation(&q, &p_minus_1, &p0, &p1, &p_plus_2, t);

		vec3f dir;
		sub_components_vec3f(&dir, &q, &p);
		normalize_vec3f(&dir);

		vec3f unit = { 0,0,-1 };
		float cos_alpha = dot_vec3f(&unit, &dir);
		float alpha = -acosf(cos_alpha);
		if (dir.x < 0)
			alpha = 2 * M_PI - alpha;

		vec3f axis = { 0, 1, 0 };
		make_rotation_matrix4x4f(mat, &axis, alpha);

		mat->col_major[12] = p.x;
		mat->col_major[13] = p.y;
		mat->col_major[14] = p.z;
	}
	else {
		mat->col_major[12] = p.x;
		mat->col_major[13] = p.y;
		mat->col_major[14] = p.z;
	}
}

#ifdef WITH_GUILE

#include <libcgl/scheme.h>

SCM_DEFINE(s_make_path_anim, "make-path-animation", 2, 0, 0, (SCM name, SCM nodes), "") {
    char *n = scm_to_locale_string(name);
    int i = scm_to_int(nodes);
    path_animation_ref ref = make_path_animation(n, i);
    free(n);
	return scm_from_int(ref.id);
}

SCM_DEFINE(s_find_path_anim, "find-path-animation", 1, 0, 0, (SCM name), "") {
    char *n = scm_to_locale_string(name);
    path_animation_ref ref = find_path_animation(n);
    free(n);
	return scm_from_int(ref.id);
}

SCM_DEFINE(s_add_node_to_pa, "add-node-to-path-animation", 4, 0, 0, (SCM pa, SCM pos, SCM up, SCM t), "") {
	path_animation_ref ref = { scm_to_int(pa) };
	vec3f p = scm_vec_to_vec3f(pos);
	vec3f uv, *u = 0;
	if (scm_is_pair(up)) {
		uv = scm_vec_to_vec3f(up);
		u = &uv;
	}
	float time = scm_to_double(t);
	add_node_to_path_animation(ref, &p, u, time);
	return scm_from_bool(path_animations[ref.id].nodes_set < path_animations[ref.id].nodes);
}

SCM_DEFINE(s_start_pa, "start-path-animation", 1, 0, 0, (SCM pa), "") {
	path_animation_ref ref = { scm_to_int(pa) };
	start_path_animation(ref);
	return SCM_BOOL_T;
}

void register_scheme_functions_for_path_animation() {
#include "path.x"
}

#endif

