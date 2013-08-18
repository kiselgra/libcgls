#include "path.h"

#include <libmcm/matrix.h>
#include <libmcm/camera-matrices.h>

#include "console.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

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
	matrix4x4f trafo;
	animation_time_t animation_start_time;
	float animation_speed;
	bool track_direction;
	bool running;
};

#include <libcgl/mm.h>
define_mm(path_animation, path_animations, path_animation_ref);
#include "path.xx"

path_animation_ref make_path_animation(const char *name) {
	path_animation_ref ref = allocate_path_animation_ref();
	struct path_animation *pa = path_animations + ref.id;
	pa->name = strdup(name);
	pa->nodes = 0;
	pa->node = 0;
	pa->animation_start_time = 0;
	pa->animation_speed = 1;
	pa->track_direction = true;
	pa->running = false;
	make_unit_matrix4x4f(&pa->trafo);

	return ref;
}

void add_node_to_path_animation(path_animation_ref ref, vec3f *p, vec3f *up, float t) {
	struct path_animation *pa = path_animations + ref.id;
	pa->node = realloc(pa->node, sizeof(path_node)*(pa->nodes+1));
	pa->node[pa->nodes].pos = *p;
	if (up)
		pa->node[pa->nodes].up = *up;
	else
		pa->track_direction = false;
	pa->node[pa->nodes].time = t;
	pa->nodes++;
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
	pa->running = true;
}

//! this is for \ref camera-animation interaction, only.
void start_path_animation_with_timestamp(path_animation_ref ref, animation_time_t stamp) {
	struct path_animation *pa = path_animations + ref.id;
	pa->animation_start_time = stamp;
	pa->running = true;
}

void stop_path_animation(path_animation_ref ref) {
	struct path_animation *pa = path_animations + ref.id;
	pa->running = false;
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

//! \attention \c time is not the gloabl timer but the path-time value stored in the nodes.
static vec3f path_position_at(path_animation_ref ref, animation_time_t time, int *N, int *P) {
	struct path_animation *pa = path_animations + ref.id;
	int next = -1, prev = 0;
	find_nodes_for_path_animation(pa, time, &next, &prev);
	if (next < 0) {
		fprintf(stderr, "Error: Invalid path-time in path_position_at(): %f.\n", time);
		vec3f ret = { 0,0,0 };
		return ret;
	}

	// interpolation parameter
	float t = (time - pa->node[prev].time) / (pa->node[next].time - pa->node[prev].time);
	
	// positions
	vec3f p0, p1, p_minus_1, p_plus_2;
	assign_hermite_points(pa, &p_minus_1, &p0, &p1, &p_plus_2, next, prev);

	// interpolate position
	vec3f p;
	hermite_interpolation(&p, &p_minus_1, &p0, &p1, &p_plus_2, t);

	if (P) *P = prev;
	if (N) *N = next;
	return p;
}

void evaluate_path_animation_at(path_animation_ref ref, animation_time_t time) {
	struct path_animation *pa = path_animations + ref.id;
	if (!pa->running)
		return;
	matrix4x4f *mat = path_matrix_of_animation(ref);
	
	time -= pa->animation_start_time;
	time /= 1000;
	time *= pa->animation_speed;

	if (time > pa->node[pa->nodes-1].time) {
		time -= pa->node[pa->nodes-1].time;
		pa->animation_start_time += pa->node[pa->nodes-1].time * 1000 / pa->animation_speed;
	}
	int next = -1, prev = 0;
	vec3f p = path_position_at(ref, time, &next, &prev);
	printf("[%6.6f] p   = %6.6f %6.6f %6.6f\n", time, p.x, p.y, p.z);
	
	if (pa->track_direction) {
		// interpolate slightly advanced position to obtain difference
		time += (pa->node[next].time - pa->node[prev].time)/16;
		vec3f q = path_position_at(ref, time, 0, 0);

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

/*! we approximate the arc length by the sum of a number of linear paths. cheap, yeah :/ */
void normalize_speed_along_path(path_animation_ref ref) {
	struct path_animation *pa = path_animations + ref.id;
	float lengths[pa->nodes-1];
	float path_length = 0;
	for (int i = 0; i < pa->nodes-1; ++i) {
		int segments = 100;
		float arc_start = pa->node[i].time;
		float arc_end = pa->node[i+1].time;
		vec3f start = path_position_at(ref, arc_start, 0, 0), end, diff;
		for (int s = 0; s < segments+1; ++s) {
			end = path_position_at(ref, arc_start + (arc_end-arc_start)*s/(segments+1), 0, 0);
			sub_components_vec3f(&diff, &end, &start);
			path_length += length_of_vec3f(&diff);
			start = end;
		}
		lengths[i] = path_length;
	}
	float expected_length = pa->node[pa->nodes-1].time;
	for (int i = 1; i < pa->nodes; ++i)
		pa->node[i].time = lengths[i-1] * expected_length / path_length;
}

void change_path_node_position(path_animation_ref ref, int node, vec3f pos) {
	struct path_animation *pa = path_animations + ref.id;
	if (pa->nodes >= node) {
		fprintf(stderr, "Error: Node id out of bounds (path animation '%s', node %d).\n", pa->name, node);
		return;
	}
	pa->node[node].pos = pos;
}

void insert_path_node(path_animation_ref ref, vec3f pos, float time) {
	struct path_animation *pa = path_animations + ref.id;
	pa->nodes++;
	pa->node = realloc(pa->node, sizeof(path_node)*pa->nodes);
	pa->node[pa->nodes-1].pos = pos;
	pa->node[pa->nodes-1].time = time;
}

static char* console_path_name(int argc, char **argv, path_animation_ref *ref) {
	if (argc < 2) return strdup("not enough arguments");
	if (isdigit(argv[1][0])) ref->id = atoi(argv[1]);
	else                     *ref = find_path_animation(argv[1]);
	if (ref->id < 0) return strdup("no such path animation");
	return 0;
}

#define path_name_or_else \
	path_animation_ref ref;\
	char *r = console_path_name(argc, argv, &ref);\
	if (r) return r;

static char* console_start_path_animation(console_ref c, int argc, char **argv) {
	path_name_or_else;
	start_path_animation(ref);
	return 0;
}

static char* console_stop_path_animation(console_ref c, int argc, char **argv) {
	path_name_or_else;
	stop_path_animation(ref);
	return 0;
}

static char* console_normalize_path_animation(console_ref c, int argc, char **argv) {
	path_name_or_else;
	normalize_speed_along_path(ref);
	return 0;
}

static char* console_pa_change_node(console_ref c, int argc, char **argv) {
	path_name_or_else;
	if (argc < 3) return strdup("not enough arguments");
	if (!isdigit(argv[2][0])) return strdup("not a node id");
	int id = atoi(argv[2]);
	if (argc < 4) return strdup("missing operation");
	enum { ass, add, sub };
	int op;
	if (argv[3][0] == '=') op = ass;
	else if (argv[3][0] == '+') op = add;
	else if (argv[3][0] == '-') op = sub;
	else return strdup("invalid operand");
	if (argc < 7) return strdup("incomplete position argument");
	vec3f pos;
	pos.x = atof(argv[4]);
	pos.y = atof(argv[5]);
	pos.z = atof(argv[6]);
	struct path_animation *pa = path_animations + ref.id;
	if (op == ass) pa->node[id].pos = pos;
	if (op == add) add_components_vec3f(&pa->node[id].pos, &pa->node[id].pos, &pos);
	if (op == sub) sub_components_vec3f(&pa->node[id].pos, &pa->node[id].pos, &pos);
	return 0;
}

static char* console_help_path_animation(console_ref c, int argc, char **argv) {
	return strdup("options:  pa-start  pa-stop  pa-normalize  pa-nodes");
}

void add_path_commands_to_viconsole(console_ref console) {
	add_vi_console_command(console, "pa-start", console_start_path_animation);
	add_vi_console_command(console, "pa-stop", console_stop_path_animation);
	add_vi_console_command(console, "pa-normalize", console_normalize_path_animation);
	add_vi_console_command(console, "pa-help", console_help_path_animation);
	scm_c_eval_string("(define (console-pa-show-points console args) \
	                     (if (< (length args) 2) \
						     (format #f \"not enough arguments\") \
							 (let ((id (if (string->number (second args)) \
							               (string->number (second args)) \
										   (find-path-animation (second args))))) \
							   (if (< id 0) \
							       (format #f \"no such path animation\") \
                                   (format #f \"~a\" (path-animation-positions id))))))");
	add_vi_console_command_scm(console, "pa-nodes", scm_c_eval_string("console-pa-show-points"));
}

#ifdef WITH_GUILE

#include <libcgl/scheme.h>

SCM_DEFINE(s_make_path_anim, "make-path-animation", 1, 0, 0, (SCM name), "") {
    char *n = scm_to_locale_string(name);
    path_animation_ref ref = make_path_animation(n);
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
	return SCM_BOOL_T;
}

SCM_DEFINE(s_start_pa, "start-path-animation", 1, 0, 0, (SCM pa), "") {
	path_animation_ref ref = { scm_to_int(pa) };
	start_path_animation(ref);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_stop_pa, "stop-path-animation", 1, 0, 0, (SCM pa), "") {
	path_animation_ref ref = { scm_to_int(pa) };
	stop_path_animation(ref);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_pa_nodes_pos, "path-animation-positions", 1, 0, 0, (SCM id), "") {
	path_animation_ref ref = { scm_to_int(id) };
	struct path_animation *pa = path_animations + ref.id;
	SCM l = SCM_EOL;
	for (int i = 0; i < pa->nodes; i++) {
		l = scm_cons(vec3f_to_list(&pa->node[i].pos), l);
	}
	return l;
}

SCM_DEFINE(s_pa_nodes_time, "path-animation-time", 1, 0, 0, (SCM id), "") {
	path_animation_ref ref = { scm_to_int(id) };
	struct path_animation *pa = path_animations + ref.id;
	SCM l = SCM_EOL;
	for (int i = 0; i < pa->nodes; i++) {
		l = scm_cons(scm_from_double(pa->node[i].time), l);
	}
	return l;
}

SCM_DEFINE(s_pa_normalize, "normalize-speed-along-path", 1, 0, 0, (SCM id), "") {
	path_animation_ref ref = { scm_to_int(id) };
	normalize_speed_along_path(ref);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_pa_change_node_pos, "change-path-node-position", 3, 0, 0, (SCM id, SCM nodeid, SCM vec), "") {
	path_animation_ref ref = { scm_to_int(id) };
	vec3f pos = scm_vec_to_vec3f(vec);
	int n = scm_to_int(nodeid);
	change_path_node_position(ref, n, pos);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_pa_insert_node, "insert-path-node", 3, 0, 0, (SCM id, SCM vec, SCM time), "") {
	path_animation_ref ref = { scm_to_int(id) };
	vec3f pos = scm_vec_to_vec3f(vec);
	float t = scm_to_double(time);
	insert_path_node(ref, pos, t);
	return SCM_BOOL_T;
}


void register_scheme_functions_for_path_animation() {
#include "path.x"
}

#endif

