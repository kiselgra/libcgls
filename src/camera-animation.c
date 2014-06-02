#include "camera-animation.h"

#include <libmcm/matrix.h>
#include <libmcm/camera-matrices.h>

#include "path.h"
#include "console.h"
#include "animation.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

typedef struct {
	vec3f dir;
	vec3f up;
	float time;
} cam_node;

struct camera_animation {
	char *name;
	path_animation_ref path;
	int nodes;
	cam_node *node;
	camera_ref cam;
	float animation_speed;
	bool running;
	animation_time_t animation_start_time;
};

#include <libcgl/mm.h>
define_mm(camera_animation, camera_animations, camera_animation_ref);
#include "camera-animation.xx"

camera_animation_ref make_camera_animation(const char *name, camera_ref cam) {
	camera_animation_ref ref = allocate_camera_animation_ref();
	struct camera_animation *ca = camera_animations + ref.id;
	ca->name = strdup(name);
	ca->nodes = 0;
	ca->node = 0;
	ca->cam = cam;
	ca->animation_speed = 1;
	ca->running = false;

	ca->path = make_path_animation(name);

	return ref;
}

void add_node_to_camera_animation(camera_animation_ref ref, vec3f *pos, vec3f *dir, vec3f *up, float t) {
	struct camera_animation *ca = camera_animations + ref.id;
	ca->node = realloc(ca->node, sizeof(cam_node)*(ca->nodes+1));
	cam_node *node = ca->node+ca->nodes;
	node->time = t;
	node->dir = *dir;
	node->up = *up;
	add_node_to_path_animation(ca->path, pos, 0, t);
	ca->nodes++;
}

void add_current_view_to_camera_animation(camera_animation_ref ref, float t) {
	struct camera_animation *ca = camera_animations + ref.id;
	ca->node = realloc(ca->node, sizeof(cam_node)*(ca->nodes+1));
	cam_node *node = ca->node+ca->nodes;

	matrix4x4f *lookat = lookat_matrix_of_cam(ca->cam);
	extract_dir_vec3f_of_matrix(&node->dir, lookat);
	extract_up_vec3f_of_matrix(&node->up, lookat);
	vec3f pos;
	extract_pos_vec3f_of_matrix(&pos, lookat);
	node->time = t;

	add_node_to_path_animation(ca->path, &pos, 0, t);

	ca->nodes++;
}

camera_animation_ref find_camera_animation(const char *name) {
	camera_animation_ref ref = { -1 };
	if (strlen(name) == 0) return ref;
    for (int i = 0; i < next_camera_animation_index; ++i) {
        if (strcmp(camera_animations[i].name, name) == 0) {
			ref.id = i;
			return ref;
		}
	}
	return ref;
}

struct camera_animation_list* list_camera_animations() {
	if (next_camera_animation_index == 0)
		return 0;
	struct camera_animation_list *head = malloc(sizeof(struct camera_animation_list));
	struct camera_animation_list *run = head;
	run->ref.id = 0;
	for (int i = 1; i < next_camera_animation_index; ++i) {
		run = run->next = malloc(sizeof(struct camera_animation_list));
		run->ref.id = i;
	}
	run->next = 0;
	return head;
}

void change_camera_animation_speed(camera_animation_ref ref, float factor) {
	struct camera_animation *ca = camera_animations+ref.id;
	ca->animation_speed = factor;
}

float camera_animation_speed(camera_animation_ref ref, float factor) {
	struct camera_animation *ca = camera_animations+ref.id;
	return ca->animation_speed;
}

void start_path_animation_with_timestamp(path_animation_ref ref, animation_time_t stamp);

void start_camera_animation_with_timestamp(camera_animation_ref ref, animation_time_t stamp) {
	struct camera_animation *ca = camera_animations + ref.id;
	ca->animation_start_time = stamp;
	ca->running = true;
	start_path_animation_with_timestamp(ca->path, ca->animation_start_time);
}

void start_camera_animation(camera_animation_ref ref) {
	struct camera_animation *ca = camera_animations + ref.id;
	ca->animation_start_time = animation_time_stamp();
	ca->running = true;
	start_path_animation_with_timestamp(ca->path, ca->animation_start_time);
}

void stop_camera_animation(camera_animation_ref ref) {
	struct camera_animation *ca = camera_animations + ref.id;
	ca->running = false;
	stop_path_animation(ca->path);
}

path_animation_ref camera_animation_path(camera_animation_ref ref) {
	struct camera_animation *ca = camera_animations + ref.id;
	return ca->path;
}

int camera_animation_nodes(camera_animation_ref ref) {
	return camera_animations[ref.id].nodes;
}

float camera_animation_time(camera_animation_ref ref, int n) {
	return camera_animations[ref.id].node[n].time;
}

static void find_nodes_for_cam_animation(struct camera_animation *ca, float time, int *next, int *prev) {
	*next = -1;
	for (int i = 0; i < ca->nodes; ++i)
		if (ca->node[i].time > time) {
			*next = i;
			break;
		}
	if (*next > 0) *prev = *next-1;
}

//! \attention \c time is not the gloabl timer but the path-time value stored in the nodes.
void set_camera_to_animation_state_at(camera_animation_ref ref, animation_time_t time) {
	struct camera_animation *ca = camera_animations + ref.id;
	matrix4x4f *mat = path_matrix_of_animation(ca->path);
	
	// update path
	vec3f path_position_at(path_animation_ref ref, animation_time_t time, int *N, int *P);
	vec3f pos = path_position_at(ca->path, time, 0, 0);
	
	int p = 0, n = -1;
	find_nodes_for_cam_animation(ca, time, &n, &p);

	if (p == n)
		make_lookat_matrixf(lookat_matrix_of_cam(ca->cam), &pos, &ca->node[n].dir, &ca->node[n].up);
	else {
		float t = (time - ca->node[p].time) / (ca->node[n].time - ca->node[p].time);
	

		quaternionf dir_from, dir_to, dir;
		make_quaternionf(&dir_from, &ca->node[p].dir, 0);
		make_quaternionf(&dir_to, &ca->node[n].dir, 0);
		slerp_quaterionf_noflip(&dir, &dir_from, &dir_to, t);

		quaternionf up_from, up_to, up;
		make_quaternionf(&up_from, &ca->node[p].up, 0);
		make_quaternionf(&up_to, &ca->node[n].up, 0);
		slerp_quaterionf_noflip(&up, &up_from, &up_to, t);

		make_lookat_matrixf(lookat_matrix_of_cam(ca->cam), &pos, &dir.v, &up.v);
	}
	recompute_gl_matrices_of_cam(ca->cam);
}

void evaluate_camera_animation_at(camera_animation_ref ref, animation_time_t time) {
	struct camera_animation *ca = camera_animations + ref.id;
	if (!ca->running)
		return;
	
	time -= ca->animation_start_time;
	time /= 1000;
	time *= ca->animation_speed;
	
	if (time > ca->node[ca->nodes-1].time) {
		time -= ca->node[ca->nodes-1].time;
		ca->animation_start_time += ca->node[ca->nodes-1].time * 1000 / ca->animation_speed;
	}

	set_camera_to_animation_state_at(ref, time);
}

void normalize_camera_speed_along_path(camera_animation_ref ref) {
	struct camera_animation *ca = camera_animations + ref.id;
	normalize_speed_along_path(ca->path);
	for (int i = 0; i < ca->nodes; ++i)
		ca->node[i].time = path_animation_node_time(ca->path, i);
}

char* camera_animation_script(camera_animation_ref ref) {
	struct camera_animation *ca = camera_animations + ref.id;
	int nodes = ca->nodes;
	char *lines[nodes+1];
	for (int i = 0; i < nodes+1; ++i)
		lines[i] = 0;
	int c = asprintf(lines, "(let ((ca (make-camera-animation \"%s\" (find-camera \"%s\"))))\n", ca->name, camera_name(ca->cam));
	if (c <= 0) perror("asprintf in camera_animation_script");
	for (int i = 0; i < nodes; ++i) {
		vec3f pos = path_animation_node_position(ca->path, i);
		c=asprintf(lines+i+1, "  (add-node-to-camera-animation ca (list %f %f %f) (list %f %f %f) (list %f %f %f) %f)\n",
				 pos.x, pos.y, pos.z,
		         ca->node[i].dir.x, ca->node[i].dir.y, ca->node[i].dir.z,
				 ca->node[i].up.x, ca->node[i].up.y, ca->node[i].up.z,
				 ca->node[i].time);
		if (c <= 0) perror("asprintf in camera_animation_script");
	}
	int len = 0;
	for (int i = 0; i < nodes+1; ++i)
		len += strlen(lines[i]);
	char *res = malloc(len+1);
	res[0] = 0;
	for (int i = 0; i < nodes+1; ++i) {
		strcat(res, lines[i]);
		free(lines[i]);
	}
	res[len-1] = ')';
	return res;
}

// 
// console
//

#define anim_name_or_else \
	camera_animation_ref ref;\
	char *r = console_anim_name(argc, argv, &ref);\
	if (r) return r;
#define get_t_at_2_or_else \
	if (argc < 3) return strdup("not enough arguments"); \
	if (!isdigit(argv[2][0])) return strdup("parameter: not a number"); \
	float t = atof(argv[2]);

static char* console_make_ca(console_ref ref, int argc, char **argv) {
	if (argc < 2) return strdup("name of animation is missing");
	char *n = 0;
	if (argc > 3) return strdup("too many arguments");
	if (argc == 3) n = argv[2];
	camera_ref c = { -1 };
	if (n == 0)
		c = current_camera();
	else
		c = find_camera(n);
	if (!valid_camera_ref(c))
		return strdup("no such camera");
	make_camera_animation(argv[1], c);
	return strdup("made new cam anim");
}

static char* console_anim_name(int argc, char **argv, camera_animation_ref *ref) {
	if (argc < 2) return strdup("not enough arguments");
	if (isdigit(argv[1][0])) ref->id = atoi(argv[1]);
	else                     *ref = find_camera_animation(argv[1]);
	if (ref->id < 0) return strdup("no such camera animation");
	return 0;
}

static char* console_start_camera_animation(console_ref c, int argc, char **argv) {
	anim_name_or_else;
	start_camera_animation(ref);
	return 0;
}

static char* console_stop_camera_animation(console_ref c, int argc, char **argv) {
	anim_name_or_else;
	stop_camera_animation(ref);
	return 0;
}

static char* console_speed_of_camera_animation(console_ref c, int argc, char **argv) {
	anim_name_or_else;
	get_t_at_2_or_else;
	change_camera_animation_speed(ref, t);
	return 0;
}

static char* console_take_ca_sample(console_ref c, int argc, char **argv) {
	anim_name_or_else;
	get_t_at_2_or_else;
	add_current_view_to_camera_animation(ref, t);
	return 0;
}

static char* console_set_camera_to_parameter(console_ref c, int argc, char **argv) {
	anim_name_or_else;
	get_t_at_2_or_else;
	set_camera_to_animation_state_at(ref, t);
	return 0;
}

static char* console_help_camera_animation(console_ref c, int argc, char **argv) {
	return strdup("options:  ca-start  ca-stop  ca-add  ca-go  ca-speed  ca-show");
}

void add_cam_anim_commands_to_viconsole(console_ref console) {
	add_vi_console_command(console, "ca-start", console_start_camera_animation);
	add_vi_console_command(console, "ca-stop", console_stop_camera_animation);
	add_vi_console_command(console, "ca-add", console_take_ca_sample);
	add_vi_console_command(console, "ca-go", console_set_camera_to_parameter);
	add_vi_console_command(console, "ca-speed", console_speed_of_camera_animation);
	add_vi_console_command(console, "ca-help", console_help_camera_animation);
	add_vi_console_command(console, "ca-new", console_make_ca);
#ifdef WITH_GUILE
	if (cgl_use_guile) {
		scm_c_eval_string("(define (console-ca-show-points console args) \
							 (if (< (length args) 2) \
								 (format #f \"not enough arguments\") \
								 (let ((id (if (string->number (second args)) \
											   (string->number (second args)) \
											   (find-path-animation (second args))))) \
								   (if (< id 0) \
									   (format #f \"no such path animation\") \
									   (format #f \"~a\" (camera-animation-config id))))))");
		add_vi_console_command_scm(console, "ca-show", scm_c_eval_string("console-ca-show-points"));
	}
#endif
}

#ifdef WITH_GUILE

SCM_DEFINE(s_make_cam_anim, "make-camera-animation", 2, 0, 0, (SCM name, SCM camref), "") {
	char *n = scm_to_locale_string(name);
	camera_ref c = { scm_to_int(camref) };
	camera_animation_ref ref = make_camera_animation(n, c);
	free(n);
	return scm_from_int(ref.id);
}

SCM_DEFINE(s_find_camera_anim, "find-camera-animation", 1, 0, 0, (SCM name), "") {
    char *n = scm_to_locale_string(name);
    camera_animation_ref ref = find_camera_animation(n);
    free(n);
	return scm_from_int(ref.id);
}

SCM_DEFINE(s_add_node_to_ca, "add-node-to-camera-animation", 5, 0, 0, (SCM ca, SCM pos, SCM dir, SCM up, SCM t), "") {
	camera_animation_ref ref = { scm_to_int(ca) };
	vec3f p = { 0, 0, 0}, d= { 0, 0, 0 }, u = { 0, 0, 0 };
	if (scm_is_pair(pos))  p  = scm_vec_to_vec3f(pos);
	else                   { fprintf(stderr, "Invalid value for 'pos'\n"); return SCM_BOOL_F; }
	if (scm_is_pair(dir))  d = scm_vec_to_vec3f(dir);
	else                   { fprintf(stderr, "Invalid value for 'dir'\n"); return SCM_BOOL_F; }
	if (scm_is_pair(up))   u = scm_vec_to_vec3f(up);
	else                   { fprintf(stderr, "Invalid value for 'up'\n"); return SCM_BOOL_F; }
		
	float time = scm_to_double(t);
	add_node_to_camera_animation(ref, &p, &d, &u, time);
	return SCM_BOOL_T;
}


SCM_DEFINE(s_add_view_to_ca, "add-current-view-camera-animation", 2, 0, 0, (SCM ca, SCM t), "") {
	camera_animation_ref ref = { scm_to_int(ca) };
	float time = scm_to_double(t);
	add_current_view_to_camera_animation(ref, time);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_start_ca, "start-camera-animation", 1, 0, 0, (SCM ca), "") {
	camera_animation_ref ref = { scm_to_int(ca) };
	start_camera_animation(ref);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_stop_ca, "stop-camera-animation", 1, 0, 0, (SCM ca), "") {
	camera_animation_ref ref = { scm_to_int(ca) };
	stop_camera_animation(ref);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_ca_path, "camera-animation-path", 1, 0, 0, (SCM ca), "") {
	camera_animation_ref ref = { scm_to_int(ca) };
	return scm_from_int(camera_animation_path(ref).id);
}

SCM_DEFINE(s_change_ca_speed, "change-camera-animation-speed!", 2, 0, 0, (SCM ca, SCM factor), "") {
	camera_animation_ref ref = { scm_to_int(ca) };
	float f = scm_to_double(factor);
	change_camera_animation_speed(ref, f);
	return SCM_BOOL_T;
}

SCM_DEFINE(s_ca_nodes, "camera-animation-config", 1, 0, 0, (SCM id), "") {
	camera_animation_ref ref = { scm_to_int(id) };
	struct camera_animation *ca = camera_animations + ref.id;
	char *tmp = 0;
	int n = asprintf(&tmp, "(path-animation-positions %d)", ca->path.id);
	SCM p = scm_c_eval_string(tmp);
	free(tmp);
	SCM d = SCM_EOL; for (int i = 0; i < ca->nodes; i++) d = scm_cons(vec3f_to_list(&ca->node[i].dir), d);
	SCM u = SCM_EOL; for (int i = 0; i < ca->nodes; i++) u = scm_cons(vec3f_to_list(&ca->node[i].up), u);
	SCM t = SCM_EOL; for (int i = 0; i < ca->nodes; i++) t = scm_cons(scm_from_double(ca->node[i].time), t);
	return scm_list_n(scm_from_locale_symbol("camera-animation"), scm_from_locale_symbol("pos"), p,
	                                                              scm_from_locale_symbol("dir"), d,
	                                                              scm_from_locale_symbol("up"), u,
	                                                              scm_from_locale_symbol("time"), t,
																  SCM_UNDEFINED);
}

SCM_DEFINE(s_ca_print, "print-cam-animation-script", 1, 0, 0, (SCM id), "") {
	camera_animation_ref ref = { scm_to_int(id) };
	printf("%s\n", camera_animation_script(ref));
	return SCM_BOOL_T;
}

void register_scheme_functions_for_camera_animation() {
#include "camera-animation.x"
}

#endif

