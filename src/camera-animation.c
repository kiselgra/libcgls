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

static void find_nodes_for_cam_animation(struct camera_animation *ca, float time, int *next, int *prev) {
	*next = -1;
	for (int i = 0; i < ca->nodes; ++i)
		if (ca->node[i].time > time) {
			*next = i;
			break;
		}
	if (*next > 0) *prev = *next-1;
}


void evaluate_camera_animation_at(camera_animation_ref ref, animation_time_t time) {
	struct camera_animation *ca = camera_animations + ref.id;
	if (!ca->running)
		return;
	matrix4x4f *mat = path_matrix_of_animation(ca->path);
	
	// update path
	evaluate_path_animation_at(ca->path, time);
	vec3f pos;
	extract_pos_vec3f_of_matrix(&pos, mat);
	
	time -= ca->animation_start_time;
	time /= 1000;
	time *= ca->animation_speed;
	
	if (time > ca->node[ca->nodes-1].time) {
		time -= ca->node[ca->nodes-1].time;
		ca->animation_start_time += ca->node[ca->nodes-1].time * 1000 / ca->animation_speed;
	}
	

	printf("pos = %6.6f %6.6f %6.6f\n", pos.x, pos.y, pos.z);

	int p = 0, n = -1;
	find_nodes_for_cam_animation(ca, time, &n, &p);
// 	printf("ECA %f [%d %d] [%6.6f %6.6f]\n", time, p, n, ca->node[p].time, ca->node[n].time);

	if (p == n)
		make_lookat_matrixf(lookat_matrix_of_cam(ca->cam), &pos, &ca->node[n].dir, &ca->node[n].up);
	else {
		float t = (time - ca->node[p].time) / (ca->node[n].time - ca->node[p].time);
	
// 	printf("ECA %f [%d %d] [%6.6f %6.6f] %f\n", time, p, n, ca->node[p].time, ca->node[n].time, t);

		quaternionf dir_from, dir_to, dir;
		make_quaternionf(&dir_from, &ca->node[p].dir, 0);
		make_quaternionf(&dir_to, &ca->node[n].dir, 0);
		slerp_quaterionf(&dir, &dir_from, &dir_to, t);

		quaternionf up_from, up_to, up;
		make_quaternionf(&up_from, &ca->node[p].up, 0);
		make_quaternionf(&up_to, &ca->node[n].up, 0);
		slerp_quaterionf(&up, &up_from, &up_to, t);

		make_lookat_matrixf(lookat_matrix_of_cam(ca->cam), &pos, &dir.v, &up.v);
	}
	recompute_gl_matrices_of_cam(ca->cam);
}

