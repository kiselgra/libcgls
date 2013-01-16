#include "drawelement.h"
#include "objloader.h"
#include "scene.h"
#include "basename.h"

#include "cmdline.h"

#include <libcgl/libcgl.h>
#include <libcgl/wall-time.h>

#include <GL/freeglut.h>
#include <string.h>

#include <stdio.h>
#include <stdlib.h>

scene_ref the_scene;
#define samples 128
float times[samples];
int valid_pos = 0, curr_pos = 0;

void display() {
	scene_set_traverser(the_scene, graph_scene_bulk_traverser);

	glClearColor(0,0,0.25,1);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glEnable(GL_DEPTH_TEST);

	glFinish();
	wall_time_t start = wall_time_in_ms();

	render_scene(the_scene);

	glFinish();
	wall_time_t end = wall_time_in_ms();

	times[curr_pos] = end-start;
	curr_pos = (curr_pos+1) % samples;
	valid_pos = (valid_pos == samples ? samples : valid_pos+1);

	swap_buffers();
}

void idle() {
	glutPostRedisplay(); 
}

void keyboard(unsigned char key, int x, int y) {
	if (key == 'c') {
		vec3f cam_pos, cam_dir, cam_up;
		matrix4x4f *lookat_matrix = lookat_matrix_of_cam(current_camera());
		extract_pos_vec3f_of_matrix(&cam_pos, lookat_matrix);
		extract_dir_vec3f_of_matrix(&cam_dir, lookat_matrix);
		extract_up_vec3f_of_matrix(&cam_up, lookat_matrix);
		printf("--pos %f,%f,%f ", cam_pos.x, cam_pos.y, cam_pos.z);
		printf("--dir %f,%f,%f ", cam_dir.x, cam_dir.y, cam_dir.z);
		printf("--up %f,%f,%f\n", cam_up.x, cam_up.y, cam_up.z);
	}
	else if (key == 'p') {
		double sum = 0;
		for (int i = 0; i < valid_pos; ++i)
			sum += times[i];
		float avg = sum / (double)valid_pos;
		printf("average render time: %.3f ms, %.1f fps \t(sum %f, n %d)\n", avg, 1000.0f/avg, (float)sum, valid_pos);
	}
	else standard_keyboard(key, x, y);
}

// try to implement this in scheme! done! :)
// then this could be part of a scene description
bool custom_light_handler(drawelement_ref ref, const char *uniform, int location) {
	if (strcmp(uniform, "light_dir") == 0)         glUniform3f(location, 0, -1, -0.2);
	else if (strcmp(uniform, "light_col") == 0)    glUniform3f(location, 1, 0.9, 0.9);
	else if (strcmp(uniform, "hemi_dir") == 0)     glUniform3f(location, cmdline.hemi_dir.x, cmdline.hemi_dir.y, cmdline.hemi_dir.z);
	else return false;
	return true;
}

void adjust_view(const vec3f *bb_min, const vec3f *bb_max, vec3f *cam_pos, float *distance) {
	vec3f bb_center, tmp;
	sub_components_vec3f(&tmp, bb_max, bb_min);
	div_vec3f_by_scalar(&tmp, &tmp, 2);
	add_components_vec3f(&bb_center, &tmp, bb_min);
	
	sub_components_vec3f(&tmp, bb_max, bb_min);
	*distance = length_of_vec3f(&tmp);
	make_vec3f(&tmp, 0, 0, *distance);
	add_components_vec3f(cam_pos, &bb_center, &tmp);

	cgl_cam_move_factor = *distance / 20.0f;
}

#ifdef WITH_GUILE
void register_scheme_functions_for_cgls_objloader();
void register_scheme_functions_for_material();
void register_scheme_functions_for_cmdline();
void register_scheme_functions_for_drawelement();
void register_scheme_functions_for_scene();
static void register_scheme_functions() {
	register_scheme_functions_for_cgls_objloader();
	register_scheme_functions_for_material();
	register_scheme_functions_for_cmdline();
	register_scheme_functions_for_drawelement();
	register_scheme_functions_for_scene();
}
#endif

void actual_main() 
{
	dump_gl_info();
	for (int i = 0; i < samples; ++i)
		times[i] = 0.0f;

	register_display_function(display);
	register_idle_function(idle);
	register_keyboard_function(keyboard);

	register_scheme_functions();
    char *config = 0;
    int n = asprintf(&config, "%s/%s", cmdline.include_path, cmdline.config);
	load_configfile(config);
    free(config);
	scene_ref scene = { 0 };
	the_scene = scene;

	enter_glut_main_loop();
}

int main(int argc, char **argv)
{	
	parse_cmdline(argc, argv);
	
// 	int guile_mode = guile_cfg_only;
	int guile_mode = with_guile;
	startup_cgl("name", 4, 2, argc, argv, (int)cmdline.res.x, (int)cmdline.res.y, actual_main, guile_mode, false, 0);

	return 0;
}


