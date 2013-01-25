#include "drawelement.h"
#include "objloader.h"
#include "scene.h"
#include "basename.h"
#include "stock-shader.h"
#include "picking.h"

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

framebuffer_ref gbuffer;
drawelement_ref deferred_spot, deferred_hemi;
picking_buffer_ref picking;

bool hemi_uniform_handler(drawelement_ref *dummy, const char *uniform, int location) {
	if (strcmp(uniform, "hemi_dir") == 0) {
		matrix4x4f *view = gl_view_matrix_of_cam(current_camera());
		vec4f in = { 0, 1, 0, 0 }, out;
		multiply_matrix4x4f_vec4f(&out, view, &in);
		glUniform3fv(location, 1, (float*)&out);
// 		printf("hemi_dir is %3.6f %3.6f %3.6f\n", out.x, out.y, out.z);
	}
	else if (strcmp(uniform, "hemi_col") == 0) {
		glUniform3f(location, .7, .6, .9);
	}
	else
		return false;
	return true;
}

void display() {
// 	scene_set_traverser(the_scene, graph_scene_bulk_traverser);
// 	glDisable(GL_DEBUG_OUTPUT);

	glEnable(GL_DEPTH_TEST);
	
    glFinish();
	wall_time_t start = wall_time_in_ms();

    bind_framebuffer(gbuffer);

	glClearColor(0,0,0.25,1);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	render_scene(the_scene);

    unbind_framebuffer(gbuffer);

	glClearColor(0,0,0,0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glEnable(GL_BLEND);
	glBlendFunc(GL_ONE, GL_ONE);
	glDisable(GL_DEPTH_TEST);
    render_drawelement(deferred_spot);
    render_drawelement(deferred_hemi);
	glDisable(GL_BLEND);
	glEnable(GL_DEPTH_TEST);


	glFinish();
	wall_time_t end = wall_time_in_ms();

	update_picking_buffer(picking);

	times[curr_pos] = end-start;
	curr_pos = (curr_pos+1) % samples;
	valid_pos = (valid_pos == samples ? samples : valid_pos+1);

    check_for_gl_errors("blarg");

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

    gbuffer = make_stock_deferred_buffer("gbuffer", cmdline.res.x, cmdline.res.y, GL_RGBA8, GL_RGBA8, GL_RGBA16F, GL_RGBA32F, GL_DEPTH_COMPONENT24);
    deferred_spot = make_stock_gbuffer_default_drawelement(gbuffer, "gbuffer spot", stock_effect_headmounted_spot());
    deferred_hemi = make_stock_gbuffer_default_drawelement(gbuffer, "gbuffer hemi", stock_effect_hemisphere_lighting());
	add_shader_uniform(drawelement_shader(deferred_hemi), "hemi_dir");
	add_shader_uniform(drawelement_shader(deferred_hemi), "hemi_col");
	prepend_drawelement_uniform_handler(deferred_hemi, (uniform_setter_t)hemi_uniform_handler);

	picking = make_picking_buffer("pick", cmdline.res.x, cmdline.res.y);

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


