#include "drawelement.h"
#include "objloader.h"
#include "scene.h"
#include "basename.h"
#include "stock-shader.h"
#include "picking.h"
#include "light.h"
#include "interaction.h"

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
// drawelement_ref deferred_spot, deferred_hemi;//, deferred_copydepth;
picking_buffer_ref picking;
drawelement_ref selected_de = { -1 };

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
	glClearColor(0,0,0,1);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    unbind_framebuffer(gbuffer);
	/*
    bind_framebuffer(gbuffer);

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	render_scene(the_scene);

    unbind_framebuffer(gbuffer);

	glClearColor(0,0,0,0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glEnable(GL_BLEND);
	glBlendFunc(GL_ONE, GL_ONE);
	render_drawelement(deferred_copydepth);
	glDisable(GL_DEPTH_TEST);
    render_drawelement(deferred_spot);
    render_drawelement(deferred_hemi);
	glDisable(GL_BLEND);
	glEnable(GL_DEPTH_TEST);
	*/
	render_scene_deferred(the_scene, gbuffer);

	if (valid_drawelement_ref(selected_de))
		highlight_object(picking, selected_de);

	glFinish();
	wall_time_t end = wall_time_in_ms();

// 	update_picking_buffer(picking, the_scene, -1, -1);

	times[curr_pos] = end-start;
	curr_pos = (curr_pos+1) % samples;
	valid_pos = (valid_pos == samples ? samples : valid_pos+1);

    check_for_gl_errors("display");

	swap_buffers();
}

void idle() {
	glutPostRedisplay(); 
}

enum ui_state_t { std, grab };
enum axis { X, Y, Z, none };
int ui_state = std;
int axis = none;
void grab_mouse_motion(int, int);
int last_mouse_grab_x, last_mouse_grab_y;

void keyboard(unsigned char key, int x, int y) {
	if (key == 'p') {
		double sum = 0;
		for (int i = 0; i < valid_pos; ++i)
			sum += times[i];
		float avg = sum / (double)valid_pos;
		printf("average render time: %.3f ms, %.1f fps \t(sum %f, n %d)\n", avg, 1000.0f/avg, (float)sum, valid_pos);
	}
	if (key == 'o') {
		if (valid_drawelement_ref(selected_de)) {
			light_ref light = find_light_by_representation(selected_de);
			if (valid_light_ref(light))
				if (light_is_on(light))
					light_off(light);
				else
					light_on(light);
		}
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

// TODO this whole thing should go to some standard libcgls place...
#ifdef WITH_GUILE
void register_scheme_functions_for_cgls_objloader();
void register_scheme_functions_for_material();
void register_scheme_functions_for_cmdline();
void register_scheme_functions_for_drawelement();
void register_scheme_functions_for_scene();
void register_scheme_functions_for_cgls_modelloader();
static void register_scheme_functions() {
	register_scheme_functions_for_cgls_objloader();
	register_scheme_functions_for_material();
	register_scheme_functions_for_cmdline();
	register_scheme_functions_for_drawelement();
	register_scheme_functions_for_scene();
	register_scheme_functions_for_cgls_modelloader();
}
#endif

void show_fps(interaction_mode *m, int x, int y) {
	double sum = 0;
	for (int i = 0; i < valid_pos; ++i)
		sum += times[i];
	float avg = sum / (double)valid_pos;
	printf("average render time: %.3f ms, %.1f fps \t(sum %f, n %d)\n", avg, 1000.0f/avg, (float)sum, valid_pos);
}

void advance_anim(interaction_mode *m, int x, int y) {
	skeletal_animation_ref ar = { 0 };
	static float time = 0;
	time += 0.01;

	evaluate_skeletal_animation_at(ar, time);
}

interaction_mode* make_viewer_mode() {
	interaction_mode *m = make_interaction_mode("viewer");
	add_function_key_to_mode(m, 'p', cgls_interaction_no_button, show_fps);
	add_function_key_to_mode(m, ' ', cgls_interaction_no_button, advance_anim);
	return m;
}

void actual_main() 
{
	dump_gl_info();
	for (int i = 0; i < samples; ++i)
		times[i] = 0.0f;

	stop_debug_output();

	register_display_function(display);
	register_idle_function(idle);
	register_keyboard_function(keyboard);
// 	register_mouse_function(mouse_func);

	initialize_interaction();
	push_interaction_mode(make_default_cgls_interaction_mode());
	push_interaction_mode(make_viewer_mode());

	register_scheme_functions();

    gbuffer = make_stock_deferred_buffer("gbuffer", cmdline.res.x, cmdline.res.y, GL_RGBA8, GL_RGBA8, GL_RGBA16F, GL_RGBA32F, GL_DEPTH_COMPONENT24);

	picking = make_picking_buffer("pick", cmdline.res.x, cmdline.res.y);

    char *config = 0;
    int n = asprintf(&config, "%s/%s", cmdline.include_path, cmdline.config);
	load_configfile(config);
    free(config);
	scene_ref scene = { 0 };
	the_scene = scene;
	
	push_interaction_mode(make_blender_style_interaction_mode(the_scene, picking));
	
	{
// 		mesh_ref mesh = make_cylinder("cyl", 10, 0);
// 		vec4f white = { 1,1,1,1 };
// 		material_ref mat = make_material("cyl-mat", &white, &white, &white);
// 		material_use_stock_shader(mat);
// 		drawelement_ref cyl = make_drawelement("cyl", mesh, material_shader(mat), mat);
// 		prepend_drawelement_uniform_handler(cyl, (uniform_setter_t)default_matrix_uniform_handler);
// 		prepend_drawelement_uniform_handler(cyl, (uniform_setter_t)default_material_uniform_handler);
// // 		scene_add_drawelement(the_scene, cyl);
	}

	
	light_ref hms = make_headmounted_spotlight("helmet", gbuffer, 30);
	change_light_color3f(hms, .6, .6, .6);
// 	add_light_to_scene(the_scene, hms);
	
	{
		vec3f up = { 0, 0, 1 };
		light_ref hemi = make_hemispherical_light("hemi", gbuffer, &up);
		change_light_color3f(hemi, 1.9, 1.9, 1.9);
		add_light_to_scene(the_scene, hemi);
	}
	{
		vec3f up = { 0, 0, -1 };
		light_ref hemi = make_hemispherical_light("hemi2", gbuffer, &up);
		change_light_color3f(hemi, 1.9, 1.9, 1.9);
		add_light_to_scene(the_scene, hemi);
	}

	vec3f p = { 311.678131,204.546875,-91.080360 };
	vec3f d = { 0.443330,-0.523770,-0.727411 };
	vec3f u = { 0.172540,0.846205,-0.504150};
	light_ref spot = make_spotlight("spot", gbuffer, &p, &d, &u, 10);
	change_light_color3f(spot, 1, .5, .5);
// 	add_light_to_scene(the_scene, spot);

	{
	vec3f pos = { 0,10,0 },
		  dir = { 1,0,0 },
		  up = { 0,1,0 };
	camera_ref c = make_perspective_cam("testcam", &pos, &dir, &up, 20, 1, 1, 1000);
	light_ref camspot = make_spotlight_from_camera("camspot", gbuffer, c);
// 	add_light_to_scene(the_scene, camspot);
	}

	scene_set_lighting(the_scene, apply_deferred_lights);


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


