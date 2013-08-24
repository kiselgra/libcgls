#include "drawelement.h"
#include "objloader.h"
#include "scene.h"
#include "basename.h"
#include "stock-shader.h"
#include "picking.h"
#include "light.h"
#include "interaction.h"
#include "sky.h"
#include "console.h"
#include "camera-animation.h"

#include "cmdline.h"
#include "cgls.h"

#include <libcgl/libcgl.h>
#include <libcgl/wall-time.h>

#include <GL/freeglut.h>
#include <string.h>

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <unistd.h>

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
console_ref console;

void display() {
	static struct timeval tv;
	double curr_time;
	curr_time = animation_time_stamp();

	for (struct skeletal_animation_list *animations = list_skeletal_animations(); animations; animations = animations->next) {
		change_skeletal_animation_speed(animations->ref, 0.5);
		evaluate_skeletal_animation_at(animations->ref, curr_time);
	}

	for (struct path_animation_list *animations = list_path_animations(); animations; animations = animations->next) {
		evaluate_path_animation_at(animations->ref, curr_time);
	}

	for (struct camera_animation_list *animations = list_camera_animations(); animations; animations = animations->next) {
		evaluate_camera_animation_at(animations->ref, curr_time);
	}

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

// 	static int rounds = 0;
// 	if (curr_pos == 0) {
// 		rounds++;
// 		if (rounds == 5) {
// 			void keyboard(unsigned char key, int x, int y);
// 			keyboard('p', 0, 0);
// 			scm_c_eval_string("(format #t \"des: ~a~%\" (length (list-drawelements)))");
// 		}
// 	}

    check_for_gl_errors("display");
	render_console(console);

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

	register_cgls_scheme_functions();
	void register_scheme_functions_for_cmdline();
	register_scheme_functions_for_cmdline();

    gbuffer = make_stock_deferred_buffer("gbuffer", cmdline.res.x, cmdline.res.y, GL_RGBA8, GL_RGBA8, GL_RGBA16F, GL_RGBA32F, GL_DEPTH_COMPONENT24);

    char *config = 0;
    int n = asprintf(&config, "%s/%s", cmdline.include_path, cmdline.config);
	load_configfile(config);
    free(config);
	scene_ref scene = { 0 };
	the_scene = scene;
	
	struct drawelement_array picking_des = make_drawelement_array();
	push_drawelement_list_to_array(scene_drawelements(the_scene), &picking_des);
	picking = make_picking_buffer("pick", &picking_des, cmdline.res.x, cmdline.res.y);
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
		vec3f up = { 0, 1, 0 };
		light_ref hemi = make_hemispherical_light("hemi", gbuffer, &up);
		change_light_color3f(hemi, .9, .9, .9);
		add_light_to_scene(the_scene, hemi);
	}

	vec3f p = { 311.678131,204.546875,-91.080360 };
	vec3f d = { 0.443330,-0.523770,-0.727411 };
	vec3f u = { 0.172540,0.846205,-0.504150};
	light_ref spot = make_spotlight("spot", gbuffer, &p, &d, &u, 10);
	change_light_color3f(spot, 1, .5, .5);
	add_light_to_scene(the_scene, spot);
	push_drawelement_to_array(light_representation(spot), &picking_des);

	{
	vec3f pos = { 0,10,0 },
		  dir = { 1,0,0 },
		  up = { 0,1,0 };
	camera_ref c = make_perspective_cam("testcam", &pos, &dir, &up, 20, 1, 1, 1000);
	light_ref camspot = make_spotlight_from_camera("camspot", gbuffer, c);
	add_light_to_scene(the_scene, camspot);
	push_drawelement_to_array(light_representation(camspot), &picking_des);
	}

	scene_set_lighting(the_scene, apply_deferred_lights);

// 	console = make_console("bla", cmdline.res.x, cmdline.res.y, 1);
	console = make_vi_console("bla", cmdline.res.x, cmdline.res.y);

	drawelement_ref sky = make_skybox_with_spherical_mapping("sky", "cgskies-0319-free.jpg");
	set_scene_skybox(the_scene, sky);

// 	path_animation_ref pa = make_path_animation("blub", 1);
// 	drawelement_ref flag = find_drawelement("/home/kiselgra/render-data/models/sponza.obj/sponza_04");
// 	make_drawelement_part_of_path_animation(flag, pa);
// 	shader_ref newshader = make_stock_shader(0, flag, 0, true);
// 	drawelement_change_shader(flag, newshader);

// 	path_animation_ref pa = make_path_animation("blub", 1);
// 	drawelement_ref girl = find_drawelement("/home/kiselgra/models/simple-girl/simple_girl2.6.dae/");
// 	make_drawelement_part_of_path_animation(girl, pa);
// 	shader_ref newshader = make_stock_shader(0, girl, 0, true);
// 	drawelement_change_shader(girl, newshader);
	
	/*
	path_animation_ref pa = make_path_animation("blub", 5);
	drawelement_ref de = find_drawelement("/home/kiselgra/render-data/models/sponza.obj/sponza_375");
	make_drawelement_part_of_path_animation(de, pa);
	shader_ref newshader = make_stock_shader(0, de, 0, true);
	drawelement_change_shader(de, newshader);
	vec3f verts[] = { { 0, 0, 0 },
	                  { -1200, 0, 0 },
	                  { -1500, 0, 400 },
	                  { -400, 0, 700 },
	                  { 0, 0, 0 } };
	float times[] = { 0, 10, 20, 30, 40 };
	vec3f up = { 0, 1, 0 };
	add_node_to_path_animation(pa, verts+0, &up, times[0]);
	add_node_to_path_animation(pa, verts+1, &up, times[1]);
	add_node_to_path_animation(pa, verts+2, &up, times[2]);
	add_node_to_path_animation(pa, verts+3, &up, times[3]);
	add_node_to_path_animation(pa, verts+4, &up, times[4]);
	start_path_animation(pa);
	*/
	
	/*
	--pos 1070.518311,629.678711,7.745225 --dir -0.995506,0.026509,-0.090880 --up 0.023953,0.999287,0.029105
	--pos 1059.410889,166.310974,-5.750772 --dir -0.995509,0.026509,-0.090880 --up 0.023953,0.999289,0.029105
	--pos 1059.410889,166.310974,-5.750772 --dir 0.038553,0.001686,-0.999254 --up 0.000455,0.999998,0.001705
	--pos 1115.503174,161.229156,-462.977631 --dir -0.999004,0.041247,0.016819 --up 0.040625,0.998532,-0.035794
	--pos 667.909607,150.321976,-356.487122 --dir -0.677259,0.042316,-0.734526 --up 0.071865,0.997375,-0.008804
	--pos 315.462280,184.234497,-586.308472 --dir -0.019237,-0.067700,0.997520 --up -0.004957,0.997699,0.067617
	*/
	/*
	camera_animation_ref ca = make_camera_animation("test", current_camera());
	vec3f pos, dir, up;
	make_vec3f(&pos, 1070.518311,629.678711,7.745225); make_vec3f(&dir, -0.995506,0.026509,-0.090880); make_vec3f(&up, 0.023953,0.999287,0.029105);
	make_lookat_matrixf(lookat_matrix_of_cam(current_camera()), &pos, &dir, &up); recompute_gl_matrices_of_cam(current_camera());
	add_current_view_to_camera_animation(ca,00);
	make_vec3f(&pos, 1059.410889,166.310974,-5.750772); make_vec3f(&dir, -0.995509,0.026509,-0.090880); make_vec3f(&up, 0.023953,0.999289,0.029105);
	make_lookat_matrixf(lookat_matrix_of_cam(current_camera()), &pos, &dir, &up); recompute_gl_matrices_of_cam(current_camera());
	add_current_view_to_camera_animation(ca,10);
	make_vec3f(&pos, 1059.410889,166.310974,-5.750772); make_vec3f(&dir, 0.038553,0.001686,-0.999254); make_vec3f(&up, 0.000455,0.999998,0.001705);
	make_lookat_matrixf(lookat_matrix_of_cam(current_camera()), &pos, &dir, &up); recompute_gl_matrices_of_cam(current_camera());
	add_current_view_to_camera_animation(ca,20);
	make_vec3f(&pos, 1115.503174,161.229156,-462.977631); make_vec3f(&dir, -0.999004,0.041247,0.016819); make_vec3f(&up, 0.040625,0.998532,-0.035794);
	make_lookat_matrixf(lookat_matrix_of_cam(current_camera()), &pos, &dir, &up); recompute_gl_matrices_of_cam(current_camera());
	add_current_view_to_camera_animation(ca,30);
	make_vec3f(&pos, 667.909607,150.321976,-356.487122); make_vec3f(&dir, -0.677259,0.042316,-0.734526); make_vec3f(&up, 0.071865,0.997375,-0.008804);
	make_lookat_matrixf(lookat_matrix_of_cam(current_camera()), &pos, &dir, &up); recompute_gl_matrices_of_cam(current_camera());
	add_current_view_to_camera_animation(ca,40);
	make_vec3f(&pos, 315.462280,184.234497,-586.308472); make_vec3f(&dir, -0.019237,-0.067700,0.997520); make_vec3f(&up, -0.004957,0.997699,0.067617);
	make_lookat_matrixf(lookat_matrix_of_cam(current_camera()), &pos, &dir, &up); recompute_gl_matrices_of_cam(current_camera());
	add_current_view_to_camera_animation(ca,50);
	camera_animation_speed(ca, 1.2);
	start_camera_animation(ca);
	*/

	add_cam_anim_commands_to_viconsole(console);
	finalize_single_material_passes_for_array(&picking_des);
	
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


