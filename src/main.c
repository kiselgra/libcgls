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
#include <libcgl/debug.h>

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
bool gb_debug = false;

picking_buffer_ref picking;
drawelement_ref selected_de = { -1 };
console_ref console;

void display() {
	static struct timeval tv;
	double curr_time;
	curr_time = animation_time_stamp();

	for (struct skeletal_animation_list *animations = list_skeletal_animations(); animations; animations = animations->next)
		evaluate_skeletal_animation_at(animations->ref, curr_time);
	for (struct path_animation_list *animations = list_path_animations(); animations; animations = animations->next)
		evaluate_path_animation_at(animations->ref, curr_time);
	for (struct camera_animation_list *animations = list_camera_animations(); animations; animations = animations->next)
		evaluate_camera_animation_at(animations->ref, curr_time);

	glEnable(GL_DEPTH_TEST);
	
    glFinish();
	wall_time_t start = wall_time_in_ms();

	if (cgls_deferred)
    	bind_framebuffer(gbuffer);
	glClearColor(0,0,0,1);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	if (cgls_deferred)
    	unbind_framebuffer(gbuffer);

	if (cgls_deferred) {
		render_scene_to_gbuffer(the_scene, gbuffer);
		if (!gb_debug)
			render_scene_from_gbuffer(the_scene, gbuffer);
		else
			render_gbuffer_visualization(the_scene, gbuffer);
	}
	else
		render_scene(the_scene);

	if (valid_drawelement_ref(selected_de))
		highlight_object(picking, selected_de);

	glFinish();
	wall_time_t end = wall_time_in_ms();

	times[curr_pos] = end-start;
	curr_pos = (curr_pos+1) % samples;
	valid_pos = (valid_pos == samples ? samples : valid_pos+1);

    check_for_gl_errors("display");
	render_console(console);

	swap_buffers();
}

void idle() {
	glutPostRedisplay(); 
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

void toggle_gb(interaction_mode *m, int x, int y) {
	gb_debug = !gb_debug;
}

interaction_mode* make_viewer_mode() {
	interaction_mode *m = make_interaction_mode("viewer");
	add_function_key_to_mode(m, 'p', cgls_interaction_no_button, show_fps);
	add_function_key_to_mode(m, ' ', cgls_interaction_no_button, advance_anim);
	add_function_key_to_mode(m, '~', cgls_interaction_no_button, toggle_gb);
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

	initialize_interaction();
	push_interaction_mode(make_default_cgls_interaction_mode());
	push_interaction_mode(make_viewer_mode());

#ifdef WITH_GUILE
	register_cgls_scheme_functions();
	void register_scheme_functions_for_cmdline();
	register_scheme_functions_for_cmdline();
#endif

	if (cgls_deferred)
		gbuffer = make_stock_deferred_buffer("gbuffer", cmdline.res.x, cmdline.res.y, GL_RGBA8, GL_RGBA8, GL_RGBA16F, GL_RGBA32F, GL_DEPTH_COMPONENT24);

#ifdef WITH_GUILE
    char *config = 0;
    int n = asprintf(&config, "%s/%s", cmdline.include_path, cmdline.config);
	load_configfile(config);
    free(config);
	scene_ref scene = { 0 };
	the_scene = scene;
#else
#warning example applicaiton cannot run without guile support.
#endif
	
	struct drawelement_array picking_des = make_drawelement_array();
	push_drawelement_list_to_array(scene_drawelements(the_scene), &picking_des);
	picking = make_picking_buffer("pick", &picking_des, cmdline.res.x, cmdline.res.y);
	push_interaction_mode(make_blender_style_interaction_mode(the_scene, picking));
	
// 	light_ref hms = make_headmounted_spotlight("helmet", gbuffer, 30);
// 	change_light_color3f(hms, .6, .6, .6);
// 	add_light_to_scene(the_scene, hms);
	
	{
		vec3f up = { 0, 1, 0 };
		light_ref hemi = make_hemispherical_light("hemi", gbuffer, &up);
		change_light_color3f(hemi, .9, .9, .9);
		add_light_to_scene(the_scene, hemi);
	}

	{
		vec3f p = { 311.678131,204.546875,-91.080360 };
		vec3f d = { 0.443330,-0.523770,-0.727411 };
		vec3f u = { 0.172540,0.846205,-0.504150};
		light_ref spot = make_spotlight("spot", gbuffer, &p, &d, &u, 10);
		change_light_color3f(spot, 1, 1, 1);
		add_light_to_scene(the_scene, spot);
		push_drawelement_to_array(light_representation(spot), &picking_des);
	}
	
	{
		vec3f p = { 311.678131,204.546875,-91.080360 };
		vec3f d = { 0.443330,-0.523770,-0.727411 };
		vec3f u = { 0.172540,0.846205,-0.504150};
		light_ref rect = make_rectangular_light("rect", gbuffer, &p, &d, &u, 140, 70);
		change_light_color3f(rect, 1, .5, .5);
		add_light_to_scene(the_scene, rect);
		push_drawelement_to_array(light_representation(rect), &picking_des);
	}

// 	{
// 		vec3f pos = { 0,10,0 },
// 			  dir = { 1,0,0 },
// 			  up = { 0,1,0 };
// 		camera_ref c = make_perspective_cam("testcam", &pos, &dir, &up, 20, 1, 1, 1000);
// 		light_ref camspot = make_spotlight_from_camera("camspot", gbuffer, c);
// 		add_light_to_scene(the_scene, camspot);
// 		push_drawelement_to_array(light_representation(camspot), &picking_des);
// 	}

	if (cgls_deferred)
		scene_set_lighting(the_scene, apply_deferred_lights);
	else {
		scene_set_light_setup(the_scene, stock_forward_shading_light_setup);
		scene_set_light_cleanup(the_scene, stock_forward_shading_light_cleanup);
	}

	console = make_vi_console("bla", cmdline.res.x, cmdline.res.y);

	drawelement_ref sky = make_skybox_with_spherical_mapping("sky", "cgskies-0319-free.jpg");
	set_scene_skybox(the_scene, sky);

	add_cam_anim_commands_to_viconsole(console);
	finalize_single_material_passes_for_array(&picking_des);
	
	for (struct skeletal_animation_list *animations = list_skeletal_animations(); animations; animations = animations->next)
		change_skeletal_animation_speed(animations->ref, 0.5);
	
	enter_glut_main_loop();
}

int main(int argc, char **argv)
{	
	cgls_deferred = true;
	parse_cmdline(argc, argv);
	
// 	int guile_mode = guile_cfg_only;
	int guile_mode = with_guile;
	startup_cgl("name", 4, 2, argc, argv, (int)cmdline.res.x, (int)cmdline.res.y, actual_main, guile_mode, false, 0);

	return 0;
}


