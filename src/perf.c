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

#include "cgls.h"
#include "perf.h"

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
#define samples 1024*1024
float times[samples];
int valid_pos = 0, curr_pos = 0;

framebuffer_ref gbuffer;
picking_buffer_ref picking;
drawelement_ref selected_de = { -1 };
bool gb_debug = false;

console_ref console;

int screen_res_x, screen_res_y;

camera_animation_ref cam_anim = { -1 };
float record_t = 0;
bool playback = false;

char *user_config = 0;
char *model_file, *config_file, *cam_file;
define_slist(stringlist, char *entry);
struct stringlist *config_paths = 0,
				  *model_paths = 0;
int number_of_samples = 2048;
double animation_duration = 0;
double animation_step = 1;

char* find_file_in(const char *basename, struct stringlist *paths)
{
	if (basename[0] == '/') {
		if (file_exists(basename))
			return strdup(basename);
	}
	else
		for (struct stringlist *run = paths; run; run = run->next) {
			int len = strlen(run->entry);
			bool require_slash = run->entry[len-1] != '/';
			char *str;
			if (require_slash)
				asprintf(&str, "%s/%s", run->entry, basename);
			else
				asprintf(&str, "%s%s", run->entry, basename);
			if (file_exists(str)) {
				printf("jep, %s is there.\n", str);
				return str;
			}
			else
				printf("nope, %s not found.\n", str);
			free(str);
		}
	return 0;
}

void display() {
	static struct timeval tv;
	static double curr_time = 0;
	static int sample = 0;

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

	glEnable(GL_DEPTH_TEST);
	
	if (update) update();

    glFinish();
	wall_time_t start = wall_time_in_ms();

	if (render)
		render();
	else {
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
	}

	glFinish();
	wall_time_t end = wall_time_in_ms();

	times[curr_pos] = end-start;
	curr_pos = (curr_pos+1);
	if (samples ==  curr_pos) {
		fprintf(stderr, "!  !  !\n   NOT ENO TIME SAMPLE\n!  !  !\n\n");
		exit(1);
	}
	valid_pos = (valid_pos == samples ? samples : valid_pos+1);

    check_for_gl_errors("display");
	render_console(console);

	swap_buffers();
	
	curr_time = curr_time + animation_step;
	if (++sample == number_of_samples) {
		fprintf(stderr, "took all samples. quitting.\n");
		quit(0);
	}
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

void take_camera_sample(interaction_mode *m, int x, int y) {
	add_current_view_to_camera_animation(cam_anim, record_t);
	char *str = 0;
	asprintf(&str, "took sample %i.", (int)record_t);
	show_message_on_console(console, str);
	free(str);
	record_t += 1;
}

interaction_mode* make_viewer_mode() {
	interaction_mode *m = make_interaction_mode("viewer");
	add_function_key_to_mode(m, 'p', cgls_interaction_no_button, show_fps);
	add_function_key_to_mode(m, ' ', cgls_interaction_no_button, advance_anim);
	add_function_key_to_mode(m, 'c', cgls_interaction_no_button, take_camera_sample);
	add_function_key_to_mode(m, '~', cgls_interaction_no_button, toggle_gb);
	return m;
}

static char* start_recording(console_ref c, int argc, char **argv) {
	char *name = "default";
	if (argc == 2)
		name = argv[1];
	char *str = 0;
    int n = asprintf(&str, "Setting up camera animation %s. Use 'c' to record samples, :save to save the animation.", name);
	cam_anim = make_camera_animation(name, current_camera());
	return str;
}

static char* save_animation(console_ref c, int argc, char **argv) {
	char *name = 0;
	if (argc == 2)
		name = argv[1];
	else return strdup("no filename given");
	
	FILE *f = fopen(argv[1], "w+");
	fprintf(f, "%s\n\n", camera_animation_script(cam_anim));
	normalize_camera_speed_along_path(cam_anim);
	fprintf(f, "%s\n\n", camera_animation_script(cam_anim));
	return 0;
}

void setup_console() {
	console = make_vi_console("perf", screen_res_x, screen_res_y);
	add_vi_console_command(console, "start", start_recording);
	add_vi_console_command(console, "save", save_animation);
}

void actual_main() 
{
	void register_scheme_functions_for_perf();
	register_scheme_functions_for_perf();

	if (init_pre)
		init_pre();

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
#endif

    gbuffer = make_stock_deferred_buffer("gbuffer", screen_res_x, screen_res_y, GL_RGBA8, GL_RGBA8, GL_RGBA16F, GL_RGBA32F, GL_DEPTH_COMPONENT24);

#ifdef WITH_GUILE
	printf("loading perf file (%s)...\n", user_config);
	load_configfile(user_config);
	printf("loading conf file (%s)...\n", config_file);
	load_configfile(find_file_in(config_file, config_paths));
	printf("loading cam file (%s)...\n", cam_file);
	load_configfile(find_file_in(cam_file, config_paths));

	scene_ref scene = { 0 };
	the_scene = scene;
#else
#warning profiler cannot run without guile support.
#endif
	
	struct drawelement_array picking_des = make_drawelement_array();
	push_drawelement_list_to_array(scene_drawelements(the_scene), &picking_des);
	picking = make_picking_buffer("pick", &picking_des, screen_res_x, screen_res_y);
	push_interaction_mode(make_blender_style_interaction_mode(the_scene, picking));
	
	{
		vec3f up = { 0, 1, 0 };
		light_ref hemi = make_hemispherical_light("hemi", gbuffer, &up);
		change_light_color3f(hemi, .9, .9, .9);
		add_light_to_scene(the_scene, hemi);
	}

	
	scene_set_lighting(the_scene, apply_deferred_lights);

	setup_console();

	drawelement_ref sky = make_skybox_with_spherical_mapping("sky", "cgskies-0319-free.jpg");
	set_scene_skybox(the_scene, sky);

	add_cam_anim_commands_to_viconsole(console);
	finalize_single_material_passes_for_array(&picking_des);

	// the last camera animation is supposed to be the one generated by the camera-anim file (as it is loaded last).
	struct camera_animation_list *cas = list_camera_animations();
	if (cas == 0) {
		fprintf(stderr, "NO CAM ANIM SPECIFIED. Quitting.\n");
		exit(1);
	}
	camera_animation_ref ca_ref = cas->ref;
	for (struct camera_animation_list *run = cas; run; run = run->next)
		ca_ref = run->ref;
	start_camera_animation_with_timestamp(ca_ref, 0);
	animation_duration = camera_animation_time(cas->ref, camera_animation_nodes(cas->ref)-1) * 1000;
	animation_step = animation_duration / number_of_samples;

	if (init_post)
		init_post();
	
	enter_glut_main_loop();
}


void enter(const char *name, int glmaj, int glmin, int res_x, int res_y, const char *configfile) {
// 	int guile_mode = guile_cfg_only;
	int guile_mode = with_guile;
	user_config = strdup(configfile);
	char *fake[] = { 0 };
	screen_res_x = res_x;
	screen_res_y = res_y;
	startup_cgl(name, glmaj, glmin, 0, fake, res_x, res_y, actual_main, guile_mode, false, 0);
}

bool make_gbuffer = true;

void (*init_pre)(void) = 0;
void (*init_post)(void) = 0;
void (*update)(void) = 0;
void (*render)(void) = 0;


#ifdef WITH_GUILE	
#include <libguile.h>
#include <libcgl/scheme.h>

float collapse_factor = 20;

SCM_DEFINE(s_cmdline, "query-cmdline", 1, 0, 0, (SCM what), "") {
	if (!scm_is_symbol(what))
		scm_throw(scm_from_locale_symbol("cmdline-error"), scm_list_2(what, scm_from_locale_string("is not a symbol")));
	char *w = scm_to_locale_string(scm_symbol_to_string(what));
	printf("what: '%s'\n", w);
	if (strcmp(w, "model") == 0) {
		free(w);
		return scm_from_locale_string(find_file_in(model_file, model_paths));
	}
	else if (strcmp(w, "merge-factor") == 0) {
		free(w);
		return scm_from_double(collapse_factor);
	}

	scm_throw(scm_from_locale_symbol("cmdline-error"), 
	          scm_list_2(what, 
	                    scm_from_locale_string("invalid option. use hemi, hemi-dir, model, filetype")));
	return SCM_BOOL_F; // for -Wreturn-type.
}

SCM_DEFINE(s_conf_file, "config-file", 1, 0, 0, (SCM name), "") {
	config_file = scm_to_locale_string(name);
	return SCM_BOOL_F;
}

SCM_DEFINE(s_cam_file, "camera-file", 1, 0, 0, (SCM name), "") {
	cam_file = scm_to_locale_string(name);
	return SCM_BOOL_F;
}

SCM_DEFINE(s_model_file, "model", 1, 0, 0, (SCM name), "") {
	model_file = scm_to_locale_string(name);
	return SCM_BOOL_F;
}

SCM_DEFINE(s_samples, "samples", 1, 0, 0, (SCM n), "") {
	number_of_samples = scm_to_int(n);
	return SCM_BOOL_F;
}

SCM_DEFINE(s_model_paths, "prepend-model-path", 1, 0, 0, (SCM name), "") {
	struct stringlist *head = malloc(sizeof(struct stringlist));
	head->next = model_paths;
	head->entry = scm_to_locale_string(name);
	model_paths = head;
	return SCM_BOOL_F;
}

SCM_DEFINE(s_conf_paths, "prepend-conf-path", 1, 0, 0, (SCM name), "") {
	struct stringlist *head = malloc(sizeof(struct stringlist));
	head->next = config_paths;
	head->entry = scm_to_locale_string(name);
	config_paths = head;
	return SCM_BOOL_F;
}

SCM_DEFINE(s_img_paths, "prepend-image-path", 1, 0, 0, (SCM name), "") {
	char *n = scm_to_locale_string(name);
	prepend_image_path(n);
	free(n);
	return SCM_BOOL_T;
}

void register_scheme_functions_for_perf() {
#include "perf.x"
	scm_c_eval_string("(define (model-paths . args) (for-each prepend-model-path (reverse args)))");
	scm_c_eval_string("(define (config-paths . args) (for-each prepend-conf-path (reverse args)))");
	scm_c_eval_string("(define (image-paths . args) (for-each prepend-image-path (reverse args)))");
}

#endif


