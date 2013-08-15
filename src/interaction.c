#include "interaction.h"

#include "c-utils.h"

#include <libcgl/glut.h>
#include <libcgl/camera.h>

#include <GL/freeglut.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

float cgls_interaction_scale = 1;

interaction_mode* make_interaction_mode(const char *name) {
	interaction_mode *mode = malloc(sizeof(interaction_mode));
	mode->name = strdup(name);
	mode->key_function_table = 0;
	mode->mouse_function_table = 0;
	mode->fallback_keyhandler = 0;
	mode->fallback_mouse_handler = 0;
	mode->motion_handler = 0;
	mode->type = cgls_invalid_interaction_mode;
	mode->aux = 0;
	return mode;
}

void add_function_key_to_mode(interaction_mode *mode, unsigned char key, unsigned int modifiers, interaction_key_function_t call) {
	if (!mode->key_function_table) {
		mode->key_function_table = malloc(sizeof(interaction_key_function_t*) * cgls_interaction_modifier_combinations);
		for (int i = 0; i < cgls_interaction_modifier_combinations; ++i)	{
			mode->key_function_table[i] = malloc(sizeof(interaction_key_function_t) * 255);
			for (int j = 0; j < 255; ++j)
				mode->key_function_table[i][j] = 0;
		}
	}
	mode->key_function_table[modifiers][key] = call;
}

void add_mouse_function_to_mode(interaction_mode *mode, int buttons, int states, unsigned int modifiers, interaction_mouse_function_t call) {
	if (!mode->mouse_function_table) {
		mode->mouse_function_table = malloc(sizeof(interaction_mouse_function_t*) * cgls_interaction_modifier_combinations);
		for (int i = 0; i < cgls_interaction_modifier_combinations; ++i)	{
			mode->mouse_function_table[i] = malloc(sizeof(interaction_mouse_function_t*) * cgls_interaction_button_combinations);
			for (int j = 0; j < cgls_interaction_button_combinations; ++j)	{
				mode->mouse_function_table[i][j] = malloc(sizeof(interaction_mouse_function_t) * cgls_interaction_button_state_combinations);
				for (int k = 0; k < cgls_interaction_button_state_combinations; ++k)
					mode->mouse_function_table[i][j][k] = 0;
			}
		}
	}
	mode->mouse_function_table[modifiers][buttons][states] = call;
}

void change_fallback_keyhandler_for_mode(interaction_mode *mode, interaction_keyhandler_t handler) {
	mode->fallback_keyhandler = handler;
}

void change_fallback_mouse_handler_for_mode(interaction_mode *mode, void (*handler)(interaction_mode *, int, int, int, int)) {
	mode->fallback_mouse_handler = handler;
}

void change_motion_handler_for_mode(interaction_mode *mode, void (*handler)(interaction_mode *, int, int)) {
	mode->motion_handler = handler;
}

int cgls_interaction_last_mouse_x = 0;	//!< this state is maintained, regardless of the current mouse motion handler, use it.
int cgls_interaction_last_mouse_y = 0;	//!< this state is maintained, regardless of the current mouse motion handler, use it.



/*!	\defgroup infoline info line
 * 	prints an information line to some place. 
 * 	currently this place is the terminal. 
 * 	this may, however, change to a screen overlay.
 *
 * 	the main reason for this is, that using different input modes can be
 * 	confusing, and this way you have a good indication of what you are doing.
 *
 * 	\ingroup interaction
 */

/*! \addtogroup cglmode
 * 	@{
 */

static info_line_printer_t info_line_printer = default_info_line_printer;

void default_info_line_printer(const char *fmt, va_list ap) {
	printf("--> ");
	vfprintf(stdout, fmt, ap);
	printf("\n");
}

void info_line(const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	info_line_printer(fmt, ap);
	va_end(ap);
}

//! @}


/*! \defgroup cglmode  cgl mode
 *	a simple mode implementing standard cgl handling.
 *	\ingroup interaction
 */

/*! \addtogroup cglmode
 * 	@{
 */
void interaction_cgl_keyboard_handler(interaction_mode *mode, unsigned char key, int x, int y) {
	standard_keyboard(key, x, y);
}

//! this calls back to a glut-ready function, therefore we have to convert our state back to glut.
void interaction_cgl_mouse_handler(interaction_mode *mode, int button, int state, int x, int y) {
	int glut_button = 0, glut_state = 0;
	if (button == cgls_interaction_left_button)   glut_button = GLUT_LEFT_BUTTON;
	if (button == cgls_interaction_right_button)  glut_button = GLUT_RIGHT_BUTTON;
	if (button == cgls_interaction_middle_button) glut_button = GLUT_MIDDLE_BUTTON;
	if (state == cgls_interaction_button_down)    glut_state = GLUT_DOWN;
	if (state == cgls_interaction_button_up)      glut_state = GLUT_UP;
	standard_mouse_func(glut_button, glut_state, x, y);
}

void interaction_cgl_motion_handler(interaction_mode *mode, int x, int y) {
	standard_mouse_motion(x, y);
}

interaction_mode* make_default_cgl_interaction_mode() {
	interaction_mode *mode = make_interaction_mode("cgl-default");
	mode->type = cgls_interaction_mode_cgl;
	change_fallback_keyhandler_for_mode(mode, interaction_cgl_keyboard_handler);
	change_fallback_mouse_handler_for_mode(mode, interaction_cgl_mouse_handler);
	change_motion_handler_for_mode(mode, interaction_cgl_motion_handler);
	return mode;
}
//! @}


/*! \defgroup cglsmode cgls mode
 *  another very simple mode: the additional functions for the cgls viewer.
 *  \ingroup interaction
 */

/*!	\addtogroup cglsmode
 * 	@{
 */

//! print the camera's lookat data to the terminal
void interaction_print_camera_lookat(interaction_mode *mode, int x, int y) {
	vec3f cam_pos, cam_dir, cam_up;
	matrix4x4f *lookat_matrix = lookat_matrix_of_cam(current_camera());
	extract_pos_vec3f_of_matrix(&cam_pos, lookat_matrix);
	extract_dir_vec3f_of_matrix(&cam_dir, lookat_matrix);
	extract_up_vec3f_of_matrix(&cam_up, lookat_matrix);
	printf("--pos %f,%f,%f ", cam_pos.x, cam_pos.y, cam_pos.z);
	printf("--dir %f,%f,%f ", cam_dir.x, cam_dir.y, cam_dir.z);
	printf("--up %f,%f,%f\n", cam_up.x, cam_up.y, cam_up.z);
}

void interaction_increase_move_factor(interaction_mode *mode, int x, int y) {
	cgl_cam_move_factor *= 2;
}

void interaction_decrease_move_factor(interaction_mode *mode, int x, int y) {
	cgl_cam_move_factor /= 2;
}

enum { interaction_camera_stack_elements = 64 };
static matrix4x4f interaction_camera_stack[interaction_camera_stack_elements];
static int interaction_camera_stack_pointer = 0;

void interaction_push_camera(interaction_mode *mode, int x, int y) {
	if (interaction_camera_stack_pointer == interaction_camera_stack_elements) {
		info_line("Camera stack overflow. What are you doing? :)");
		return;
	}
	info_line("pushing camera matrix %d.", interaction_camera_stack_pointer);
	copy_matrix4x4f(interaction_camera_stack + interaction_camera_stack_pointer++, lookat_matrix_of_cam(current_camera()));
}

void interaction_pop_camera(interaction_mode *mode, int x, int y) {
	if (interaction_camera_stack_pointer == 0) {
		info_line("Camera stack underflow. No worries. :)");
		return;
	}
	copy_matrix4x4f(lookat_matrix_of_cam(current_camera()), interaction_camera_stack + --interaction_camera_stack_pointer);
	info_line("popping camera matrix %d.", interaction_camera_stack_pointer);
	recompute_gl_matrices_of_cam(current_camera());
}

interaction_mode* make_default_cgls_interaction_mode() {
	interaction_mode *mode = make_interaction_mode("cgls-default");
	mode->type = cgls_interaction_mode_cgls;
	add_function_key_to_mode(mode, 'c', cgls_interaction_no_modifier, interaction_print_camera_lookat);
	add_function_key_to_mode(mode, '+', cgls_interaction_no_modifier, interaction_increase_move_factor);
	add_function_key_to_mode(mode, '-', cgls_interaction_no_modifier, interaction_decrease_move_factor);
	add_function_key_to_mode(mode, ' ', cgls_interaction_shift, interaction_push_camera);
	add_function_key_to_mode(mode, ' ', cgls_interaction_alt, interaction_pop_camera);
	return mode;
}
//! @}


/*! \defgroup blendermode blender-sytle interaction
 *  select and transform objects similar to blender (no `passive mothion', though).
 *
 *  when an object is selected you can
 *  \li use 'g' to grab an object. 
 *  	after this you have to select an axis along which to translate.
 *  	hitting g also activates the motion handler, i.e you wont be able to navigate in the scene, but will move the selected object.
 *  
 *  \li use 'g' or <esc> to leave grab mode.
 *
 *  \li a non blender-style addition: use 'o' to turn a selected light on or off.
 *
 *  \li use 'a' to deselect.
 */

/*!	\addtogroup blendermode
 * 	@{
 */
enum { none, X, Y, Z };

struct blendermode_aux {
	scene_ref scene;
	picking_buffer_ref picking;
	bool grab, rotate, scale;
	int axis;
	drawelement_ref selected_de;
};

void interaction_bm_enter_grab_mode(interaction_mode *mode);
void interaction_bm_enter_rotate_mode(interaction_mode *mode);
void interaction_bm_enter_scale_mode(interaction_mode *mode);
void interaction_bm_leave_grs_mode(interaction_mode *mode);

void interaction_bm_leave_grs_key(interaction_mode *mode, int x, int y) {
	interaction_bm_leave_grs_mode(mode);
}

void interaction_bm_light_switch(interaction_mode *mode, int x, int y) {
	struct blendermode_aux *bm = mode->aux;
	if (valid_drawelement_ref(bm->selected_de)) {
		light_ref light = find_light_by_representation(bm->selected_de);
		if (valid_light_ref(light))
			if (light_is_on(light))
				light_off(light);
			else
				light_on(light);
	}
}

void interaction_bm_grab_key(interaction_mode *mode, int x, int y) {
	struct blendermode_aux *bm = mode->aux;
	interaction_bm_enter_grab_mode(mode);
}

void interaction_bm_rotate_key(interaction_mode *mode, int x, int y) {
	struct blendermode_aux *bm = mode->aux;
	interaction_bm_enter_rotate_mode(mode);
}

void interaction_bm_scale_key(interaction_mode *mode, int x, int y) {
	struct blendermode_aux *bm = mode->aux;
	interaction_bm_enter_scale_mode(mode);
}

void interaction_bm_deselect_key(interaction_mode *mode, int x, int y) {
	struct blendermode_aux *bm = mode->aux;
	add_function_key_to_mode(mode, 'g', cgls_interaction_no_modifier, 0);
	add_function_key_to_mode(mode, 'r', cgls_interaction_no_modifier, 0);
	add_function_key_to_mode(mode, 's', cgls_interaction_no_modifier, 0);
	add_function_key_to_mode(mode, 'o', cgls_interaction_no_modifier, 0);
	add_function_key_to_mode(mode, 'a', cgls_interaction_no_modifier, 0);
	add_function_key_to_mode(mode, 'h', cgls_interaction_no_modifier, 0);
	add_function_key_to_mode(mode, 'h', cgls_interaction_alt, 0);
	add_function_key_to_mode(mode, 'b', cgls_interaction_no_modifier, 0);
	if (valid_drawelement_ref(bm->selected_de))
		info_line("deselected %s.", drawelement_name(bm->selected_de));
	bm->selected_de.id = -1;
	interaction_bm_leave_grs_mode(mode);
}

void interaction_bm_hide_key(interaction_mode *mode, int x, int y) {
	struct blendermode_aux *bm = mode->aux;
	if (valid_drawelement_ref(bm->selected_de))
		hide_drawelement(bm->selected_de, !drawelement_hidden(bm->selected_de));
}

void interaction_bm_unhide_key(interaction_mode *mode, int x, int y) {
	for (struct drawelement_list *run = list_drawelements(); run; run = run->next)
		hide_drawelement(run->ref, false);
}

void interaction_bm_toggle_bb(interaction_mode *mode, int x, int y) {
#ifdef CGLS_DRAWELEMENT_BB_VIS
	struct blendermode_aux *bm = mode->aux;
	if (valid_drawelement_ref(bm->selected_de))
		if (drawelement_has_bounding_box(bm->selected_de))
			drawelement_show_bounding_box(bm->selected_de, !drawelement_shows_bounding_box(bm->selected_de));
#endif
}

void interaction_bm_mouse(interaction_mode *mode, int button, int state, int x, int y) {
	struct blendermode_aux *bm = mode->aux;
	if (state == cgls_interaction_button_down) {
		int yy = picking_buffer_height(bm->picking) - y;
		update_picking_buffer(bm->picking, x, yy);
		bm->selected_de = read_picking_buffer(bm->picking, x, yy);
		if (valid_drawelement_ref(bm->selected_de)) {
			add_function_key_to_mode(mode, 'g', cgls_interaction_no_modifier, interaction_bm_grab_key);
			add_function_key_to_mode(mode, 'r', cgls_interaction_no_modifier, interaction_bm_rotate_key);
			add_function_key_to_mode(mode, 's', cgls_interaction_no_modifier, interaction_bm_scale_key);
			add_function_key_to_mode(mode, 'o', cgls_interaction_no_modifier, interaction_bm_light_switch);
			add_function_key_to_mode(mode, 'a', cgls_interaction_no_modifier, interaction_bm_deselect_key);
			add_function_key_to_mode(mode, 'h', cgls_interaction_no_modifier, interaction_bm_hide_key);
			add_function_key_to_mode(mode, 'h', cgls_interaction_alt, interaction_bm_unhide_key);
			add_function_key_to_mode(mode, 'b', cgls_interaction_no_modifier, interaction_bm_toggle_bb);
			info_line("selected drawelement %s.", drawelement_name(bm->selected_de));
		}
		else {
			interaction_bm_deselect_key(mode, x, y);
			info_line("selected nothing.");
		}
	}
}

void interaction_bm_set_axis_to_x(interaction_mode *mode, int x, int y) {
	struct blendermode_aux *bm = mode->aux;
	if (valid_drawelement_ref(bm->selected_de)) {
		info_line("axis = X.");
		bm->axis = X;
	}
	else
		interaction_bm_leave_grs_mode(mode);
}

void interaction_bm_set_axis_to_y(interaction_mode *mode, int x, int y) {
	struct blendermode_aux *bm = mode->aux;
	if (valid_drawelement_ref(bm->selected_de)) {
		info_line("axis = Y.");
		bm->axis = Y;
	}
	else
		interaction_bm_leave_grs_mode(mode);
}

void interaction_bm_set_axis_to_z(interaction_mode *mode, int x, int y) {
	struct blendermode_aux *bm = mode->aux;
	if (valid_drawelement_ref(bm->selected_de)) {
		info_line("axis = Z.");
		bm->axis = Z;
	}
	else
		interaction_bm_leave_grs_mode(mode);
}

void interaction_bm_grab_motion(interaction_mode *mode, int x, int y) {
	struct blendermode_aux *bm = mode->aux;
	if (!valid_drawelement_ref(bm->selected_de)) {
		interaction_bm_leave_grs_mode(mode);
		return;
	}
	if (bm->axis != none) {
		float delta_x = x - cgls_interaction_last_mouse_x;
		float delta_y = cgls_interaction_last_mouse_y - y;
		// transform the eyespace x/y axes to world space
		static vec4f xax = { 1, 0, 0, 0 };
		static vec4f yax = { 0, 1, 0, 0 };
		static vec4f zax = { 0, 0, 1, 0 };
		vec4f x_eye, y_eye;
		matrix4x4f *view = gl_normal_matrix_for_view_of(current_camera());
		matrix4x4f eye_to_world;
		invert_matrix4x4f(&eye_to_world, view);
		vec4f x_direction, y_direction;
		multiply_matrix4x4f_vec4f(&x_direction, &eye_to_world, &xax);
		multiply_matrix4x4f_vec4f(&y_direction, &eye_to_world, &yax);
		// the thusly obtained directions give the direction the given movement corresponds to in the world.
		// this is not very intuitive, however. therefore we limit the translation to one axis
		vec4f mask = { (bm->axis == X?1:0), (bm->axis==Y?1:0), (bm->axis==Z?1:0), 0 };
		mul_components_vec4f(&x_direction, &x_direction, &mask);
		mul_components_vec4f(&y_direction, &y_direction, &mask);

		// apply
		mul_vec4f_by_scalar(&x_direction, &x_direction, delta_x * cgls_interaction_scale);
		mul_vec4f_by_scalar(&y_direction, &y_direction, delta_y * cgls_interaction_scale);
		matrix4x4f *de_trafo = drawelement_trafo(bm->selected_de);
		vec4f whole;
		add_components_vec4f(&whole, &x_direction, &y_direction);
		de_trafo->col_major[12] += whole.x;
		de_trafo->col_major[13] += whole.y;
		de_trafo->col_major[14] += whole.z;
	}
}

void make_translation_matrix4x4f(matrix4x4f *mat, vec3f *transl) {
	make_unit_matrix4x4f(mat);
	mat->col_major[12] = transl->x;
	mat->col_major[13] = transl->y;
	mat->col_major[14] = transl->z;
}

void interaction_bm_rotate_motion(interaction_mode *mode, int x, int y) {
	struct blendermode_aux *bm = mode->aux;
	if (!valid_drawelement_ref(bm->selected_de)) {
		interaction_bm_leave_grs_mode(mode);
		return;
	}
	if (bm->axis != none) {
		int w = picking_buffer_width(bm->picking), h = picking_buffer_height(bm->picking);
		vec2f c = { w/2.0, h/2.0 },
			  last_pos = { cgls_interaction_last_mouse_x, h-cgls_interaction_last_mouse_y },
			  curr_pos = { x, h-y };
		vec2f last, curr, up = { 0, 1 };
		sub_components_vec2f(&last, &last_pos, &c);
		sub_components_vec2f(&curr, &curr_pos, &c);
		normalize_vec2f(&last);
		normalize_vec2f(&curr);
		float last_angle = acosf(last.x);
		if (last.y < 0) last_angle = 2*M_PI - last_angle;
		float new_angle = acosf(curr.x);
		if (curr.y < 0) new_angle = 2*M_PI - new_angle;
		float angle = new_angle - last_angle;
		if (angle > M_PI)  angle -= 2*M_PI;
		if (angle < -M_PI) angle += 2*M_PI;
		
		static vec3f xax = { 1, 0, 0 };
		static vec3f yax = { 0, 1, 0 };
		static vec3f zax = { 0, 0, 1 };
		static vec3f null = { 0, 0, 0 };
		vec3f *ax = bm->axis == X ? &xax : bm->axis == Y ? &yax : &zax;
		matrix4x4f rot, old_trafo, *de_trafo = drawelement_trafo(bm->selected_de);
		make_rotation_matrix4x4f(&rot, ax, angle);
		vec3f translation;
		extract_pos_vec3f_of_matrix(&translation, de_trafo);
		de_trafo->col_major[12] = de_trafo->col_major[13] = de_trafo->col_major[14] = 0;
		copy_matrix4x4f(&old_trafo, de_trafo);
// 		multiply_matrices4x4f(de_trafo, &old_trafo, &rot);
		multiply_matrices4x4f(de_trafo, &rot, &old_trafo);
		de_trafo->col_major[12] = translation.x; de_trafo->col_major[13] = translation.y; de_trafo->col_major[14] = translation.z;
	}
}

void interaction_bm_scale_motion(interaction_mode *mode, int x, int y) {
	struct blendermode_aux *bm = mode->aux;
	if (!valid_drawelement_ref(bm->selected_de)) {
		interaction_bm_leave_grs_mode(mode);
		return;
	}

	int w = picking_buffer_width(bm->picking), h = picking_buffer_height(bm->picking);
	vec2f c = { w/2.0, h/2.0 },
		  last_pos = { cgls_interaction_last_mouse_x, h-cgls_interaction_last_mouse_y },
		  curr_pos = { x, h-y };
	vec2f last, curr, up = { 0, 1 };
	sub_components_vec2f(&last, &last_pos, &c);
	sub_components_vec2f(&curr, &curr_pos, &c);

	float old_len = length_of_vec2f(&last);
	float new_len = length_of_vec2f(&curr);
	float scale = new_len / old_len;

	vec3f scale_vec = { 1, 1, 1};
	if (bm->axis == X) scale_vec.x = scale;
	else if (bm->axis == Y) scale_vec.y = scale;
	else if (bm->axis == Z) scale_vec.z = scale;
	else scale_vec.x = scale_vec.y = scale_vec.z = scale;
	
	matrix4x4f *de_trafo = drawelement_trafo(bm->selected_de);
	matrix4x4f old_trafo, scale_mat;
	make_scale_matrix4x4f(&scale_mat, &scale_vec);
	
	vec3f translation;
	extract_pos_vec3f_of_matrix(&translation, de_trafo);
	de_trafo->col_major[12] = de_trafo->col_major[13] = de_trafo->col_major[14] = 0;
	copy_matrix4x4f(&old_trafo, de_trafo);
	
// 	multiply_matrices4x4f(de_trafo, &old_trafo, &scale_mat);
	multiply_matrices4x4f(de_trafo, &scale_mat, &old_trafo);
	de_trafo->col_major[12] = translation.x; de_trafo->col_major[13] = translation.y; de_trafo->col_major[14] = translation.z;
}

void interaction_bm_enter_grab_mode(interaction_mode *mode) {
	struct blendermode_aux *bm = mode->aux;
	info_line("grab mode.");
	bm->grab = true;
	bm->rotate = false;
	bm->scale = false;
	mode->motion_handler = interaction_bm_grab_motion;
	add_function_key_to_mode(mode, 'x', cgls_interaction_no_modifier, interaction_bm_set_axis_to_x);
	add_function_key_to_mode(mode, 'y', cgls_interaction_no_modifier, interaction_bm_set_axis_to_y);
	add_function_key_to_mode(mode, 'z', cgls_interaction_no_modifier, interaction_bm_set_axis_to_z);
	add_function_key_to_mode(mode,  27, cgls_interaction_no_modifier, interaction_bm_leave_grs_key);
}

void interaction_bm_enter_rotate_mode(interaction_mode *mode) {
	struct blendermode_aux *bm = mode->aux;
	info_line("rotate mode.");
	bm->rotate = true;
	bm->scale = false;
	bm->scale = false;
	mode->motion_handler = interaction_bm_rotate_motion;
	add_function_key_to_mode(mode, 'x', cgls_interaction_no_modifier, interaction_bm_set_axis_to_x);
	add_function_key_to_mode(mode, 'y', cgls_interaction_no_modifier, interaction_bm_set_axis_to_y);
	add_function_key_to_mode(mode, 'z', cgls_interaction_no_modifier, interaction_bm_set_axis_to_z);
	add_function_key_to_mode(mode,  27, cgls_interaction_no_modifier, interaction_bm_leave_grs_key);
}

void interaction_bm_enter_scale_mode(interaction_mode *mode) {
	struct blendermode_aux *bm = mode->aux;
	info_line("scale mode.");
	bm->grab = false;
	bm->rotate = false;
	bm->scale = true;
	bm->axis = none;
	mode->motion_handler = interaction_bm_scale_motion;
	add_function_key_to_mode(mode, 'x', cgls_interaction_no_modifier, interaction_bm_set_axis_to_x);
	add_function_key_to_mode(mode, 'y', cgls_interaction_no_modifier, interaction_bm_set_axis_to_y);
	add_function_key_to_mode(mode, 'z', cgls_interaction_no_modifier, interaction_bm_set_axis_to_z);
	add_function_key_to_mode(mode,  27, cgls_interaction_no_modifier, interaction_bm_leave_grs_key);
}

void interaction_bm_leave_grs_mode(interaction_mode *mode) {
	struct blendermode_aux *bm = mode->aux;
	info_line("leaving grab mode.");
	bm->grab = false;
	bm->rotate = false;
	bm->scale = false;
	mode->motion_handler = 0;
	add_function_key_to_mode(mode, 'x', cgls_interaction_no_modifier, 0);
	add_function_key_to_mode(mode, 'y', cgls_interaction_no_modifier, 0);
	add_function_key_to_mode(mode, 'z', cgls_interaction_no_modifier, 0);
	add_function_key_to_mode(mode,  27, cgls_interaction_no_modifier, 0);
}

interaction_mode* make_blender_style_interaction_mode(scene_ref scene, picking_buffer_ref pickingbuffer) {
	interaction_mode *mode = make_interaction_mode("blender mode");
	mode->type = cgls_interaction_mode_blender;
	struct blendermode_aux *aux = mode->aux = malloc(sizeof(struct blendermode_aux));
	aux->scene = scene;
	aux->picking = pickingbuffer;
	aux->axis = none;
	aux->grab = false;
	aux->rotate = false;
	aux->scale = false;
	aux->selected_de.id = -1;

	add_mouse_function_to_mode(mode, cgls_interaction_right_button, cgls_interaction_button_down, cgls_interaction_no_modifier, interaction_bm_mouse);

	return mode;
}

drawelement_ref blender_mode_selected_drawelement(interaction_mode *mode) {
	if (mode->type != cgls_interaction_mode_blender) {
		drawelement_ref ret = { -1 };
		return ret;
	}
	return ((struct blendermode_aux*)mode->aux)->selected_de;
}

//! @}

// actual mode management

define_slist(mode_node, interaction_mode *mode);
static struct mode_node *modes = 0;

static void interaction_base_keyhandler(unsigned char key, int x, int y) {
	int glut_modifiers = glutGetModifiers();
	unsigned int modifiers = 0;
	if (glut_modifiers & GLUT_ACTIVE_SHIFT) {
		if (isalpha(key))
			modifiers += cgls_interaction_shift;
	}
	if (glut_modifiers & GLUT_ACTIVE_CTRL)	  modifiers += cgls_interaction_control;
	if (glut_modifiers & GLUT_ACTIVE_ALT)     modifiers += cgls_interaction_alt;

// 	printf("key: %d mod %s %s %s\n", key, 
// 			(glut_modifiers & GLUT_ACTIVE_SHIFT)   ? "[S]" : "[ ]",
// 			(glut_modifiers & GLUT_ACTIVE_CTRL)	   ? "[C]" : "[ ]",
// 			(glut_modifiers & GLUT_ACTIVE_ALT)     ? "[A]" : "[ ]");

	bool handled = false;
	for (struct mode_node *run = modes; run; run = run->next) {
		interaction_mode *mode = run->mode;
		if (mode->key_function_table && mode->key_function_table[modifiers][key]) {
			mode->key_function_table[modifiers][key](mode, x, y);
			handled = true;
			break;
		}
		if (mode->fallback_keyhandler) {
			mode->fallback_keyhandler(mode, key, x, y);
			handled = true;
			break;
		}
	}
	if (!handled)
		info_line("cannot find a mapping for key %c.", key);
}

static void interaction_mouse_handler(int glut_button, int glut_state, int x, int y) {
	int glut_modifiers = glutGetModifiers();
	unsigned int modifiers = 0;
	if (glut_modifiers & GLUT_ACTIVE_SHIFT)   modifiers += cgls_interaction_shift;
	if (glut_modifiers & GLUT_ACTIVE_CTRL)	  modifiers += cgls_interaction_control;
	if (glut_modifiers & GLUT_ACTIVE_ALT)     modifiers += cgls_interaction_alt;
	unsigned int button = 0;
	if (glut_button == GLUT_LEFT_BUTTON)      button += cgls_interaction_left_button;
	if (glut_button == GLUT_RIGHT_BUTTON)     button += cgls_interaction_right_button;
	if (glut_button == GLUT_MIDDLE_BUTTON)    button += cgls_interaction_middle_button;
	unsigned int state = 0;
	if (glut_state == GLUT_UP)                state += cgls_interaction_button_up;
	if (glut_state == GLUT_DOWN)              state += cgls_interaction_button_down;

	bool handled = false;
	for (struct mode_node *run = modes; run; run = run->next) {
		interaction_mode *mode = run->mode;
		if (mode->mouse_function_table && mode->mouse_function_table[modifiers][button][state]) {
			mode->mouse_function_table[modifiers][button][state](mode, button, state, x, y);
			handled = true;
			break;
		}
		if (mode->fallback_mouse_handler) {
			mode->fallback_mouse_handler(mode, button, state, x, y);
			handled = true;
			break;
		}
	}
	if (!handled)
		info_line("cannot find a mouse handler.");

	if (button == cgls_interaction_button_down)
		cgls_interaction_last_mouse_x = x,
		cgls_interaction_last_mouse_y = y;
}

static void interaction_motion_handler(int x, int y) {
	bool handled = false;
	for (struct mode_node *run = modes; run; run = run->next) {
		interaction_mode *mode = run->mode;
		if (mode->motion_handler) {
			mode->motion_handler(mode, x, y);
			handled = true;
			break;
		}
	}
	if (!handled)
		info_line("cannot find a motion handler.");
	cgls_interaction_last_mouse_x = x;
	cgls_interaction_last_mouse_y = y;
}

void initialize_interaction() {
	push_interaction_mode(make_default_cgl_interaction_mode());
	register_keyboard_function(interaction_base_keyhandler);
	register_mouse_function(interaction_mouse_handler);
	register_mouse_motion_function(interaction_motion_handler);
}

void push_interaction_mode(interaction_mode *mode) {
	struct mode_node *node = malloc(sizeof(struct mode_node));
	node->next = modes;
	node->mode = mode;
	modes = node;
}

interaction_mode* pop_interaction_mode() {
	if (!modes) return 0;
	interaction_mode *mode = modes->mode;
	modes = modes->next;
	return mode;
}

