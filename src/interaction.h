#ifndef __INTERACTION_H__ 
#define __INTERACTION_H__ 

#include "picking.h"
#include "scene.h"

#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct cgls_interaction_mode interaction_mode;

typedef void (*interaction_key_function_t)(interaction_mode *, int, int);
typedef void (*interaction_mouse_function_t)(interaction_mode *, int, int, int, int);
typedef void (*interaction_motion_function_t)(interaction_mode *, int, int);
typedef void (*interaction_keyhandler_t)(interaction_mode *, unsigned char, int, int);

typedef struct cgls_interaction_mode {
	const char *name;
	interaction_key_function_t **key_function_table;
	interaction_mouse_function_t ***mouse_function_table;
	interaction_keyhandler_t fallback_keyhandler;
	void (*fallback_mouse_handler)(interaction_mode *, int, int, int, int);
	interaction_motion_function_t motion_handler;
	void *aux;
} interaction_mode;

enum {
	cgls_interaction_no_modifier = 0,
	cgls_interaction_shift = 1,
	cgls_interaction_control = 2,
	cgls_interaction_alt = 4,
	cgls_interaction_modifier_combinations = 8
};

enum {
	cgls_interaction_no_button = 0,
	cgls_interaction_left_button = 1,
	cgls_interaction_right_button = 2,
	cgls_interaction_middle_button = 4,
	cgls_interaction_button_combinations = 8
};

enum {
	cgls_interaction_button_dummy = 0,
	cgls_interaction_button_down = 1,
	cgls_interaction_button_up = 2,
	cgls_interaction_button_state_combinations = 4
		//!< handlers can be used with up/down/u+d/00 (well, 00 makes no sense, but it is simpler to handle it this way)
};

// the mode interface itself
interaction_mode* make_interaction_mode(const char *name);
void add_function_key_to_mode(interaction_mode *mode, unsigned char key, unsigned int modifiers, interaction_key_function_t call);
void add_mouse_function_to_mode(interaction_mode *mode, int buttons, int states, unsigned int modifiers, interaction_mouse_function_t call);

// mode handling
void initialize_interaction();
void push_interaction_mode(interaction_mode *mode);
interaction_mode* pop_interaction_mode();

// specific modes
interaction_mode* make_default_cgl_interaction_mode();
interaction_mode* make_default_cgls_interaction_mode();
interaction_mode* make_blender_style_interaction_mode(scene_ref scene, picking_buffer_ref pickingbuffer);

// generally useful mode functions
void interaction_increase_move_factor(interaction_mode *mode, int x, int y);
void interaction_decrease_move_factor(interaction_mode *mode, int x, int y);
void interaction_print_camera_lookat(interaction_mode *mode, int x, int y);

// mode info printing
typedef void (*info_line_printer_t)(const char *fmt, va_list ap);
void default_info_line_printer(const char *fmt, va_list ap);
void info_line(const char *fmt, ...);

#ifdef __cplusplus
}
#endif

#endif

