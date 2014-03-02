#ifndef __CONSOLE_H__ 
#define __CONSOLE_H__ 

#include "refs.h"

#ifdef WITH_GUILE
#include <libcgl/scheme.h>
#endif

#include "interaction.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef char* (*console_command_handler_t)(console_ref ref, const char *command);

console_ref make_console(const char *name, int screen_w, int screen_h, int height);
void render_console(console_ref ref);



//
// vi console
//

enum { console_t_vi = 1 };
typedef char* (*vi_command_t)(console_ref ref, int argc, char **argv);

console_ref make_vi_console(const char *name, int screen_w, int screen_h);

console_ref find_console(const char *name);
interaction_mode* console_interaction_mode(console_ref ref);

void add_vi_console_command(console_ref ref, const char *name, vi_command_t handler);

#ifdef WITH_GUILE
void add_vi_console_command_scm(console_ref ref, const char *name, SCM handler);
#endif

void show_message_on_console(console_ref ref, const char *message);

define_array(console);

#ifdef __cplusplus
}
#endif

#endif

