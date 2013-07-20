#ifndef __CONSOLE_H__ 
#define __CONSOLE_H__ 

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	int id;
} console_ref;

console_ref make_console(const char *name, int screen_w, int screen_h, int height);
void render_console(console_ref ref);

#ifdef __cplusplus
}
#endif

#endif

