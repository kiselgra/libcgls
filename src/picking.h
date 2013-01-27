#ifndef __CGLS_PICKING_H__ 
#define __CGLS_PICKING_H__ 

#include "scene.h"

typedef struct {
	int id;
} picking_buffer_ref;

picking_buffer_ref make_picking_buffer(const char *name, unsigned int width, unsigned int height);
void update_picking_buffer(picking_buffer_ref ref, scene_ref scene, int x, int y);
drawelement_ref read_picking_buffer(picking_buffer_ref ref, unsigned int x, unsigned int y);

#endif

