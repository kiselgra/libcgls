#ifndef __CGLS_PICKING_H__ 
#define __CGLS_PICKING_H__ 

#include "refs.h"

#include "scene.h"

#ifdef __cplusplus
extern "C" {
#endif


picking_buffer_ref make_picking_buffer(const char *name, struct drawelement_array *array, unsigned int width, unsigned int height);
void update_picking_buffer(picking_buffer_ref ref, int x, int y);
drawelement_ref read_picking_buffer(picking_buffer_ref ref, unsigned int x, unsigned int y);
void highlight_object(picking_buffer_ref ref, drawelement_ref de);

int picking_buffer_height(picking_buffer_ref ref);
int picking_buffer_width(picking_buffer_ref ref);

define_array(picking_buffer);

#ifdef __cplusplus
}
#endif

#endif

