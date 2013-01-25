#ifndef __CGLS_PICKING_H__ 
#define __CGLS_PICKING_H__ 

typedef struct {
	int id;
} picking_buffer_ref;

picking_buffer_ref make_picking_buffer(const char *name, unsigned int width, unsigned int height);

#endif

