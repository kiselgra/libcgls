#ifndef __CGLS_REFS_H__ 
#define __CGLS_REFS_H__ 

#define REF(X) typedef struct { int id; } X##_ref;

REF(camera_animation);
REF(command_animation);
REF(console);
REF(drawelement);
REF(light);
REF(material);
REF(path_animation);
REF(picking_buffer);
REF(scene);
REF(single_material_pass);
REF(skeletal_animation);

#undef REF

#endif

