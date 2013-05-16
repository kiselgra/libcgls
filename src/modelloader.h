#ifndef __MODELLOADER_H__ 
#define __MODELLOADER_H__ 

#include "drawelement.h"

#ifdef __cplusplus
extern "C" {
#endif

void load_model_and_create_objects_with_separate_vbos(const char *filename, const char *object_name, vec3f *bb_min, vec3f *bb_max,
                                                      void (*make_drawelem)(const char*, mesh_ref, material_ref, vec3f *bbmin, vec3f *bbmax), material_ref fallback_material);

#ifdef __cplusplus
}
#endif


#endif

