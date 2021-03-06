#ifndef __CGLS_OBJLOADER_H__ 
#define __CGLS_OBJLOADER_H__ 

#include "material.h"
#include "drawelement.h"

#include <libcgl/mesh.h>

#include <libmcm/vectors.h>

#ifdef __cplusplus
extern "C" {
#endif

void load_objfile_and_create_objects_with_separate_vbos(const char *filename, const char *object_name, vec3f *bb_min, vec3f *bb_max, drawelement_ref (*make_drawelem)(const char*, mesh_ref, material_ref, vec3f *bb_min, vec3f *bb_max), material_ref fallback_material);
void load_objfile_and_create_objects_with_single_vbo(const char *filename, const char *object_name, vec3f *bb_min, vec3f *bb_max, drawelement_ref (*make_drawelem)(const char*, mesh_ref, material_ref, unsigned int start, unsigned int len, vec3f *bbmin, vec3f *bbmax), material_ref fallback_material, float merge_factor);
void load_objfile_and_create_objects_with_single_vbo_keeping_cpu_data(const char *filename, const char *object_name, vec3f *bb_min, vec3f *bb_max, drawelement_ref (*make_drawelem)(const char*, mesh_ref, material_ref, unsigned int start, unsigned int len, vec3f *bbmin, vec3f *bbmax), material_ref fallback_material, float merge_factor);
		
#ifdef __cplusplus
}
#endif

#endif

