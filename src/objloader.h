#ifndef __CGLS_OBJLOADER_H__ 
#define __CGLS_OBJLOADER_H__ 

#ifdef __cplusplus
extern "C" {
#endif

void load_objfile_and_create_objects_with_separate_vbos(const char *filename, const char *object_name, vec3f *bb_min, vec3f *bb_max, void (*make_drawelem)(const char*, mesh_ref, material_ref), material_ref fallback_material);
void load_objfile_and_create_objects_with_single_vbo(const char *filename, const char *object_name, vec3f *bb_min, vec3f *bb_max, void (*make_drawelem)(const char*, mesh_ref, material_ref, unsigned int start, unsigned int len), material_ref fallback_material);

#ifdef __cplusplus
}
#endif

#endif

