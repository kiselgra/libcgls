#ifndef __GEN_VERTEX_SHADERS_H__ 
#define __GEN_VERTEX_SHADERS_H__ 

#ifdef __cplusplus
extern "C" {
#endif

const char* select_vertex_shader (bool normals, bool tex, bool path, int bones, bool instanced);

#ifdef __cplusplus
}
#endif

#endif

