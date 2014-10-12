#ifndef __GEN_GBUFFER_SHADERS_H__ 
#define __GEN_GBUFFER_SHADERS_H__ 

#ifdef __cplusplus
extern "C" {
#endif

const char* select_gbuffer_fragment_part(bool mask_tex, bool normalmap, bool amb_col, bool amb_tex, bool diff_col, bool diff_tex, 
                                         bool spec_col, bool spec_tex, const char ***u);

#ifdef __cplusplus
}
#endif

#endif

