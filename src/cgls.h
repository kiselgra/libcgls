#ifndef __CGLS_H__ 
#define __CGLS_H__ 

#include <libcgl/libcgl.h>

#include "material.h"
#include "drawelement.h"
#include "scene.h"
#include "objloader.h"
#include "stock-shader.h"

#ifdef WITH_GUILE
#ifdef __cplusplus
extern "C" {
#endif
void register_cgls_scheme_functions();
#ifdef __cplusplus
}
#endif
#endif

#endif

