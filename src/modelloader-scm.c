#include "modelloader.h"

#ifdef WITH_GUILE
#include <libguile.h>
	
#pragma GCC diagnostic ignored "-Wtrampolines"

SCM_DEFINE(s_load_model_and_create_objects_with_separate_vbos,
           "load-model-and-create-objects-with-separate-vbos", 4, 0, 0, (SCM filename, SCM object_name, SCM callback, SCM fallback_mat), "") {
	char *f = scm_to_locale_string(filename);
	char *o = scm_to_locale_string(object_name);
	drawelement_ref create_drawelement_forwarder(const char *modelname, mesh_ref mesh, material_ref mat, vec3f *bmi, vec3f *bma) {
		SCM id = scm_call_5(callback, scm_from_locale_string(modelname), scm_from_int(mesh.id), scm_from_int(mat.id), vec3f_to_list(bmi), vec3f_to_list(bma));
		drawelement_ref ref = { scm_to_int(id) };
		return ref;
	}
	vec3f min, max;
	vec4f amb = {1,0,0,1}, diff = {1,0,0,1}, spec = {1,0,0,1};
	material_ref fallback = { scm_to_int(fallback_mat) };
	load_model_and_create_objects_with_separate_vbos(f, o, &min, &max, create_drawelement_forwarder, fallback);
	return scm_values(scm_list_2(vec3f_to_list(&min), vec3f_to_list(&max)));
}

void register_scheme_functions_for_cgls_modelloader() {
#ifndef SCM_MAGIC_SNARFER
#include "modelloader-scm.x"
#endif
}


#endif


