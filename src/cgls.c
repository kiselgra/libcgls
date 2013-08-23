
#ifdef WITH_GUILE

void register_scheme_functions_for_camera_animation();
void register_scheme_functions_for_cgls_objloader();
void register_scheme_functions_for_command_animation();
void register_scheme_functions_for_material();
void register_scheme_functions_for_drawelement();
void register_scheme_functions_for_light();
void register_scheme_functions_for_scene();
void register_scheme_functions_for_cgls_modelloader();
void register_scheme_functions_for_console();
void register_scheme_functions_for_path_animation();
void register_scheme_functions_for_skeletal_animation();
void register_scheme_functions_for_stock_shader();

void register_cgls_scheme_functions() {
	register_scheme_functions_for_camera_animation();
	register_scheme_functions_for_cgls_objloader();
	register_scheme_functions_for_command_animation();
	register_scheme_functions_for_drawelement();
	register_scheme_functions_for_light();
	register_scheme_functions_for_material();
	register_scheme_functions_for_scene();
	register_scheme_functions_for_cgls_modelloader();
	register_scheme_functions_for_console();
	register_scheme_functions_for_path_animation();
	register_scheme_functions_for_skeletal_animation();
	register_scheme_functions_for_stock_shader();
}

#endif

