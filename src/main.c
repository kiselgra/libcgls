#include "drawelement.h"
#include "objloader.h"
#include "scene.h"
#include "basename.h"

#include "cmdline.h"

#include <libcgl/libcgl.h>

#include <GL/freeglut.h>
#include <string.h>

#include <stdio.h>
#include <stdlib.h>

scene_ref the_scene;

void display() {
	glClearColor(0,0,0.25,1);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glEnable(GL_DEPTH_TEST);

	render_scene(the_scene);

	swap_buffers();
}

void idle() {
	glutPostRedisplay(); } 
void keyboard(unsigned char key, int x, int y) {
	if (key == 'c') {
		vec3f cam_pos, cam_dir, cam_up;
		matrix4x4f *lookat_matrix = lookat_matrix_of_cam(current_camera());
		extract_pos_vec3f_of_matrix(&cam_pos, lookat_matrix);
		extract_dir_vec3f_of_matrix(&cam_dir, lookat_matrix);
		extract_up_vec3f_of_matrix(&cam_up, lookat_matrix);
		printf("--pos %f,%f,%f ", cam_pos.x, cam_pos.y, cam_pos.z);
		printf("--dir %f,%f,%f ", cam_dir.x, cam_dir.y, cam_dir.z);
		printf("--up %f,%f,%f\n", cam_up.x, cam_up.y, cam_up.z);
	}
	else standard_keyboard(key, x, y);
}

// try to implement this in scheme!
// then this could be part of a scene description
bool custom_light_handler(drawelement_ref ref, const char *uniform, int location) {
	if (strcmp(uniform, "light_dir") == 0)         glUniform3f(location, 0, -1, -0.2);
	else if (strcmp(uniform, "light_col") == 0)    glUniform3f(location, 1, 0.9, 0.9);
	else if (strcmp(uniform, "hemi_dir") == 0)     glUniform3f(location, cmdline.hemi_dir.x, cmdline.hemi_dir.y, cmdline.hemi_dir.z);
	else return false;
	return true;
}

void adjust_view(const vec3f *bb_min, const vec3f *bb_max, vec3f *cam_pos, float *distance) {
	vec3f bb_center, tmp;
	sub_components_vec3f(&tmp, bb_max, bb_min);
	div_vec3f_by_scalar(&tmp, &tmp, 2);
	add_components_vec3f(&bb_center, &tmp, bb_min);
	
	sub_components_vec3f(&tmp, bb_max, bb_min);
	*distance = length_of_vec3f(&tmp);
	make_vec3f(&tmp, 0, 0, *distance);
	add_components_vec3f(cam_pos, &bb_center, &tmp);

	cgl_cam_move_factor = *distance / 20.0f;
}
	
void actual_main() 
{
	register_display_function(display);
	register_idle_function(idle);
	register_keyboard_function(keyboard);

	scene_ref scene = make_scene(0);
	the_scene = scene;
	int drawelements = 0;
	void create_drawelement(const char *modelname, mesh_ref mesh, material_ref mat) {
		shader_ref s;
		if (cmdline.hemi)
			if (material_textures(mat)) s = find_shader("diffuse-hemi+tex");
			else                        s = find_shader("diffuse-hemi");
		else
			if (material_textures(mat)) s = find_shader("diffuse-dl+tex");
			else                        s = find_shader("diffuse-dl");
		drawelement_ref de = make_drawelement(modelname, mesh, s, mat);
		prepend_uniform_handler(de, default_material_uniform_handler);
		prepend_uniform_handler(de, default_matrix_uniform_handler);
		prepend_uniform_handler(de, custom_light_handler);
		scene_add_drawelement(scene, de);
		++drawelements;
	}

	vec3f min, max;
	const char *file = basename(cmdline.filename); // the gnu version.
	load_objfile_and_create_objects_with_separate_vbos(cmdline.filename, file, &min, &max, create_drawelement);
	printf("created %d drawelement%s.\n", drawelements, drawelements>1?"s":"");
	printf("bb: [%.3f : %.3f]  x  [%.3f : %.3f]  x  [%.3f : %.3f]\n", min.x, max.x, min.y, max.y, min.z, max.z);

	vec3f cam_pos, cam_dir = { 0, 0, -1 }, cam_up = { 0, 1, 0 };
	float cam_distance_to_bb_center;
	adjust_view(&min, &max, &cam_pos, &cam_distance_to_bb_center);
	change_lookat_of_cam(current_camera(), &cam_pos, &cam_dir, &cam_up);

	float near = camera_near(current_camera()),
	      far = camera_far(current_camera());
	while (near > cam_distance_to_bb_center/100.0f) near /= 10.0f;
	while (far < cam_distance_to_bb_center*2.0f) far *= 2.0f;
	change_projection_of_cam(current_camera(), camera_fovy(current_camera()), camera_aspect(current_camera()), near, far);
	recompute_gl_matrices_of_cam(current_camera());

	enter_glut_main_loop();
}

int main(int argc, char **argv)
{	
	parse_cmdline(argc, argv);
	
	char *renderdata;
	int n = asprintf(&renderdata, "%s/render-data/images", getenv("HOME"));
	append_image_path(renderdata);
	n = asprintf(&renderdata, "%s/render-data/images/wikimedia", getenv("HOME"));
	append_image_path(renderdata);
	n = asprintf(&renderdata, "%s/render-data/images/sponza", getenv("HOME"));
	append_image_path(renderdata);
	free(renderdata);

	int guile_mode = guile_cfg_only;
	startup_cgl("name", 3, 3, argc, argv, 1366, 768, actual_main, guile_mode, false, "default.scm");

	return 0;
}


