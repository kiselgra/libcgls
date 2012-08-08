#include <libcgl/libcgl.h>

#include <GL/freeglut.h>
#include <iostream>
#include <string.h>

#include "drawelement.h"

#include "cmdline.h"

using namespace std;

mesh_ref cube;
shader_ref cube_shader;
drawelement_ref de;

void display() {
	glClearColor(0,0,0.25,1);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glEnable(GL_DEPTH_TEST);

	render_drawelement(de);

	swap_buffers();
}

void idle() {
	glutPostRedisplay();
}

void keyboard(unsigned char key, int x, int y) {
	if (key == 'c') {
		vec3f cam_pos, cam_dir, cam_up;
		matrix4x4f *lookat_matrix = lookat_matrix_of_cam(current_camera());
		extract_pos_vec3f_of_matrix(&cam_pos, lookat_matrix);
		extract_dir_vec3f_of_matrix(&cam_dir, lookat_matrix);
		extract_up_vec3f_of_matrix(&cam_up, lookat_matrix);
		cout << "--pos " << cam_pos.x << "," << cam_pos.y << "," << cam_pos.z << " ";
		cout << "--dir " << cam_dir.x << "," << cam_dir.y << "," << cam_dir.z << " ";
		cout << "--up " << cam_up.x << "," << cam_up.y << "," << cam_up.z << endl;
	}
	else standard_keyboard(key, x, y);
}

// try to implement this in scheme!
// then this could be part of a scene description
bool custom_light_handler(drawelement_ref ref, const char *uniform, int location) {
	if (strcmp(uniform, "light_dir") == 0)         glUniform3f(location, 0, -1, -0.2);
	else if (strcmp(uniform, "light_col") == 0)    glUniform3f(location, 1, 0.9, 0.1);
	else if (strcmp(uniform, "color") == 0)        glUniform3f(location, .9, .9, .9);
	else return false;
	return true;
}

void actual_main() 
{
	register_display_function(display);
	register_idle_function(idle);
	register_keyboard_function(keyboard);

    cube = make_cube("test cube", 0);
    cube_shader = find_shader("diffuse-pl");

	de = make_drawelement("testcube", cube, cube_shader);
	prepend_uniform_handler(de, default_matrix_uniform_handler);
	prepend_uniform_handler(de, custom_light_handler);

	enter_glut_main_loop();

}

int main(int argc, char **argv)
{	
	parse_cmdline(argc, argv);
	
	std::string home(getenv("HOME"));
	append_image_path((home+"/render-data/images/").c_str());

	int guile_mode = guile_cfg_only;
	startup_cgl("name", 3, 3, argc, argv, 1366, 768, actual_main, guile_mode, false, "default.scm");

	return 0;
}


