#include "skeletal.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "drawelement.h"

struct skeletal_animation {
	const char *name;
	struct bone_list *bones;
	struct bone **all_bones;
};

#include <libcgl/mm.h>
define_mm(skeletal_animation, skeletal_animations, skeletal_animation_ref);
#include "skeletal.xx"

typedef bool (*bone_trav_handler_t)(struct bone *bone, int depth, void *data);
bool traverse_bone_hierarchy(struct bone_list *list, bone_trav_handler_t handler, int depth, void *data);
bool count_bones(struct bone *bone, int depth, int *counter);
struct bone_id_state {
	struct bone **array;
	int id;
};
bool assign_ids(struct bone *bone, int depth, struct bone_id_state *state);

skeletal_animation_ref make_skeletal_animation(const char *name, struct bone_list *bones) {
	skeletal_animation_ref ref = allocate_skeletal_animation_ref();
	struct skeletal_animation *sa = skeletal_animations+ref.id;
	sa->name = strdup(name);
	sa->bones = bones;

	int counter = 0;
	traverse_bone_hierarchy(sa->bones, (bone_trav_handler_t)count_bones, 0, &counter);
	sa->all_bones = malloc(sizeof(struct bone*)*counter);

	struct bone_id_state bis;
	bis.array = sa->all_bones;
	bis.id = 0;
	traverse_bone_hierarchy(sa->bones, (bone_trav_handler_t)assign_ids, 0, &bis);
	
	return ref;
}

struct bone* find_bone(struct bone *tree, const char *name) {
	if (strcmp(tree->name, name) == 0)
		return tree;
	for (struct bone_list *run = tree->children; run; run = run->next) {
		struct bone *found = find_bone(run->bone, name);
		if (found)
			return found;
	}
}

bool traverse_bone_hierarchy(struct bone_list *list, bool (*handler)(struct bone *bone, int depth, void *data), int depth, void *data) {
	while (list) {
		for (int i = 0; i < depth; ++i) printf("  ");
		printf("at bone %s\n", list->bone->name);
		if (!handler(list->bone, depth, data))
			list = list->next;
		else
			return true;
	}
	return false;
}

bool count_bones(struct bone *bone, int depth, int *counter) {
	(*counter)++;
	return traverse_bone_hierarchy(bone->children, (bone_trav_handler_t)count_bones, depth+1, counter);
}

bool assign_ids(struct bone *bone, int depth, struct bone_id_state *state) {
	state->array[state->id] = bone;
	state->id++;
	return traverse_bone_hierarchy(bone->children, (bone_trav_handler_t)assign_ids, depth+1, state);
}

struct bone* find_bone_in_skeletal_animation(skeletal_animation_ref ref, const char *bone_name) {
	struct skeletal_animation *sa = skeletal_animations+ref.id;
	for (struct bone_list *run = sa->bones; run; run = run->next) {
		struct bone *found = find_bone(run->bone, bone_name);
		if (found)
			return found;
	}
}

bool bone_matrix_uniform_handler(drawelement_ref *ref, const char *uniform, int location) {
	if (strcmp(uniform, "bone_matrices") == 0) {
		int bones = drawelement_number_of_bones(*ref);
		struct bone **de_bones = drawelement_bones(*ref);
		matrix4x4f *matrices = drawelement_bone_matrix_area(*ref);
		for (int i = 0; i < bones; ++i) {
// 			memcpy(matrices+i, &de_bones[i]->rest_trafo, sizeof(matrix4x4f));
// 			memcpy(matrices+i, &de_bones[i]->rest_trafo, sizeof(matrix4x4f));
			multiply_matrices4x4f(matrices+i, &de_bones[i]->rest_trafo, &de_bones[i]->offset_trafo);
		}
		glUniformMatrix4fv(location, bones, GL_FALSE, matrices);
	}
	else if (strcmp(uniform, "bones") == 0) {
		int bones = drawelement_number_of_bones(*ref);
		glUniform1i(location, bones);
	}
	else
		return false;
	return true;
}
