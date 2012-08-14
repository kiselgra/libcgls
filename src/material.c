#include <string.h>
#include <stdlib.h>

#include <stdio.h>

/* a *very* simple material definition, suited to standard gl rendering. 
 * extensible the good ol' c way.
 */

typedef struct {
	int id;
} material_ref;


#include <libcgl/libcgl.h>

#include "slist.h"

define_slist(texture_node, const char *name; texture_ref ref; int unit);

/* material. 
 *
 * a material collects all its textures into one list.
 * this is to emphasize that the material itself has no way to enforce a
 * specific use a any texture: this is controlled by the shader requesting it.
 * 
 * the texture list caches the names of the stored textures for faster
 * searching time. note that these may beome stale as you replace textures.
 *
 */
struct material {
	char *name;
	vec4f k_amb, k_diff, k_spec;
	struct texture_node *textures;
	// blend stuff?
	void *aux;
};

#define TYPE material
#define ARRAY materials
#define REF material_ref
#include <libcgl/mm.h>

material_ref make_material(char *name, vec4f *amb, vec4f *diff, vec4f *spec) {
	material_ref ref = allocate_ref();
	struct material *mat = materials+ref.id;

	mat->k_amb = *amb;
	mat->k_diff = *diff;
	mat->k_spec = *spec;

	mat->textures = 0;
	mat->aux = 0;
	return ref;
}
