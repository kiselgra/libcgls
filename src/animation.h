#ifndef __CGLS_ANIMATION_H__ 
#define __CGLS_ANIMATION_H__ 

#include <libmcm/vectors.h>
#include <libmcm/matrix.h>

/* general animation related stuff.
 * see
 * - skeletal.[ch]
 * - path.[ch]
 * for the actual implementations.
 */

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	vec3f v;
	float w;
} quaternionf;

void make_quaternionf(quaternionf *q, const vec3f *v, float w);
void make_quaternion4f(quaternionf *q, float x, float y, float z, float w);
void copy_quaternion4f(quaternionf *to, const quaternionf *from);
void quaternionf_to_matrix4x4f(matrix4x4f *mat, quaternionf *q);

void lerp_vec3f(vec3f *to, const vec3f *a, const vec3f *b, float t);
float dot_quaternionf(const quaternionf *a, const quaternionf *b);
void add_components_quaternionf(quaternionf *to, const quaternionf *a, const quaternionf *b);
void mul_components_quaternionf(quaternionf *to, const quaternionf *a, const quaternionf *b);
void sub_components_quaternionf(quaternionf *to, const quaternionf *a, const quaternionf *b);
void mul_quaternionf_by_scalar(quaternionf *to, const quaternionf *a, float s);
void lerp_quaterionf(quaternionf *to, const quaternionf *a, const quaternionf *b, float t);
void slerp_quaterionf(quaternionf *to, const quaternionf *p, const quaternionf *q, float t);
void slerp_quaterionf_noflip(quaternionf *to, const quaternionf *p, const quaternionf *q, float t);

void hermite_interpolation(vec3f *to, vec3f *p_minus_1, vec3f *p0, vec3f *p1, vec3f *p_plus_2, float t, float tension);

typedef double animation_time_t;
animation_time_t animation_time_stamp();

#ifdef __cplusplus
}
#endif

#endif

