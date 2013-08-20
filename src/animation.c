#include "animation.h"

#include <sys/time.h>
#include <stdio.h>

static void print_matrix(const matrix4x4f *m, int d) {
	for (int i = 0; i < d; ++i) printf(" ");	printf("%6.6f %6.6f %6.6f %6.6f\n", m->col_major[0], m->col_major[4], m->col_major[8], m->col_major[12]);
	for (int i = 0; i < d; ++i) printf(" ");	printf("%6.6f %6.6f %6.6f %6.6f\n", m->col_major[1], m->col_major[5], m->col_major[9], m->col_major[13]);
	for (int i = 0; i < d; ++i) printf(" ");	printf("%6.6f %6.6f %6.6f %6.6f\n", m->col_major[2], m->col_major[6], m->col_major[10], m->col_major[14]);
	for (int i = 0; i < d; ++i) printf(" ");	printf("%6.6f %6.6f %6.6f %6.6f\n", m->col_major[3], m->col_major[7], m->col_major[11], m->col_major[15]);
}



void make_quaternionf(quaternionf *q, const vec3f *v, float w) {
	q->v.x = v->x;
	q->v.y = v->y;
	q->v.z = v->z;
	q->w = w;
}

void make_quaternion4f(quaternionf *q, float x, float y, float z, float w) {
	q->v.x = x;
	q->v.y = y;
	q->v.z = z;
	q->w = w;
}

void copy_quaternion4f(quaternionf *to, const quaternionf *from) {
	to->v.x = from->v.x;
	to->v.y = from->v.y;
	to->v.z = from->v.z;
	to->w = from->w;
}

//! t \in [0,1]
void lerp_vec3f(vec3f *to, const vec3f *a, const vec3f *b, float t) {
	sub_components_vec3f(to, b, a);
	mul_vec3f_by_scalar(to, to, t);
	add_components_vec3f(to, a, to);
}

float dot_quaternionf(const quaternionf *a, const quaternionf *b) {
	return dot_vec3f(&a->v, &b->v) + a->w*b->w;
}

void add_components_quaternionf(quaternionf *to, const quaternionf *a, const quaternionf *b) {
	add_components_vec3f(&to->v, &a->v, &b->v);
	to->w = a->w + b->w;
}

void mul_components_quaternionf(quaternionf *to, const quaternionf *a, const quaternionf *b) {
	mul_components_vec3f(&to->v, &a->v, &b->v);
	to->w = a->w * b->w;
}

void sub_components_quaternionf(quaternionf *to, const quaternionf *a, const quaternionf *b) {
	sub_components_vec3f(&to->v, &a->v, &b->v);
	to->w = a->w - b->w;
}

void mul_quaternionf_by_scalar(quaternionf *to, const quaternionf *a, float s) {
	mul_vec3f_by_scalar(&to->v, &a->v, s);
	to->w = a->w * s;
}

//! not what you would usually want to do
void lerp_quaterionf(quaternionf *to, const quaternionf *a, const quaternionf *b, float t) {
	sub_components_quaternionf(to, b, a);
	mul_quaternionf_by_scalar(to, to, t);
	add_components_quaternionf(to, a, to);
}


//! t \in [0,1]
void slerp_quaterionf(quaternionf *to, const quaternionf *p, const quaternionf *q, float t) {
	quaternionf tmp_p, tmp_q;
	float cos_phi = dot_quaternionf(p, q);
	// printf("quaternion dot of %6.6f %6.6f %6.6f %6.6f\n", p->v.x, p->v.y, p->v.z, p->w);
	// printf("            with  %6.6f %6.6f %6.6f %6.6f\n", q->v.x, q->v.y, q->v.z, q->w);
	// printf("    ----------->  %6.6f.\n", cos_phi);
	// this breaks: (0,0,-1) -> (-1,0,0) vs (0,0,-1) -> (-1,0,0.1)
	// it's not in http://number-none.com/product/Understanding%20Slerp,%20Then%20Not%20Using%20It/, either.
// 	if (cos_phi < 0) {	
// 		tmp_q.v.x = -q->v.x;
// 		tmp_q.v.y = -q->v.y;
// 		tmp_q.v.z = -q->v.z;
// 		tmp_q.w = -q->w;
// 		cos_phi = -cos_phi;
// 	}
// 	else
		copy_quaternion4f(&tmp_q, q);

	if (cos_phi >= 0.999999) {
		lerp_quaterionf(to, p, q, t);
	}
	else {
		float phi = acosf(cos_phi);
		float sin_phi = sinf(phi);
		float w_0 = sin((1-t) * phi) / sin_phi;
		float w_1 = sin(t*phi) / sin_phi;
		mul_quaternionf_by_scalar(&tmp_p, p, w_0);
		mul_quaternionf_by_scalar(&tmp_q, &tmp_q, w_1);
		add_components_quaternionf(to, &tmp_p, &tmp_q);
	}
}

animation_time_t animation_time_stamp() {
	double stamp;
	struct timeval tv;
	gettimeofday(&tv, 0);
	stamp = tv.tv_sec * 1000.0 + tv.tv_usec / 1000.0;
	return stamp;
}

//! t \in [0,1]
void hermite_interpolation(vec3f *to, vec3f *p_minus_1, vec3f *p0, vec3f *p1, vec3f *p_plus_2, float t) {
	float t2 = t*t;
	float t3 = t2*t;
	float h00 = 2*t3 - 3*t2 + 1;
	float h10 = -2*t3 + 3*t2;
	float h01 = t3 - 2*t2 + t;
	float h11 = t3 - t2;
	
	vec3f tmp;

	// tangents
	vec3f m0, m1;
	// m0
	sub_components_vec3f(&tmp, p0, p_minus_1);
	sub_components_vec3f(&m0, p1, p0);
	add_components_vec3f(&m0, &m0, &tmp);
	div_vec3f_by_scalar(&m0, &m0, 2);
	// m1
	sub_components_vec3f(&tmp, p_plus_2, p1);
	sub_components_vec3f(&m1, p1, p0);
	add_components_vec3f(&m1, &m1, &tmp);
	div_vec3f_by_scalar(&m1, &m1, 2);
	
	// combination
	mul_vec3f_by_scalar(to, p0, h00);
	
	mul_vec3f_by_scalar(&tmp, p1, h10);
	add_components_vec3f(to, to, &tmp);

	mul_vec3f_by_scalar(&m0, &m0, h01);
	add_components_vec3f(to, to, &m0);

	mul_vec3f_by_scalar(&m1, &m1, h11);
	add_components_vec3f(to, to, &m1);
}

