;; (define-material-shader just-color ((uniform vec4 color))
;;   color)
;; (define-material-shader texture ((uniform vec4 color) (uniform sampler2D tex) (uniform vec2 tc))
;;   (texture tex tc))
;; 
;; (define-light-shader dirlight ((uniform vec3 light_dir) (uniform vec3 light_col) (uniform vec3 normal))
;;   (decl ((float ndotl (dot normal (- light_dir))))
;;     (* light_col ndotl)))
;;   
;; (define-shader my-phong
;;   ...
;;   :frag-uniforms ((vec2 tc))
;;   :frag-shader 
;;     (function main () -> void
;;       (decl ((vec3 mat-color (texture :tc tc))
;; 			 (vec3 light-color (dirlight)))
;; 	    (set outcolr (* mat-color light-color))))
;;   ...)

(format #t "-------- reading cgls default shaders ---------~%")

#<make-shader "diffuse-dl"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	in vec3 in_norm;
	uniform mat4 proj;
	uniform mat4 view;
	uniform mat4 model;
	out vec4 pos_wc;
	out vec3 norm_wc;
	void main() {
		pos_wc = model * vec4(in_pos, 1.0);
		norm_wc = in_norm;
		gl_Position = proj * view * pos_wc;
	}
}
#:fragment-shader #{
#version 150 core
	out vec4 out_col;
	uniform vec3 light_dir;
	uniform vec3 light_col;
	uniform vec4 diffuse_color;
	in vec4 pos_wc;
	in vec3 norm_wc;
	void main() {
		out_col = vec4(0.,0.,0.,1.);

		float n_dot_l = max(0, dot(norm_wc, -light_dir));
		out_col += vec4(diffuse_color.rgb * light_col * n_dot_l, 0.);
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "proj" "view" "model" "light_dir" "light_col" "diffuse_color")>


#<make-shader "diffuse-dl+tex"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	in vec3 in_norm;
	in vec2 in_tc;
	uniform mat4 proj;
	uniform mat4 view;
	uniform mat4 model;
	out vec4 pos_wc;
	out vec3 norm_wc;
	out vec2 tc;
	void main() {
		pos_wc = model * vec4(in_pos, 1.0);
		norm_wc = in_norm;
		gl_Position = proj * view * pos_wc;
		tc = in_tc;
	}
}
#:fragment-shader #{
#version 150 core
	out vec4 out_col;
	uniform vec3 light_dir;
	uniform vec3 light_col;
	uniform sampler2D tex0;
	in vec4 pos_wc;
	in vec3 norm_wc;
	in vec2 tc;
	void main() {
		out_col = vec4(0.,0.,0.,1.);

		float n_dot_l = max(0, dot(norm_wc, -light_dir));
		vec3 color = texture(tex0, tc).rgb;
		out_col += vec4(color * light_col * n_dot_l, 0.);
	}
}
#:inputs (list "in_pos" "in_norm" "in_tc")
#:uniforms (list "proj" "view" "model" "light_dir" "light_col" "tex0")>


#<make-shader "diffuse-hemi"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	in vec3 in_norm;
	uniform mat4 proj;
	uniform mat4 view;
	uniform mat4 model;
	out vec4 pos_wc;
	out vec3 norm_wc;
	void main() {
		pos_wc = model * vec4(in_pos, 1.0);
		norm_wc = in_norm;
		gl_Position = proj * view * pos_wc;
	}
}
#:fragment-shader #{
#version 150 core
	out vec4 out_col;
	uniform vec3 hemi_dir;
	uniform vec3 light_col;
	uniform vec4 diffuse_color;
	in vec4 pos_wc;
	in vec3 norm_wc;
	void main() {
		out_col = vec4(0.,0.,0.,1.);

		float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
		out_col += vec4(diffuse_color.rgb * light_col * n_dot_l, 0.);
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "proj" "view" "model" "hemi_dir" "light_col" "diffuse_color")>


#<make-shader "diffuse-hemi+tex"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	in vec3 in_norm;
	in vec2 in_tc;
	uniform mat4 proj;
	uniform mat4 view;
	uniform mat4 model;
	out vec4 pos_wc;
	out vec3 norm_wc;
	out vec2 tc;
	void main() {
		pos_wc = model * vec4(in_pos, 1.0);
		norm_wc = in_norm;
		gl_Position = proj * view * pos_wc;
		tc = in_tc;
	}
}
#:fragment-shader #{
#version 150 core
	out vec4 out_col;
	uniform vec3 hemi_dir;
	uniform vec3 light_col;
	uniform sampler2D tex0;
	in vec4 pos_wc;
	in vec3 norm_wc;
	in vec2 tc;
	void main() {
		out_col = vec4(0.,0.,0.,1.);

		float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
		vec3 color = texture(tex0, tc).rgb;
		out_col += vec4(color * light_col * n_dot_l, 0.);
	}
}
#:inputs (list "in_pos" "in_norm" "in_tc")
#:uniforms (list "proj" "view" "model" "hemi_dir" "light_col" "tex0")>



#<make-shader "diffuse-hemi/dp"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	in vec3 in_norm;
	uniform mat4 proj;
	uniform mat4 view;
	uniform mat4 model;
	out vec4 pos_wc;
	out vec3 norm_wc;
	out vec2 tc;
	void main() {
		pos_wc = model * vec4(in_pos, 1.0);
		norm_wc = in_norm;
		vec4 pos_proj = proj * view * pos_wc;
        gl_Position = pos_proj;
		tc = pos_proj.xy/pos_proj.w * 0.5 + 0.5;
	}
}
#:fragment-shader #{
#version 150 core
	out vec4 out_col;
	uniform vec3 hemi_dir;
	uniform vec3 light_col;
	uniform sampler2D depth;
	uniform vec4 diffuse_color;
	in vec4 pos_wc;
	in vec3 norm_wc;
	in vec2 tc;
	void main() {
        float min_depth = texture(depth, tc).r;
        float frag_depth = gl_FragCoord.z;
        if (frag_depth <= min_depth)
            discard;

		float n_dot_l = max(0, 0.5*(1+dot(-norm_wc, hemi_dir)));
		out_col = vec4(diffuse_color.rgb * light_col * n_dot_l, diffuse_color.a);
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "proj" "view" "model" "hemi_dir" "light_col" "depth" "diffuse_color")>


#<make-shader "diffuse-hemi+tex/dp"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	in vec3 in_norm;
	in vec2 in_tc;
	uniform mat4 proj;
	uniform mat4 view;
	uniform mat4 model;
	out vec4 pos_wc;
	out vec3 norm_wc;
	out vec2 tc;
	out vec2 tc2;
	void main() {
		pos_wc = model * vec4(in_pos, 1.0);
		norm_wc = in_norm;
		vec4 pos_proj = proj * view * pos_wc;
        gl_Position = pos_proj;
		tc = in_tc;
		tc2 = pos_proj.xy/pos_proj.w * 0.5 + 0.5;
	}
}
#:fragment-shader #{
#version 150 core
	out vec4 out_col;
	uniform vec3 hemi_dir;
	uniform vec3 light_col;
	uniform sampler2D tex0;
	uniform sampler2D depth;
    uniform vec4 diffuse_color;
	in vec4 pos_wc;
	in vec3 norm_wc;
	in vec2 tc;
	in vec2 tc2;
	void main() {
        float min_depth = texture(depth, tc2).r;
        float frag_depth = gl_FragCoord.z;
        if (frag_depth <= min_depth)
            discard;

		float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
		vec4 color = texture(tex0, tc);
		out_col = vec4(color.rgb * diffuse_color.rgb * light_col * n_dot_l, color.a * diffuse_color.a);
	}
}
#:inputs (list "in_pos" "in_norm" "in_tc")
#:uniforms (list "proj" "view" "model" "hemi_dir" "light_col" "tex0" "depth" "diffuse_color")>



#<make-shader "diffuse-hemi/frag-arrays"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	in vec3 in_norm;
	uniform mat4 proj;
	uniform mat4 view;
	uniform mat4 model;
	out vec4 pos_wc;
	out vec3 norm_wc;
	void main() {
		pos_wc = model * vec4(in_pos, 1.0);
		norm_wc = in_norm;
		vec4 pos_proj = proj * view * pos_wc;
        gl_Position = pos_proj;
	}
}
#:fragment-shader #{
#version 420 core
	out vec4 out_col;
	uniform vec3 hemi_dir;
	uniform vec3 light_col;
	uniform vec4 diffuse_color;
	in vec4 pos_wc;
	in vec3 norm_wc;
    layout(binding = 0, offset = 0) uniform atomic_uint counter;
    coherent uniform layout(size1x32) uimage2D mutex_buffer;
    coherent uniform layout(size4x32) image2D per_frag_array;
	void main() {
		float n_dot_l = max(0, 0.5*(1+dot(-norm_wc, hemi_dir)));
        uint c = atomicCounterIncrement(counter);
		out_col = vec4(diffuse_color.rgb * light_col * n_dot_l, diffuse_color.a);
//         out_col.r += 0.0000001 * float(c);
        imageAtomicAdd(mutex_buffer, ivec2(gl_FragCoord.xy), 1u);
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "proj" "view" "model" "hemi_dir" "light_col" "diffuse_color" "mutex_buffer" "per_frag_array")>


#<make-shader "textured-diffuse-pl"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	in vec3 in_norm;
	in vec2 in_tc;
	uniform mat4 proj;
	uniform mat4 view;
	uniform mat4 model;
	out vec4 pos_wc;
	out vec3 norm_wc;
	out vec2 tc;
	void main() {
		pos_wc = model * vec4(in_pos, 1.0);
		norm_wc = in_norm;
		tc = in_tc;
		gl_Position = proj * view * pos_wc;
	}
}
#:fragment-shader #{
#version 150 core
	out vec4 out_col;
	uniform sampler2D diffuse_tex;
	uniform vec3 light_dir;
	uniform vec3 light_col;
	in vec4 pos_wc;
	in vec3 norm_wc;
	in vec2 tc;
	void main() {
		out_col = vec4(0.,0.,0.,1.);
		vec3 color = texture(diffuse_tex, tc.st).rgb;

		float n_dot_l = max(0, dot(norm_wc, -light_dir));
		out_col += vec4(color * light_col * n_dot_l, 0.);
	}
}
#:inputs (list "in_pos" "in_norm" "in_tc")>





#<make-shader "texquad"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	in vec2 in_tc;
	out vec2 tc;
	void main() {
		gl_Position = vec4(in_pos.xy, .5,1);
		tc = in_tc;
	}
}
#:fragment-shader #{
#version 150 core
	out vec4 out_col;
	uniform sampler2D tex0;
	in vec2 tc;
	void main() {
		out_col = texture(tex0, tc);
	}
}
#:inputs (list "in_pos" "in_tc")
#:uniforms (list "tex0")>



#<make-shader "copy-depth"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	in vec2 in_tc;
	out vec2 tc;
	void main() {
		gl_Position = vec4(in_pos.xy, .5,1);
		tc = in_tc;
	}
}
#:fragment-shader #{
#version 150 core
	uniform sampler2D tex0;
	in vec2 tc;
	void main() {
		gl_FragDepth = texture(tex0, tc).r;
	}
}
#:inputs (list "in_pos" "in_tc")
#:uniforms (list "tex0")>



#<make-shader "quad/atomic"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	in vec2 in_tc;
	out vec2 tc;
	void main() {
		gl_Position = vec4(in_pos.xy, .9999,1);
		tc = in_tc;
	}
}
#:fragment-shader #{
#version 420 core
	out vec4 out_col;
	in vec2 tc;
    layout(binding = 0, offset = 0) uniform atomic_uint counter;
	void main() {
        uint c = atomicCounterIncrement(counter);
        float r = (c/1024) / 1024.f;
        out_col = vec4(r, 0, 0, 1);
		out_col = vec4(tc.x,tc.y,0,1);
	}
}
#:inputs (list "in_pos" "in_tc")
#:uniforms (list )>



#<make-shader "quad/mutex"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	in vec2 in_tc;
	out vec2 tc;
	void main() {
		gl_Position = vec4(in_pos.xy, .9999,1);
		tc = in_tc;
	}
}
#:fragment-shader #{
#version 420 core
	out vec4 out_col;
	in vec2 tc;
    layout(binding = 0, offset = 0) uniform atomic_uint counter;
    coherent uniform layout(size1x32) uimage2D mutex_buffer;
	void main() {
        uint c = atomicCounterIncrement(counter);
        imageStore(mutex_buffer, ivec2(0,0), uvec4(c,0,0,0));
        float r = (c/1024) / 1024.f;
        out_col = vec4(r, 0, 0, 1);
		out_col = vec4(tc.x,tc.y,imageLoad(mutex_buffer, ivec2(0,0)).r,1);
		out_col = vec4(0,0,(imageLoad(mutex_buffer, ivec2(0,0)).r/256)/10000.0f,1);
	}
}
#:inputs (list "in_pos" "in_tc")
#:uniforms (list "mutex_buffer")>



#<make-shader "quad/show-frag-array-len"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	in vec2 in_tc;
	out vec2 tc;
	void main() {
		gl_Position = vec4(in_pos.xy, .9999,1);
		tc = in_tc;
	}
}
#:fragment-shader #{
#version 420 core
	out vec4 out_col;
	in vec2 tc;
    layout(binding = 0, offset = 0) uniform atomic_uint counter;
    coherent uniform layout(size1x32) uimage2D mutex_buffer;
	void main() {
//         uint c = atomicCounterIncrement(counter);
//         imageStore(mutex_buffer, ivec2(0,0), uvec4(c,0,0,0));
//         float r = (c/1024) / 1024.f;
//         out_col = vec4(r, 0, 0, 1);
		out_col = vec4(tc.x,tc.y,0,1); //imageLoad(mutex_buffer, ivec2(0,0)).r,1);
        unsigned int array_len = imageLoad(mutex_buffer, ivec2(gl_FragCoord.xy)).r;
        out_col = vec4(float(array_len) / 10.0, 0, 0, 1);
//         imageStore(mutex_buffer, ivec2(gl_FragCoord.xy), uvec4(0,0,0,0));
// 		out_col = vec4(tc.x,tc.y,0,1); //imageLoad(mutex_buffer, ivec2(0,0)).r,1);
// 		out_col = vec4(0,0,(imageLoad(mutex_buffer, ivec2(0,0)).r/256)/10000.0f,1);
	}
}
#:inputs (list "in_pos" "in_tc")
#:uniforms (list "mutex_buffer")>



