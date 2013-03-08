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

		float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
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
		float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
        uint c = atomicCounterIncrement(counter);
		out_col = vec4(diffuse_color.rgb * light_col * n_dot_l, diffuse_color.a);
        imageAtomicAdd(mutex_buffer, ivec2(gl_FragCoord.xy), 1u);
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "proj" "view" "model" "hemi_dir" "light_col" "diffuse_color" "mutex_buffer" "per_frag_array")>


#<make-shader "diffuse-hemi/collect"
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
    coherent uniform layout(rgba8) image2D per_frag_colors;
    coherent uniform layout(r32f) image2D per_frag_depths;
	uniform sampler2D opaque_depth;
    uniform ivec2 wh;
    uniform int array_layers;
	void main() {
		if (texture(opaque_depth, gl_FragCoord.xy/vec2(wh)).r <= gl_FragCoord.z)
			discard;
        uint c = atomicCounterIncrement(counter);

        uint index = imageAtomicAdd(mutex_buffer, ivec2(gl_FragCoord.xy), 1u);
        if (index >= array_layers) discard;
        ivec2 target = ivec2(gl_FragCoord.xy) + ivec2(0,index*int(wh.y));

        float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
		vec4 result = vec4(diffuse_color.rgb * light_col * n_dot_l, diffuse_color.a);

        imageStore(per_frag_colors, target, result);
        imageStore(per_frag_depths, target, vec4(gl_FragCoord.z,0,0,0)); //vec4(.8-(float(index)*0.1),0,0,0));

        out_col = vec4(c);
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "proj" "view" "model" "hemi_dir" "light_col" "diffuse_color" "mutex_buffer" "per_frag_colors" "per_frag_depths" "wh" "array_layers" "opaque_depth")>


#<make-shader "diffuse-hemi+tex/collect"
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
		vec4 pos_proj = proj * view * pos_wc;
		tc = in_tc;
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
	in vec2 tc;
    layout(binding = 0, offset = 0) uniform atomic_uint counter;
    coherent uniform layout(size1x32) uimage2D mutex_buffer;
    coherent uniform layout(rgba8) image2D per_frag_colors;
    coherent uniform layout(r32f) image2D per_frag_depths;
	uniform sampler2D tex0;
	uniform sampler2D opaque_depth;
    uniform ivec2 wh;
    uniform int array_layers;
	void main() {
		if (texture(opaque_depth, gl_FragCoord.xy/vec2(wh)).r <= gl_FragCoord.z)
			discard;
        uint c = atomicCounterIncrement(counter);

        uint index = imageAtomicAdd(mutex_buffer, ivec2(gl_FragCoord.xy), 1u);
        if (index >= array_layers) discard;
        ivec2 target = ivec2(gl_FragCoord.xy) + ivec2(0,index*int(wh.y));

		float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
		vec4 color = texture(tex0, tc);
		vec4 result = vec4(color.rgb * diffuse_color.rgb * light_col * n_dot_l, color.a * diffuse_color.a);

        imageStore(per_frag_colors, target, result);
        imageStore(per_frag_depths, target, vec4(gl_FragCoord.z,0,0,0)); //vec4(.8-(float(index)*0.1),0,0,0));

        out_col = vec4(c);
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "proj" "view" "model" "hemi_dir" "light_col" "diffuse_color" "mutex_buffer" "per_frag_colors" "per_frag_depths" "wh" "array_layers" "opaque_depth" "tex0")>



#<make-shader "texquad/apply-array"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	void main() {
		gl_Position = vec4(in_pos.xy, .9,1);
	}
}
#:fragment-shader #{
#version 420 core
#extension GL_NV_gpu_shader5 : enable
	out vec4 out_col;
    coherent uniform layout(size1x32) uimage2D mutex_buffer;
    coherent uniform layout(rgba8) image2D per_frag_colors;
    coherent uniform layout(r32f) image2D per_frag_depths;
	uniform sampler2D tex0;
    uniform ivec2 wh;
    uniform int array_layers;
    float depth(int i) {
        ivec2 index = ivec2(gl_FragCoord.xy) + ivec2(0,i*int(wh.y));
        return imageLoad(per_frag_depths, index).r;
    }
    void set_depth(int i, float d) {
        ivec2 index = ivec2(gl_FragCoord.xy) + ivec2(0,i*int(wh.y));
        imageStore(per_frag_depths, index, vec4(d,0,0,0));
    }
    vec4 color(int i) {
        ivec2 index = ivec2(gl_FragCoord.xy) + ivec2(0,i*int(wh.y));
        return imageLoad(per_frag_colors, index);
    }
    void set_color(int i, vec4 c) {
        ivec2 index = ivec2(gl_FragCoord.xy) + ivec2(0,i*int(wh.y));
        imageStore(per_frag_colors, index, c);
    }
    void exchange_color(int i, int j) {
        ivec2 index_i = ivec2(gl_FragCoord.xy) + ivec2(0,i*int(wh.y));
        ivec2 index_j = ivec2(gl_FragCoord.xy) + ivec2(0,j*int(wh.y));
        vec4 I = imageLoad(per_frag_colors, index_i);
        vec4 J = imageLoad(per_frag_colors, index_j);
        imageStore(per_frag_colors, index_i, J);
        imageStore(per_frag_colors, index_j, I);
    }
	void main() {
        uint array_len = imageLoad(mutex_buffer, ivec2(gl_FragCoord.xy)).r;
        if (array_len >= array_layers) array_len = array_layers - 1;

        float depth_vals[16];   // this still has to be set manually.
		int16_t color_v[16];
        for (int i = 0; i < int(array_len); ++i) {	// looping unto array_layers is death!
        	depth_vals[i] = depth(i);
			color_v[i] = int16_t(i);
		}
        
		if (array_len > 0)
	    for (int i = 0; i < array_len-1; ++i) {
	           float d = depth_vals[i];
	           int swap_id = i;
	           for (int j = i; j < array_len; ++j) {
	               float dj = depth_vals[j];
	               if (dj < d) {
	                   d = dj;
	                   swap_id = j;
	               }
	           }
	           if (swap_id != i) {
	           	depth_vals[swap_id] = depth_vals[i];
	           	depth_vals[i] = d;
				// vec4 tmp = color_vals[i];
				// color_vals[i] = color_vals[swap_id];
				// color_vals[swap_id] = tmp;
				int16_t tmp = color_v[i];
				color_v[i] = color_v[swap_id];
				color_v[swap_id] = tmp;
	           }
	        }

		/*	bubble sort version
        if (array_len > 0)
		for (int i = (int(array_len) - 2); i >= 0; --i) {
			for (int j = 0; j <= i; ++j) {
				if (depth_vals[j] > depth_vals[j+1]) {
					float temp = depth_vals[j+1];
					depth_vals[j+1] = depth_vals[j];
					depth_vals[j] = temp;
					int16_t tmp = color_v[j];
					color_v[j] = color_v[j+1];
					color_v[j+1] = tmp;
				}
			}
		}
		*/
	
		vec4 base = vec4(texture(tex0, gl_FragCoord.xy/vec2(wh)).rgb, 1);
		if (array_len > 0)
			for (int i = int (array_len)-1; i >= 0; --i) {
				vec4 src = color(int(color_v[i]));
				base = src.rgba * src.a + base.rgba * (1-src.a);
			}

		out_col = base;
	}
}
#:inputs (list "in_pos")
#:uniforms (list "mutex_buffer" "per_frag_colors" "per_frag_depths" "wh" "array_layers" "tex0")>



#<make-shader "texquad/clear-array"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	void main() {
		gl_Position = vec4(in_pos.xy, .9,1);
	}
}
#:fragment-shader #{
#version 420 core
    coherent uniform layout(size1x32) uimage2D mutex_buffer;
	void main() {
        imageStore(mutex_buffer, ivec2(gl_FragCoord.xy), uvec4(0,0,0,0));
	}
}
#:inputs (list "in_pos")
#:uniforms (list "mutex_buffer")>



#<make-shader "texquad/clear-color"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	void main() {
		gl_Position = vec4(in_pos.xy, .9,1);
	}
}
#:fragment-shader #{
#version 420 core
    coherent uniform layout(rgba8) image2D per_frag_colors;
    coherent uniform layout(size1x32) image2D per_frag_depths;
    uniform ivec2 wh;
	void main() {
        for (int i = 0; i < 20; ++i) {
            imageStore(per_frag_colors, ivec2(gl_FragCoord.xy)+ivec2(0,i*wh.y), vec4(0,0,float(i)/20.0,1));
            imageStore(per_frag_depths, ivec2(gl_FragCoord.xy)+ivec2(0,i*wh.y), vec4(1,0,0,0));
        }
	}
}
#:inputs (list "in_pos")
#:uniforms (list "per_frag_colors" "per_frag_depths" "wh")>



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



#<make-shader "texquad-with-depth"
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
	uniform sampler2D tex1;
	in vec2 tc;
    out vec4 out_col;
	void main() {
		out_col = texture(tex0, tc);
		gl_FragDepth = texture(tex1, tc).r;
	}
}
#:inputs (list "in_pos" "in_tc")
#:uniforms (list "tex0" "tex1")>



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
        out_col = vec4(tc.x,tc.y,0,1); //imageLoad(mutex_buffer, ivec2(0,0)).r,1);
        unsigned int array_len = imageLoad(mutex_buffer, ivec2(gl_FragCoord.xy)).r;
        out_col = vec4(float(array_len) / 10.0, 0, 0, 1);

        if (array_len == 0) out_col.xyz = vec3(0,0,0);
        if (array_len == 1) out_col.xyz = vec3(1,0,0);
        if (array_len == 2) out_col.xyz = vec3(0,1,0);
        if (array_len == 3) out_col.xyz = vec3(0,0,1);
        if (array_len == 4) out_col.xyz = vec3(1,1,0);
        if (array_len == 5) out_col.xyz = vec3(0,1,1);
        if (array_len == 6) out_col.xyz = vec3(1,0,1);
        if (array_len >  6) out_col.xyz = vec3(1,1,1);
    }
}
#:inputs (list "in_pos" "in_tc")
#:uniforms (list "mutex_buffer")>


#<make-shader "quad/clear-mutex-buffer"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	void main() {
		gl_Position = vec4(in_pos.xy, .9999,1);
	}
}
#:fragment-shader #{
#version 420 core
	out vec4 out_col;
	in vec2 tc;
    layout(binding = 0, offset = 0) uniform atomic_uint counter;
    coherent uniform layout(size1x32) uimage2D mutex_buffer;
	void main() {
        imageStore(mutex_buffer, ivec2(gl_FragCoord.xy), uvec4(0,0,0,0));
	}
}
#:inputs (list "in_pos")
#:uniforms (list "mutex_buffer")>


#<make-shader "highlight object"
#:vertex-shader #{
#version 420 core
	in vec3 in_pos;
	uniform mat4 proj;
	uniform mat4 view;
	uniform mat4 model;
	out vec4 pos_wc;
	void main() {
		pos_wc = model * vec4(in_pos, 1.0);
		gl_Position = proj * view * pos_wc;
	}
}
#:geometry-shader #{
#version 420 core
	layout(triangles) in;
	layout(triangle_strip, max_vertices=12) out;
	uniform vec2 screenres;
	void make_line(vec4 a, vec4 b) {
		vec2 d = 3 * (1.0/screenres);
		vec2 p = a.xy, q = b.xy;
		vec2 p_to_q = q - p;
		vec2 left = vec2(-p_to_q.y, p_to_q.x) * d;
		vec2 right = vec2(p_to_q.y, -p_to_q.x) * d;
		gl_Position = a + vec4(left, 0, 0); EmitVertex();
		gl_Position = b + vec4(left, 0, 0); EmitVertex();
		gl_Position = a + vec4(right, 0, 0); EmitVertex();
		gl_Position = b + vec4(right, 0, 0); EmitVertex();
		EndPrimitive();
	}
	void main() {
		make_line(gl_in[0].gl_Position, gl_in[1].gl_Position);
		make_line(gl_in[1].gl_Position, gl_in[2].gl_Position);
		make_line(gl_in[2].gl_Position, gl_in[0].gl_Position);
	}
}
#:fragment-shader #{
#version 420 core
	out vec4 out_col;
	void main() {
		out_col = vec4(1,0,0,1);
	}
}
#:inputs (list "in_pos")
#:uniforms (list "proj" "view" "model" "screenres")>

