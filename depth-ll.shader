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
    coherent uniform layout(rgba8) image2D per_frag_colors;
    coherent uniform layout(r32f) image2D per_frag_depths;
    coherent uniform layout(size1x32) iimage2D tail_buffer;
    coherent uniform layout(size1x32) iimage2D head_buffer;
	uniform sampler2D opaque_depth;
    uniform ivec2 wh;
    uniform int array_layers;

	void main() {
		if (texture(opaque_depth, gl_FragCoord.xy/vec2(wh)).r <= gl_FragCoord.z)
			discard;
        ivec2 coord = ivec2(gl_FragCoord.xy);

        int pos = int(atomicCounterIncrement(counter));
        int old = imageAtomicExchange(head_buffer, coord, pos);

        float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
		vec4 result = vec4(diffuse_color.rgb * light_col * n_dot_l, diffuse_color.a);

        ivec2 pos_c = ivec2(pos % wh.x, pos / wh.x);
        imageStore(tail_buffer, pos_c, ivec4(old,0,0,0));
        imageStore(per_frag_colors, pos_c, result);
        imageStore(per_frag_depths, pos_c, vec4(gl_FragCoord.z,0,0,0));
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "proj" "view" "model" "hemi_dir" "light_col" "diffuse_color" "per_frag_colors" "per_frag_depths" "wh" "opaque_depth" "head_buffer" "tail_buffer")>

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
    coherent uniform layout(rgba8) image2D per_frag_colors;
    coherent uniform layout(r32f) image2D per_frag_depths;
    coherent uniform layout(size1x32) iimage2D tail_buffer;
    coherent uniform layout(size1x32) iimage2D head_buffer;
	uniform sampler2D tex0;
	uniform sampler2D opaque_depth;
    uniform ivec2 wh;
    uniform int array_layers;
	void main() {
		if (texture(opaque_depth, gl_FragCoord.xy/vec2(wh)).r <= gl_FragCoord.z)
			discard;
        ivec2 coord = ivec2(gl_FragCoord.xy);

        int pos = int(atomicCounterIncrement(counter));
        int old = imageAtomicExchange(head_buffer, coord, pos);

		float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
		vec4 color = texture(tex0, tc);
		vec4 result = vec4(color.rgb * diffuse_color.rgb * light_col * n_dot_l, color.a * diffuse_color.a);

        ivec2 pos_c = ivec2(pos % wh.x, pos / wh.x);
        imageStore(tail_buffer, pos_c, ivec4(old,0,0,0));
        imageStore(per_frag_colors, pos_c, result);
        imageStore(per_frag_depths, pos_c, vec4(gl_FragCoord.z,0,0,0));
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "proj" "view" "model" "hemi_dir" "light_col" "diffuse_color" "per_frag_colors" "per_frag_depths" "wh" "opaque_depth" "tex0" "head_buffer" "tail_buffer")>





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
    coherent uniform layout(size1x32) iimage2D head_buffer;
    coherent uniform layout(size1x32) iimage2D tail_buffer;
	uniform sampler2D tex0;
    uniform ivec2 wh;
    uniform int array_layers;
	
	void main() {

        float depth_vals[16];   // this still has to be set manually.
		vec4 color_vals[16];
        
        int run = imageLoad(head_buffer, ivec2(gl_FragCoord.xy)).r;
        if (run >= 0) {
            int array_len = 0;
		    while (run >= 0 && array_len < 16) {
                ivec2 pos_c = ivec2(run % wh.x, run / wh.x);
                depth_vals[array_len] = imageLoad(per_frag_depths, pos_c).r;
                color_vals[array_len] = imageLoad(per_frag_colors, pos_c);
    	    	run = imageLoad(tail_buffer, pos_c).r;
                ++array_len;
            }

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
		    		vec4 tmp = color_vals[i];
		    		color_vals[i] = color_vals[swap_id];
		    		color_vals[swap_id] = tmp;
	            }
	        }
		
			vec4 base = vec4(texture(tex0, gl_FragCoord.xy/vec2(wh)).rgb, 1);
			for (int i = int (array_len)-1; i >= 0; --i) {
				vec4 src = color_vals[i];
				base = src.rgba * src.a + base.rgba * (1-src.a);
			}

			out_col = base;
		}
		else 
			out_col = vec4(texture(tex0, gl_FragCoord.xy/vec2(wh)).rgb, 1);
	}
}
#:inputs (list "in_pos")
#:uniforms (list "mutex_buffer" "per_frag_colors" "per_frag_depths" "wh" "tex0" "head_buffer" "tail_buffer")>



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
    coherent uniform layout(size1x32) iimage2D head_buffer;
    coherent uniform layout(size1x32) iimage2D tail_buffer;
	uniform ivec2 wh;
	void main() {
        imageStore(mutex_buffer, ivec2(gl_FragCoord.xy), uvec4(0,0,0,0));
        imageStore(head_buffer, ivec2(gl_FragCoord.xy), ivec4(-1,0,0,0));
		for (int i = 0; i < 4; ++i)
			imageStore(tail_buffer, ivec2(gl_FragCoord.xy) + ivec2(0,wh.y*i), ivec4(-1,0,0,0));
	}
}
#:inputs (list "in_pos")
#:uniforms (list "mutex_buffer" "head_buffer" "tail_buffer" "wh")>



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
        imageStore(per_frag_colors, ivec2(gl_FragCoord.xy), vec4(0,0,0,0));
        imageStore(per_frag_depths, ivec2(gl_FragCoord.xy), vec4(1,0,0,0));
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



