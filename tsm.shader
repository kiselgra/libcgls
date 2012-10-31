
#<shader-fragment "vs:default"
#{
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
#:uniforms (list "proj" "view" "model")>

#<shader-fragment "vs/no-norm:default"
#{
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
#:uniforms (list "proj" "view" "model")>

#<shader-fragment "vs/tc:default"
#{
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
#:uniforms (list "proj" "view" "model")>



;;; gbuffer stuff

#<shader-fragment "gbuffer:v:vn" #{
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
	gl_Position = proj * view * pos_wc;
	norm_wc = in_norm;
    }
}
#:uniforms (list "proj" "view" "model")>

#<shader-fragment "gbuffer:v:vnt" #{
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
	gl_Position = proj * view * pos_wc;
	norm_wc = in_norm;
	tc = in_tc;
    }
}
#:uniforms (list "proj" "view" "model")>

#<shader-fragment "gbuffer:f:out-spec" #{
    out vec4 out_diff;
    out vec4 out_norm;
    out vec4 out_spec;
    out vec4 out_wpos;
}> 

#<shader-fragment "gbuffer:f:vn" #{
    in vec4 pos_wc;
    in vec3 norm_wc;
  ,(use "gbuffer:f:out-spec");
}>
	
#<shader-fragment "gbuffer:f:vnt" #{
    in vec4 pos_wc;
    in vec3 norm_wc;
    in vec2 tc;
    ,(use "gbuffer:f:out-spec");
}>
	  

;; actual shaders

#<make-shader "gbuffer:vn"
#:vertex-shader #{
    ,(use "gbuffer:v:vn");
}
#:fragment-shader #{
    #version 150 core
    ,(use "gbuffer:f:vn");
      uniform vec4 diffuse_color;
      uniform vec4 specular_color;
      void main() {
	  out_diff = diffuse_color;
	  out_spec = specular_color;
	  out_norm = vec4(norm_wc, 0);
	  out_wpos = pos_wc;
      }
}
#:uniforms (list "diffuse_color" "specular_color")
#:inputs (list "in_pos" "in_norm")>
	

#<make-shader "gbuffer:vnt"
#:vertex-shader #{
    ,(use "gbuffer:v:vnt");
}
#:fragment-shader #{
    #version 150 core
    ,(use "gbuffer:f:vnt");
      uniform sampler2D tex0;
      uniform vec4 specular_color;
      void main() {
	  out_diff = texture(tex0, tc);
	  out_spec = specular_color;
	  out_norm = vec4(norm_wc, 0);
	  out_wpos = pos_wc;
      }
}
#:uniforms (list "tex0" "specular_color")
#:inputs (list "in_pos" "in_norm" "in_tc")>
	
	  


;;; old stuff


#<make-shader "solid-line"
#:vertex-shader #{
#version 150 core
	,(use "vs/no-norm:default")
}
#:fragment-shader #{
#version 150 core
	out vec4 out_col;
	uniform vec4 diffuse_color;
	in vec4 pos_wc;
	void main() {
		out_col = vec4(diffuse_color.rgb,1.);
	}
}
#:inputs (list "in_pos")
#:uniforms (list "diffuse_color")>



#<make-shader "diffuse-hemi"
#:vertex-shader #{
#version 150 core
	,(use "vs:default")
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
#:uniforms (list "hemi_dir" "light_col" "diffuse_color")>

#<make-shader "diffuse-hemi+tex"
#:vertex-shader #{
#version 150 core
	,(use "vs/tc:default")
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
#:uniforms (list "hemi_dir" "light_col" "tex0")>



#<make-shader "render-shadowmap"
#:vertex-shader #{
#version 150 core
    ,(use "vs:default")
}
#:fragment-shader #{
#version 150 core
    out vec4 out_col;
    void main() {
	out_col = vec4(1,1,1,1);
    }
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list)>


#<shader-fragment "spot"
#{
    uniform vec3 spot_pos;
    uniform vec3 spot_dir;
    uniform vec3 spot_col;
    uniform float spot_cutoff;
    vec3 spot_factor() {
        vec3 to_spot = normalize(pos_wc.xyz - spot_pos);
        vec3 spot_to_pos = to_spot;
        float theta = acos(dot(spot_to_pos, normalize(spot_dir)));
        float factor = 1.0 - smoothstep(spot_cutoff*.5, spot_cutoff, theta);
        float ndotl = dot(normalize(norm_wc), -to_spot);
        return spot_col * factor * (.5+ndotl*.5);
    }
    float spot_factor_only() {
        vec3 to_spot = normalize(pos_wc.xyz - spot_pos);
        vec3 spot_to_pos = to_spot;
        float theta = acos(dot(spot_to_pos, normalize(spot_dir)));
        float factor = 1.0 - smoothstep(spot_cutoff*.5, spot_cutoff, theta);
        float ndotl = dot(normalize(norm_wc), -to_spot);
        return factor * (.5+ndotl*.5);
    }
}
#:uniforms (list "spot_pos" "spot_dir" "spot_col" "spot_cutoff")>

#<make-shader "diffuse-hemi+spot"
#:vertex-shader #{
#version 150 core
	,(use "vs:default")
}
#:fragment-shader #{
#version 150 core
    out vec4 out_col;
    uniform vec3 hemi_dir;
    uniform vec3 light_col;
    uniform vec4 diffuse_color;
    in vec4 pos_wc;
    in vec3 norm_wc;
    ,(use "spot")
    void main() {
	out_col = vec4(0.,0.,0.,1.);

	float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
	out_col += vec4(diffuse_color.rgb * light_col * n_dot_l, 0.);

        out_col.rgb += spot_factor() * diffuse_color.rgb;
     }
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "hemi_dir" "light_col" "diffuse_color")>


#<make-shader "diffuse-hemi+spot/noshadow"
#:vertex-shader #{
#version 150 core
	,(use "vs:default")
}
#:fragment-shader #{
#version 150 core
    out vec4 out_col;
    uniform vec3 hemi_dir;
    uniform vec3 light_col;
    uniform vec4 diffuse_color;
    in vec4 pos_wc;
    in vec3 norm_wc;
    ,(use "spot")
    void main() {
	out_col = vec4(0.,0.,0.,1.);

	float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
	out_col += vec4(diffuse_color.rgb * light_col * n_dot_l, 0.);

        out_col.rgb += spot_factor() * diffuse_color.rgb;
     }
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "hemi_dir" "light_col" "diffuse_color")>



#<make-shader "diffuse-hemi+spot+tex"
#:vertex-shader #{
#version 150 core
	,(use "vs/tc:default")
}
#:fragment-shader #{
#version 420 core
#extension GL_NV_gpu_shader5 : enable
    out vec4 out_col;
    uniform vec3 hemi_dir;
    uniform vec3 light_col;
    uniform sampler2D tex0;
    uniform mat4 shadow_view;
    uniform mat4 shadow_proj;
    uniform sampler2D shadow_map;
    in vec4 pos_wc;
    in vec3 norm_wc;
    in vec2 tc;
    ,(use "spot")

    void main() {
	out_col = vec4(0.,0.,0.,1.);

	float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
	vec3 color = texture(tex0, tc).rgb;
	out_col += vec4(color * light_col * n_dot_l, 0.);
        
        vec4 shadow_frag = mat4(vec4(.5,0,0,0), vec4(0,.5,0,0), vec4(0,0,.5,0), vec4(.5,.5,.5,1)) * shadow_proj * shadow_view * pos_wc;
        vec2 tc = shadow_frag.xy / shadow_frag.w;

        vec3 pr = shadow_frag.xyz / shadow_frag.w;
        float r = 0;
        if (tc.x <= 1 && tc.x >= 0 && tc.y <= 1 && tc.y >= 0) {
            float d = texture(shadow_map, pr.xy).r;
            if (d > pr.z)
                r = 1;
        }

	// use linked data only if the fragment is not shadowed by opaque geometry.
	if (r == 1) {
	    out_col.rgb += spot_factor_only() * color;
	}


     }
}
#:inputs (list "in_pos" "in_norm" "in_tc")
#:uniforms (list "hemi_dir" "light_col" "tex0" "shadow_map" "shadow_proj" "shadow_view")>


#<make-shader "diffuse-hemi+spot+tex/noshadow"
#:vertex-shader #{
#version 150 core
	,(use "vs/tc:default")
}
#:fragment-shader #{
#version 420 core
#extension GL_NV_gpu_shader5 : enable
    out vec4 out_col;
    uniform vec3 hemi_dir;
    uniform vec3 light_col;
    uniform sampler2D tex0;
    in vec4 pos_wc;
    in vec3 norm_wc;
    in vec2 tc;
    ,(use "spot")

    void main() {
	out_col = vec4(0.,0.,0.,1.);

	float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
	vec3 color = texture(tex0, tc).rgb;
	out_col += vec4(color * light_col * n_dot_l, 0.);
        
	out_col.rgb += spot_factor_only() * color;

     }
}
#:inputs (list "in_pos" "in_norm" "in_tc")
#:uniforms (list "hemi_dir" "light_col" "tex0" "shadow_map" "shadow_proj" "shadow_view")>




;; 
;; tsm
;; 

#<make-shader "shadow-frag-collect"
#:vertex-shader #{
#version 150 core
	,(use "vs:default")
}
#:fragment-shader #{
#version 420 core
	out vec4 out_col;
	in vec4 pos_wc;
	in vec3 norm_wc;
 	layout(binding = 0, offset = 0) uniform atomic_uint counter;
	coherent uniform layout(r32f) image2D shadow_frag_depths;
	coherent uniform layout(size1x32) iimage2D shadow_tail_buffer;
	coherent uniform layout(size1x32) iimage2D shadow_head_buffer;
	uniform ivec2 shadow_buffer_size;
	void main() {
	    ivec2 coord = ivec2(gl_FragCoord.xy);

	    int pos = int(atomicCounterIncrement(counter));
	    int old = imageAtomicExchange(shadow_head_buffer, coord, pos);
        
	    ivec2 pos_c = ivec2(pos % int(shadow_buffer_size.x), pos / int(shadow_buffer_size.x));
	    imageStore(shadow_tail_buffer, pos_c, ivec4(old,0,0,0));
	    imageStore(shadow_frag_depths, pos_c, vec4(gl_FragCoord.z,0,0,0));
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "diffuse_color" "shadow_frag_depths" "shadow_tail_buffer" "shadow_head_buffer" "shadow_buffer_size")>




#<make-shader "frag-count-vis"
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
#version 420 core
    coherent uniform layout(size1x32) iimage2D shadow_head_buffer;
    coherent uniform layout(size1x32) iimage2D shadow_tail_buffer;
    uniform ivec2 shadow_buffer_size;
    uniform ivec4 xywh;
    in vec2 tc;
    out vec4 out_col;
    void main() {
	out_col = vec4(0.,0.,0.,1.);
	int n = 0;
        ivec2 coord = ivec2(tc * shadow_buffer_size);
	int run = imageLoad(shadow_head_buffer, coord).r;

	if (coord.x == xywh.x || coord.x == xywh.x+xywh.w-1)
	    if (coord.y >= xywh.y && coord.y <= xywh.y+xywh.w-1) {
		out_col = vec4(1,1,0,1);
		return;
	    }

	if (coord.y == xywh.y || coord.y == xywh.y+xywh.w-1)
	    if (coord.x >= xywh.x && coord.x <= xywh.x+xywh.w-1) {
		out_col = vec4(1,1,0,1);
		return;
	    }


	while (run >= 0) {
	    ivec2 pos_c = ivec2(run % shadow_buffer_size.x, run / shadow_buffer_size.x);
	    run = imageLoad(shadow_tail_buffer, pos_c).r;	
	    ++n;
	}
	if (n < 16)
	    out_col.r += float(n)/16;
	else
	    out_col.g = .5;
     }
}
#:inputs (list "in_pos" "in_tc")
#:uniforms (list "shadow_head_buffer" "shadow_tail_buffer" "xywh" "shadow_buffer_size")>



#<make-shader "sort-shadow-frags"
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
    coherent uniform layout(r32f) image2D shadow_frag_depths;
    coherent uniform layout(size1x32) iimage2D shadow_head_buffer;
    coherent uniform layout(size1x32) iimage2D shadow_tail_buffer;
    uniform ivec2 shadow_buffer_size;
	
    void main() {

	const unsigned int N = 16;
	float depth_vals[N];
	unsigned int depth_index[N];
        
        int run = imageLoad(shadow_head_buffer, ivec2(gl_FragCoord.xy)).r;
        if (run >= 0) {
            int array_len = 0;
	    while (run >= 0 && array_len < N) {
                ivec2 pos_c = ivec2(run % shadow_buffer_size.x, run / shadow_buffer_size.x);
                depth_vals[array_len] = imageLoad(shadow_frag_depths, pos_c).r;
		depth_index[array_len] = run;
    	    	run = imageLoad(shadow_tail_buffer, pos_c).r;
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
		}
	    }
	    for (int i = 0; i < array_len; ++i) {
		imageStore(shadow_frag_depths, ivec2(depth_index[i] % shadow_buffer_size.x, depth_index[i] / shadow_buffer_size.x), vec4(depth_vals[i],0,0,0));
	    }
		
	    if (run >= 0)
		out_col = vec4(1,.6,0,0);
	    else
		out_col = vec4(0,float(array_len)/float(N),0,0);
	}
	else 
	    out_col = vec4(0,0,1,1);
    }
}
#:inputs (list "in_pos")
#:uniforms (list "shadow_frag_depths" "shadow_head_buffer" "shadow_tail_buffer" "shadow_buffer_size")>



#<make-shader "check-shadow-sort"
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
    coherent uniform layout(r32f) image2D shadow_frag_depths;
    coherent uniform layout(size1x32) iimage2D shadow_head_buffer;
    coherent uniform layout(size1x32) iimage2D shadow_tail_buffer;
    uniform ivec2 shadow_buffer_size;
	
    void main() {

	float prev_d = -1;
        int run = imageLoad(shadow_head_buffer, ivec2(gl_FragCoord.xy)).r;
        if (run >= 0) {
	    while (run >= 0) {
                ivec2 pos_c = ivec2(run % shadow_buffer_size.x, run / shadow_buffer_size.x);
                float d = imageLoad(shadow_frag_depths, pos_c).r;
		if (d < prev_d) {
		    out_col = vec4(1,0,0,1);
		    return;
		}
		prev_d = d;
    	    	run = imageLoad(shadow_tail_buffer, pos_c).r;
            }
	    out_col = vec4(0,.7,0,1);
	}
	else
	    out_col = vec4(0,0,1,1);
    }
}
#:inputs (list "in_pos")
#:uniforms (list "shadow_frag_depths" "shadow_head_buffer" "shadow_tail_buffer" "shadow_buffer_size")>



;; 
;; 
;; 

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
    coherent uniform layout(size1x32) iimage2D head_buffer;
    coherent uniform layout(size1x32) iimage2D tail_buffer;
    void main() {
	imageStore(head_buffer, ivec2(gl_FragCoord.xy), ivec4(-1,0,0,0));
	imageStore(tail_buffer, ivec2(gl_FragCoord.xy), ivec4(-1,0,0,0));
    }
}
#:inputs (list "in_pos")
#:uniforms (list "shadow_head_buffer" "shadow_tail_buffer")>



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
    coherent uniform layout(size1x32) image2D frag_depths;
    uniform ivec2 shadow_buffer_size;
    void main() {
        imageStore(frag_depths, ivec2(gl_FragCoord.xy), vec4(1,0,0,0));
    }
}
#:inputs (list "in_pos")
#:uniforms (list "shadow_frag_depths" "shadow_buffer_sizewh")>



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



