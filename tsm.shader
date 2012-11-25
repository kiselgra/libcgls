
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
	in vec3 in_pos;
	uniform mat4 proj;
	uniform mat4 view;
	uniform mat4 model;
	out vec4 pos_ec;
	void main() {
	    pos_ec = view * model * vec4(in_pos, 1.0);
	    gl_Position = proj * pos_ec;
	}
}
#:fragment-shader #{
#version 420 core
	out vec4 out_col;
	in vec4 pos_ec;
	in vec3 norm_wc;
 	layout(binding = 0, offset = 0) uniform atomic_uint counter;
	coherent uniform layout(r32f) image2D shadow_frag_depths;
	coherent uniform layout(size1x32) iimage2D shadow_tail_buffer;
	coherent uniform layout(size1x32) iimage2D shadow_head_buffer;
	uniform ivec2 shadow_buffer_size;
	uniform vec2 cam_near_far;
	void main() {
	    ivec2 coord = ivec2(gl_FragCoord.xy);

	    int pos = int(atomicCounterIncrement(counter));
	    int old = imageAtomicExchange(shadow_head_buffer, coord, pos);
        
	    ivec2 pos_c = ivec2(pos % int(shadow_buffer_size.x), pos / int(shadow_buffer_size.x));
	    float d = (-pos_ec.z - cam_near_far.x) / (cam_near_far.y - cam_near_far.x);
	    imageStore(shadow_tail_buffer, pos_c, ivec4(old,0,0,0));
	    imageStore(shadow_frag_depths, pos_c, vec4(d,0,0,0));
	}
}
#:inputs (list "in_pos")
#:uniforms (list "proj" "view" "model" "shadow_frag_depths" "shadow_tail_buffer" "shadow_head_buffer" "shadow_buffer_size" "cam_near_far")>




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
// initializes the alpha values, too.
    out vec4 out_col;
    coherent uniform layout(r32f) image2D shadow_frag_depths;
	    coherent uniform layout(r32f) image2D shadow_frag_alpha;
	    coherent uniform layout(size1x32) iimage2D shadow_head_buffer;
    coherent uniform layout(size1x32) iimage2D shadow_tail_buffer;
    uniform ivec2 shadow_buffer_size;
    uniform float epsilon;
	
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
	    imageStore(shadow_frag_depths, ivec2(depth_index[0] % shadow_buffer_size.x, depth_index[0] / shadow_buffer_size.x), vec4(depth_vals[0],0,0,0));
	    int j = 1;
	    for (int i = 1; i < array_len; ++i) {
		if (depth_vals[i] - depth_vals[j-1] > epsilon) {
		    imageStore(shadow_frag_depths, ivec2(depth_index[j] % shadow_buffer_size.x, depth_index[j] / shadow_buffer_size.x), vec4(depth_vals[j],0,0,0));
		    imageStore(shadow_frag_alpha, ivec2(depth_index[j] % shadow_buffer_size.x, depth_index[j] / shadow_buffer_size.x), vec4(1,0,0,0));
		    ++j;
		}
	    }
	    imageStore(shadow_tail_buffer, ivec2(depth_index[j-1] % shadow_buffer_size.x, depth_index[j-1] / shadow_buffer_size.x), ivec4(-1,0,0,0));
		
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
#:uniforms (list "shadow_frag_depths" "shadow_frag_alpha" "shadow_head_buffer" "shadow_tail_buffer" "shadow_buffer_size" "epsilon")>



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


#<make-shader "mipmap"
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
    coherent uniform layout(size1x32) iimage2D from_head;
    coherent uniform layout(size1x32) iimage2D from_tail;
    coherent uniform layout(r32f) image2D from_depth;
    coherent uniform layout(r32f) image2D from_alpha;
    coherent uniform layout(size1x32) iimage2D to_head;
    coherent uniform layout(size1x32) iimage2D to_tail;
    coherent uniform layout(r32f) image2D to_depth;
    coherent uniform layout(r32f) image2D to_alpha;
    uniform ivec2 shadow_buffer_size;
    uniform ivec2 target_level_size;
    uniform float epsilon;
    layout(binding = 0, offset = 0) uniform atomic_uint counter;

    const int sentinel = 2;

    ivec2 index_to_buffer_pos(int index) {
	return ivec2(index % shadow_buffer_size.x, index / shadow_buffer_size.x);
    }

    float read_depth(int run, inout ivec2 buf_pos) {
	float d;
	if (run >= 0) {
	    buf_pos = index_to_buffer_pos(run);
	    d = imageLoad(from_depth, buf_pos).r;
	}
	else
	    d = sentinel;
	return d;
    }

    int write_depth(inout float d, inout int run, inout int out_index, inout ivec2 buf_pos, inout float last_depth_written,
		    inout float[4] alpha, int alpha_id) {
	if (d == sentinel)
	    return 1;

	if (d - last_depth_written > epsilon) {
	    last_depth_written = d;

	    // write the new cell in the tail pointer of the previous cell, if there is such a cell
	    int new_out = int(atomicCounterIncrement(counter));
	    if (out_index >= 0) {
		buf_pos = index_to_buffer_pos(out_index);
		imageStore(to_tail, buf_pos, ivec4(new_out,0,0,0));
		// when adding a new tail pointer we have computed an alpha for the previous cell (the values we skipped in between)
		imageStore(to_alpha, buf_pos, vec4((alpha[0]+alpha[1]+alpha[2]+alpha[3])/4.0,0,0,0));
	    }
	    else { // otherwise install the head pointer
		imageStore(to_head, ivec2(gl_FragCoord.xy), ivec4(new_out,0,0,0));
	    }
	    	 
	    // write the current depth value into the new cell
	    buf_pos = index_to_buffer_pos(new_out);
	    imageStore(to_depth, buf_pos, vec4(d,0,0,0));

	    // reset output position
	    out_index = new_out;
	    
	    // reset alpha
	    alpha = float[4](0,0,0,0);
	}

	// update running data structures for next element of the sublist this value written out came from.
	buf_pos = index_to_buffer_pos(run);
	run = imageLoad(from_tail, buf_pos).r;
	d = read_depth(run, buf_pos);
	alpha[alpha_id] = max(1.0, alpha[alpha_id] + imageLoad(from_alpha, buf_pos).r);
	
	return 0;
    }

    void main() {
	int run00 = imageLoad(from_head, 2*ivec2(gl_FragCoord.xy)).r,
	    run01 = imageLoad(from_head, 2*ivec2(gl_FragCoord.xy) + ivec2(0, 1)).r,
	    run10 = imageLoad(from_head, 2*ivec2(gl_FragCoord.xy) + ivec2(1, 0)).r,
	    run11 = imageLoad(from_head, 2*ivec2(gl_FragCoord.xy) + ivec2(1, 1)).r;
	ivec2 buf_pos;
	float depth00 = read_depth(run00, buf_pos);
	float depth01 = read_depth(run01, buf_pos);
	float depth10 = read_depth(run10, buf_pos);
	float depth11 = read_depth(run11, buf_pos);
	float last_depth_written = -2;
	int out_index = -1; // tail of the new list
	float alpha[4] = float[4](0,0,0,0);
	int alpha_id = 0;
	while (true) {
	    float chosen_d;
	    int chosen_run, changed_run;
	    if (depth00 < depth01) {
		if (depth00 < depth10) {
		    if (depth00 < depth11) { chosen_d = depth00; chosen_run = changed_run = run00; alpha_id = 0;}
		    else                   { chosen_d = depth11; chosen_run = changed_run = run11; alpha_id = 3;}
		}
		else {
		    if (depth10 < depth11) { chosen_d = depth10; chosen_run = changed_run = run10; alpha_id = 2;}
		    else                   { chosen_d = depth11; chosen_run = changed_run = run11; alpha_id = 3;}
		}
	    }
	    else {
		if (depth01 < depth10) {
		    if (depth01 < depth11) { chosen_d = depth01; chosen_run = changed_run = run01; alpha_id = 1;}
		    else                   { chosen_d = depth11; chosen_run = changed_run = run11; alpha_id = 3;}
		}
		else {
		    if (depth10 < depth11) { chosen_d = depth10; chosen_run = changed_run = run10; alpha_id = 2;}
		    else                   { chosen_d = depth11; chosen_run = changed_run = run11; alpha_id = 3;}
		}
	    }
	    if (write_depth(chosen_d, changed_run, out_index, buf_pos, last_depth_written, alpha, alpha_id) == 1)
		break;
		
	    if (run00 == chosen_run) run00 = changed_run, depth00 = chosen_d;
	    else if (run01 == chosen_run) run01 = changed_run, depth01 = chosen_d;
	    else if (run10 == chosen_run) run10 = changed_run, depth10 = chosen_d;
	    else if (run11 == chosen_run) run11 = changed_run, depth11 = chosen_d;
	}
	
	// write end of list marker in the last cell
	buf_pos = index_to_buffer_pos(out_index);
	imageStore(to_tail, buf_pos, ivec4(-1,0,0,0));

	// imageStore(to_head, ivec2(gl_FragCoord.xy), ivec4(out_index,0,0,0));

	out_col = vec4(0.2, 0.5, 0.8, 1.0);
    }
}
#:inputs (list "in_pos")
#:uniforms (list "from_head" "from_tail" "from_depth" "from_alpha" "to_head" "to_tail" "to_depth" "to_alpha" "shadow_buffer_size" "target_level_size" "epsilon")>
	
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



