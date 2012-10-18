
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




#<shader-fragment "apply-linked-shadows"
#{
    coherent uniform layout(rgba8) image2D shadow_frag_colors;
    coherent uniform layout(r32f) image2D shadow_frag_depths;
    coherent uniform layout(size1x32) iimage2D shadow_head_buffer;
    coherent uniform layout(size1x32) iimage2D shadow_tail_buffer;
    float depth_vals[16];
    vec4 color_vals[16];

    vec3 apply_linked_shadows(vec3 projected, vec3 light_color) {
	vec2 shadow_tc = projected.xy;
	float reference_depth = projected.z;
	ivec2 wh = ivec2(511, 511);
	ivec2 coord = ivec2(shadow_tc * vec2(wh));
	vec3 result;

	int run = imageLoad(shadow_head_buffer, coord).r;
	if (run >= 0) {
	    int array_len = 0;
	    while (run >= 0 && array_len < 16) {
		ivec2 pos_c = ivec2(run % 512, run / 512);
		depth_vals[array_len] = imageLoad(shadow_frag_depths, pos_c).r;
		color_vals[array_len] = imageLoad(shadow_frag_colors, pos_c);
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
		    vec4 tmp = color_vals[i];
		    color_vals[i] = color_vals[swap_id];
		    color_vals[swap_id] = tmp;
		}
		if (depth_vals[i] >= reference_depth) {
		    array_len = i;
		    break;
		}
	    }
	    result = light_color;
	    for (int i = array_len-1; i >= 0; --i) {
		vec4 src = color_vals[i];
		// result = result.rgb * src.rgb * (1.0-src.a);
		// result = (1-src.a) * result.rgb + src.rgb * (src.a);
		// result = result.rgb * exp(-src.a *3* (vec3(1)-src.rgb));
		// result.r *= exp(-src.a*.001) * exp(-1+src.r);
		// result.g *= exp(-src.a*.001) * exp(-1+src.g);
		// result.b *= exp(-src.a*.001) * exp(-1+src.b);
		// result *= vec3(1)-exp(10.*-src.a*src.rgb);
		result *= (vec3(1)-exp(-src.a)) * src.rgb;
	    }
	    // result = light_color*exp(2*(vec3(1.0)-result));
	}
	else
	    result = light_color;
	return result;
    }
}
#:uniforms (list "shadow_frag_colors" "shadow_frag_depths" "shadow_head_buffer" "shadow_tail_buffer")>


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



#<shader-fragment "collector/decls"
#{
    layout(binding = 0, offset = 0) uniform atomic_uint counter;
    coherent uniform layout(rgba8) image2D cam_frag_colors;
    coherent uniform layout(r32f) image2D cam_frag_depths;
    coherent uniform layout(size1x32) iimage2D cam_tail_buffer;
    coherent uniform layout(size1x32) iimage2D cam_head_buffer;
    uniform sampler2D cam_opaque_depth;
    uniform ivec2 cam_buffer_size;
}
#:uniforms (list "cam_frag_colors" "cam_frag_depths" "cam_tail_buffer" "cam_head_buffer" "cam_opaque_depth" "cam_buffer_size")>

#<shader-fragment "collector/collect"
#{
    int pos = int(atomicCounterIncrement(counter));
    int old = imageAtomicExchange(cam_head_buffer, coord, pos);
        
    ivec2 pos_c = ivec2(pos % cam_buffer_size.x, pos / cam_buffer_size.x);
    imageStore(cam_tail_buffer, pos_c, ivec4(old,0,0,0));
    imageStore(cam_frag_colors, pos_c, result);
    imageStore(cam_frag_depths, pos_c, vec4(gl_FragCoord.z,0,0,0));
}>

#<make-shader "diffuse-hemi/collect"
#:vertex-shader #{
#version 150 core
	,(use "vs:default")
}
#:fragment-shader #{
#version 420 core
	out vec4 out_col;
	uniform vec3 hemi_dir;
	uniform vec3 light_col;
	uniform vec4 diffuse_color;
	in vec4 pos_wc;
	in vec3 norm_wc;
	,(use "collector/decls")

	void main() {
            if (texture(cam_opaque_depth, gl_FragCoord.xy/vec2(cam_buffer_size)).r <= gl_FragCoord.z)
		discard;
            ivec2 coord = ivec2(gl_FragCoord.xy);

	    float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
	    vec4 result = vec4(diffuse_color.rgb * light_col * n_dot_l, diffuse_color.a);

	    ,(use "collector/collect")
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "hemi_dir" "light_col" "diffuse_color")>


#<make-shader "diffuse-hemi+spot/collect"
#:vertex-shader #{
#version 150 core
	,(use "vs:default")
}
#:fragment-shader #{
#version 420 core
	out vec4 out_col;
	uniform vec3 hemi_dir;
	uniform vec3 light_col;
	uniform vec4 diffuse_color;
	in vec4 pos_wc;
	in vec3 norm_wc;
	,(use "collector/decls")
	,(use "spot")

	void main() {
            if (texture(cam_opaque_depth, gl_FragCoord.xy/vec2(cam_buffer_size)).r <= gl_FragCoord.z)
		discard;
            ivec2 coord = ivec2(gl_FragCoord.xy);

	    float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
	    vec4 result = vec4(diffuse_color.rgb * light_col * n_dot_l, diffuse_color.a);
	    result.rgb += spot_factor() * diffuse_color.rgb;

	    ,(use "collector/collect")
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "hemi_dir" "light_col" "diffuse_color")>

#<make-shader "diffuse-hemi+tex/collect"
#:vertex-shader #{
#version 150 core
	,(use "vs/tc:default")
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
	uniform sampler2D tex0;
	,(use "collector/decls")
	void main() {
	    if (texture(cam_opaque_depth, gl_FragCoord.xy/vec2(cam_buffer_size)).r <= gl_FragCoord.z)
		discard;
	    ivec2 coord = ivec2(gl_FragCoord.xy);

	    float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
	    vec4 color = texture(tex0, tc);
	    vec4 result = vec4(color.rgb * diffuse_color.rgb * light_col * n_dot_l, color.a * diffuse_color.a);

	    ,(use "collector/collect")
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "hemi_dir" "light_col" "diffuse_color" "tex0")>

#<make-shader "diffuse-hemi+spot+tex/collect"
#:vertex-shader #{
#version 150 core
	,(use "vs/tc:default")
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
	uniform sampler2D tex0;
	,(use "collector/decls")
	,(use "spot")

	void main() {
            if (texture(cam_opaque_depth, gl_FragCoord.xy/vec2(cam_buffer_size)).r <= gl_FragCoord.z)
		discard;
            ivec2 coord = ivec2(gl_FragCoord.xy);

	    float n_dot_l = max(0, 0.5*(1+dot(norm_wc, hemi_dir)));
	    vec4 color = texture(tex0, tc);
	    vec4 result = vec4(color.rgb * diffuse_color.rgb * light_col * n_dot_l, diffuse_color.a);
	    result.rgb += spot_factor() * color.rgb * diffuse_color.rgb;

	    ,(use "collector/collect")
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "hemi_dir" "light_col" "diffuse_color" "tex0")>


;; 
;; shadow version
;; 

#<shader-fragment "transp-shadow-collector/decls"
#{
    layout(binding = 0, offset = 0) uniform atomic_uint counter;
    coherent uniform layout(rgba8) image2D shadow_frag_colors;
    coherent uniform layout(r32f) image2D shadow_frag_depths;
    coherent uniform layout(size1x32) iimage2D shadow_tail_buffer;
    coherent uniform layout(size1x32) iimage2D shadow_head_buffer;
    uniform sampler2D shadow_opaque_depth;
}
#:uniforms (list "shadow_frag_colors" "shadow_frag_depths" "shadow_tail_buffer" "shadow_head_buffer" "shadow_opaque_depth")>


#<shader-fragment "transp-shadow-collector/collect"
#{
    int pos = int(atomicCounterIncrement(counter));
    int old = imageAtomicExchange(shadow_head_buffer, coord, pos);
        
    ivec2 pos_c = ivec2(pos % wh.x, pos / wh.x);
    imageStore(shadow_tail_buffer, pos_c, ivec4(old,0,0,0));
    imageStore(shadow_frag_colors, pos_c, result);
    imageStore(shadow_frag_depths, pos_c, vec4(gl_FragCoord.z,0,0,0));
}>


#<make-shader "transp-shadow-collect"
#:vertex-shader #{
#version 150 core
	,(use "vs:default")
}
#:fragment-shader #{
#version 420 core
	out vec4 out_col;
	uniform vec4 diffuse_color;
	in vec4 pos_wc;
	in vec3 norm_wc;
	,(use "transp-shadow-collector/decls")
	void main() {
	    ivec2 wh = ivec2(512, 512);
	    if (texture(shadow_opaque_depth, gl_FragCoord.xy/vec2(wh)).r <= gl_FragCoord.z)
		discard;
	    ivec2 coord = ivec2(gl_FragCoord.xy);
	    vec4 result = diffuse_color;
	    ,(use "transp-shadow-collector/collect")
	    out_col = result;
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "diffuse_color")>


#<make-shader "transp-shadow-collect+tex"
#:vertex-shader #{
#version 150 core
	,(use "vs/tc:default")
}
#:fragment-shader #{
#version 420 core
	out vec4 out_col;
        uniform sampler2D tex0;
	uniform vec4 diffuse_color;
	in vec4 pos_wc;
	in vec3 norm_wc;
	in vec2 tc;
	,(use "transp-shadow-collector/decls")
	void main() {
	    ivec2 wh = ivec2(512, 512);
	    if (texture(shadow_opaque_depth, gl_FragCoord.xy/vec2(wh)).r <= gl_FragCoord.z)
		discard;
	    ivec2 coord = ivec2(gl_FragCoord.xy);
	    vec4 color = texture(tex0, tc);
	    vec4 result = vec4(color.rgb * diffuse_color.rgb, color.a * diffuse_color.a);
	    ,(use "transp-shadow-collector/collect")
	}
}
#:inputs (list "in_pos" "in_norm" "in_tc")
#:uniforms (list "tex0" "diffuse_color")>



;; 
;; apply linked-list
;;



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
    coherent uniform layout(rgba8) image2D cam_frag_colors;
    coherent uniform layout(r32f) image2D cam_frag_depths;
    coherent uniform layout(size1x32) iimage2D cam_head_buffer;
    coherent uniform layout(size1x32) iimage2D cam_tail_buffer;
    uniform sampler2D tex0;
    uniform ivec2 cam_buffer_size;
    uniform int array_layers;
	
    void main() {

	float depth_vals[16];   // this still has to be set manually.
	vec4 color_vals[16];
        
        int run = imageLoad(cam_head_buffer, ivec2(gl_FragCoord.xy)).r;
        if (run >= 0) {
            int array_len = 0;
	    while (run >= 0 && array_len < 16) {
                ivec2 pos_c = ivec2(run % cam_buffer_size.x, run / cam_buffer_size.x);
                depth_vals[array_len] = imageLoad(cam_frag_depths, pos_c).r;
                color_vals[array_len] = imageLoad(cam_frag_colors, pos_c);
    	    	run = imageLoad(cam_tail_buffer, pos_c).r;
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
		
	    vec4 base = vec4(texture(tex0, gl_FragCoord.xy/vec2(cam_buffer_size)).rgb, 1);
	    for (int i = int (array_len)-1; i >= 0; --i) {
		vec4 src = color_vals[i];
		base = src.rgba * src.a + base.rgba * (1-src.a);
	    }

	    out_col = base;
	}
	else 
	    out_col = vec4(texture(tex0, gl_FragCoord.xy/vec2(cam_buffer_size)).rgb, 1);
    }
}
#:inputs (list "in_pos")
#:uniforms (list "cam_frag_colors" "cam_frag_depths" "cam_buffer_size" "tex0" "cam_head_buffer" "cam_tail_buffer")>


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
	//uniform vec2 wh;
	void main() {
	    vec2 wh = vec2(512, 512);
	    ivec2 coord = ivec2(gl_FragCoord.xy);

	    int pos = int(atomicCounterIncrement(counter));
	    int old = imageAtomicExchange(shadow_head_buffer, coord, pos);
        
	    ivec2 pos_c = ivec2(pos % int(wh.x), pos / int(wh.x));
	    imageStore(shadow_tail_buffer, pos_c, ivec4(old,0,0,0));
	    imageStore(shadow_frag_depths, pos_c, vec4(gl_FragCoord.z,0,0,0));
	}
}
#:inputs (list "in_pos" "in_norm")
#:uniforms (list "diffuse_color" "shadow_frag_depths" "shadow_tail_buffer" "shadow_head_buffer")>




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
    uniform ivec4 xywh;
    in vec2 tc;
    out vec4 out_col;
    void main() {
	out_col = vec4(0.,0.,0.,1.);
	int n = 0;
        ivec2 coord = ivec2(tc * vec2(512));
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
	    ivec2 pos_c = ivec2(run % 512, run / 512);
	    run = imageLoad(shadow_tail_buffer, pos_c).r;	
	    ++n;
	}
	if (n < 16)
	    out_col.r += float(n)/16;
	else
	    out_col.g = .5;
//	if (run == -1) out_col.r = 1;
//	else if (run == 0) out_col.g = 1;
//	else out_col.b = 1;
     }
}
#:inputs (list "in_pos" "in_tc")
#:uniforms (list "shadow_head_buffer" "shadow_tail_buffer" "xywh")>




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
	ivec2 wh = ivec2(512, 512);
	imageStore(head_buffer, ivec2(gl_FragCoord.xy), ivec4(-1,0,0,0));
	imageStore(tail_buffer, ivec2(gl_FragCoord.xy), ivec4(-1,0,0,0));
    }
}
#:inputs (list "in_pos")
#:uniforms (list "shadow_head_buffer" "shadow_tail_buffer" "wh")>



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
    uniform ivec2 wh;
    void main() {
        imageStore(frag_depths, ivec2(gl_FragCoord.xy), vec4(1,0,0,0));
    }
}
#:inputs (list "in_pos")
#:uniforms (list "shadow_frag_depths" "wh")>



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



