
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



