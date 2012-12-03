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




#<make-shader "quad"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	out vec4 pos_wc;
	void main() {
		pos_wc = vec4(in_pos.xy*.25, .8, 1.0);
		gl_Position = pos_wc;
	}
}
#:fragment-shader #{
#version 150 core
	out vec4 out_col;
	in vec4 pos_wc;
	void main() {
		out_col = vec4(0.,1.,0.,1.);
	}
}
#:inputs (list "in_pos")
#:uniforms (list )>




#<make-shader "quad-tess"
#:vertex-shader #{
#version 150 core
	in vec3 in_pos;
	out vec3 pos_wc;
	void main() {
		pos_wc = vec3(in_pos.xy*.25, .8);
	}
}
#:tess-control-shader #{
#version 420 core
    layout(vertices = 3) out;
    in vec3 pos_wc[];
    out vec3 tc_pos_wc[];
    uniform float tl_inner;
    uniform float tl_outer;

    void main()
    {
        tc_pos_wc[gl_InvocationID] = pos_wc[gl_InvocationID];
        //if (gl_InvocationID == 0) {
            gl_TessLevelInner[0] = 2.0f;//tl_inner;
            gl_TessLevelOuter[0] = 2.0f;//tl_outer;
            gl_TessLevelOuter[1] = 2.0f;//tl_outer;
            gl_TessLevelOuter[2] = 4.0f;//tl_outer;
        //}
    }
}
#:tess-eval-shader #{
#version 420 core
    layout(triangles, equal_spacing, cw) in;
    in vec3 tc_pos_wc[];

    void main()
    {
        vec3 p0 = gl_TessCoord.x * tc_pos_wc[0];
        vec3 p1 = gl_TessCoord.y * tc_pos_wc[1];
        vec3 p2 = gl_TessCoord.z * tc_pos_wc[2];
        vec3 te_pos = p0 + p1 + p2;
        gl_Position = vec4(te_pos,1);
    }
}
#:fragment-shader #{
#version 150 core
	out vec4 out_col;
	void main() {
		out_col = vec4(0.,1.,0.,1.);
	}
}
#:inputs (list "in_pos")
#:uniforms (list "tl_inner" "tl_outer")>




