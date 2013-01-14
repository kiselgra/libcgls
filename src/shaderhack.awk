{ 
	if      ($1 == "shader{")  { in_shader = 1; printf "\"\\\n"; }
	else if ($1 == "}shader;") { in_shader = 0; printf "\";\n"; }
	else if (in_shader == 1)   { printf "%s\\n\\\n", $0; }
	else                       { printf "%s\n", $0; }
}
