#include "cmdline.h"


#include <argp.h>
#include <string>
#include <iostream>
#include <sstream>
#include <stdlib.h>



using namespace std;

const char *argp_program_version = VERSION;

static char doc[]       = PACKAGE ": description";
static char args_doc[]  = "argumentdescription";

// long option without corresponding short option have to define a symbolic constant >= 300
// enum { FIRST = 300, REMOVE, OPTS };

static struct argp_option options[] = 
{
	// --[opt]		short/const		arg-descr		?		option-descr
	{ "verbose", 'v', 0,         0, "Be verbose." },
	{ "hemi", 'h', "x,y,z",     0, "Use a hemispherical light."},
	{ 0 }
};	

string& replace_nl(string &s)
{
	for (int i = 0; i < s.length(); ++i)
		if (s[i] == '\n' || s[i] == '\r')
			s[i] = ' ';
	return s;
}

vec3f read_vec3f(const std::string &s) {
	istringstream iss(s);
	vec3f v;
	char sep;
	iss >> v.x >> sep >> v.y >> sep >> v.z;
// 	cout << v.x << "\t" << v.y << "\t" << v.z << endl;
	return v;
}


static error_t parse_options(int key, char *arg, argp_state *state)
{
	// call argp_usage to stop program execution if something is wrong
	
	string sarg;
	if (arg)
		sarg = arg;
	sarg = replace_nl(sarg);

	switch (key)
	{
	case 'v':	cmdline.verbose = true; 	break;
	case 'h':	cmdline.hemi = true; cmdline.hemi_dir = read_vec3f(sarg); break;
	
	case ARGP_KEY_ARG:		// process arguments. 
							// state->arg_num gives number of current arg
				if (cmdline.filename)
					fprintf(stderr, "ERROR: you can display only one model at a time.\n");
				cmdline.filename = strdup(arg);
		break;

	default:
		return ARGP_ERR_UNKNOWN;
	}

	return 0;
}

static struct argp parser = { options, parse_options, args_doc, doc };

int parse_cmdline(int argc, char **argv)
{
	cmdline.filename = 0;
	cmdline.hemi = false;
	int ret = argp_parse(&parser, argc, argv, /*ARGP_NO_EXIT*/0, 0, 0);

	if (cmdline.filename == 0) {
		fprintf(stderr, "ERROR: no model file specified. exiting...\n");
		exit(EXIT_FAILURE);
	}
	return ret;
}
	
Cmdline cmdline;

