#include "perf.h"

#include <libmcm/vectors.h>
#include <argp.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <string>
#include <sstream>

using namespace std;

const char *argp_program_version = VERSION;

static char doc[]       = PACKAGE "/performance: take performance measurements for a given scene.";
static char args_doc[]  = "<scene file>";

static struct argp_option options[] = 
{
	// --[opt]		short/const		arg-descr		?		option-descr
//     { "config", 'c', "configfile", 0, "Scheme file to supply instructions."},
//     { "include-path", 'I', "path", 0, "Path to search for the config file. Default: " DATADIR "."},
    { "output", 'o', "file", 0, "Output file for performance information."},
    { "res", 'r', "w,h", 0, "Window resolution."},
    { "factor", 'f', "x", 0, "Drawelement collapse threshold."},
	{ 0 }
};	

string& replace_nl(string &s)
{
	for (int i = 0; i < s.length(); ++i)
		if (s[i] == '\n' || s[i] == '\r')
			s[i] = ' ';
	return s;
}


vec2i read_vec2i(const std::string &s) {
	istringstream iss(s);
	vec2i v;
	char sep;
	iss >> v.x >> sep >> v.y;
	return v;
}

char *config = 0, *include_path = 0, *perf_file = 0, *outfile = 0;
vec2i res(1366, 768);
extern float collapse_factor;

static error_t parse_options(int key, char *arg, argp_state *state)
{
	// call argp_usage to stop program execution if something is wrong
	
	extern bool cgls_deferred;

	string sarg;
	if (arg)
		sarg = arg;
	sarg = replace_nl(sarg);

	switch (key)
	{
//     case 'c':   config = strdup(arg); break;
//     case 'I':   include_path = strdup(arg); break;
    case 'r':   res = read_vec2i(sarg); break;
    case 'f':   collapse_factor = atof(arg); break;
    case 'o':   outfile = strdup(arg); break;
	case ARGP_KEY_ARG:		// process arguments. 
							// state->arg_num gives number of current arg
				if (perf_file)
					fprintf(stderr, "ERROR: you can display only one perf_file at a time.\n");
				perf_file = strdup(arg);
		break;

	default:
		return ARGP_ERR_UNKNOWN;
	}

	return 0;
}

static struct argp parser = { options, parse_options, args_doc, doc };

// #ifdef WITH_GUILE	
// #include <libguile.h>
// #include <libcgl/scheme.h>
// 
// extern "C" {
// 	SCM_DEFINE(s_cmdline, "query-cmdline", 1, 0, 0, (SCM what), "") {
// 		if (!scm_is_symbol(what))
// 			scm_throw(scm_from_locale_symbol("cmdline-error"), scm_list_2(what, scm_from_locale_string("is not a symbol")));
// 		char *w = scm_to_locale_string(scm_symbol_to_string(what));
// 		string s = w;
// 		free(w);
// 		if (s == "model")
// 			return scm_from_locale_string(model);
// 		else if (s == "merge-factor")
// 			return scm_from_double(collapse_factor);
// 
// 		scm_throw(scm_from_locale_symbol("cmdline-error"), 
// 		          scm_list_2(what, 
// 		                    scm_from_locale_string("invalid option. use hemi, hemi-dir, model, filetype")));
// 		return SCM_BOOL_F; // for -Wreturn-type.
// 	}
// }
// #endif
// 
// void register_scheme_functions_for_perf_cmdline() {
// #include "perf-main.x"
// }
// 
// static void my_init_pre() {
// 	register_scheme_functions_for_perf_cmdline();
// }

int main(int argc, char **argv)
{	
	int ret = argp_parse(&parser, argc, argv, /*ARGP_NO_EXIT*/0, 0, 0);

	if (perf_file == 0) {
		fprintf(stderr, "ERROR: no perf_file specified. exiting...\n");
		exit(EXIT_FAILURE);
	}
	
// 	init_pre = my_init_pre;

// 	int guile_mode = guile_cfg_only;
// 	int guile_mode = with_guile;
// 	startup_cgl("name", 4, 2, argc, argv, (int)cmdline.res.x, (int)cmdline.res.y, actual_main, guile_mode, false, 0);
	enter("default performance run", 4, 3, res.x, res.y, perf_file, outfile);

	return 0;
}


