#ifndef __CMDLINE_H__ 
#define __CMDLINE_H__ 

#include <libmcm/vectors.h>

#include <string.h>

//! \brief Translated command line options
typedef struct
{
	bool verbose;	//!< wheather or not to be verbose
	const char *filename;
	vec3f hemi_dir;
	bool hemi;
    const char *config;
    const char *include_path;
    vec2f res;
} Cmdline;

#ifdef __cplusplus
extern "C" {
#endif

	extern Cmdline cmdline;
	int parse_cmdline(int argc, char **argv);

#ifdef __cplusplus
}
#endif

#endif

