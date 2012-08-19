#lib_LTLIBRARIES = libcgls.la
bin_PROGRAMS = viewer-cgls

BUILT_SOURCES = objloader.x cmdline.x material.x drawelement.x scene.x

# libconfigfiles_la_SOURCES = grammar.yy  lexer.ll  parser.h  token.h lexer.h config_creator.h configfile.h configfile.cpp
viewer_cgls_SOURCES = 	main.c \
						drawelement.c drawelement.h \
						objloader.c objloader.h \
						scene.c scene.h \
						slist.h \
						material.c \
						cmdline.cpp cmdline.h

# for bison/flex
# AM_CPPFLAGS = -Isrc/
# AM_YFLAGS = -d -p__yacc_prefix__
# AM_LFLAGS = -P__lex_prefix__ -o lex.yy.c

#library_includedir = $(includedir)/$(PACKAGE)-$(VERSION)
#library_include_HEADERS = cgls.h

# setting c++ flags:
# AM_CXXFLAGS = -Wall -ansi -pedantic -ggdb -pg

# setting c preprocessor flags for main project
viewer_cgls_CPPFLAGS = -D PACKAGE=\"$(PACKAGE)\" -D VERSION=\"$(VERSION)\" -D CGL_GL_VERSION=GL3 -D_GNU_SOURCE @GUILE_DEF@ $(GUILE_CFLAGS)

if CAN_USE_GUILE
snarf_opts = @GUILE_DEF@ $(AM_CPPFLAGS) $(CPPFLAGS) $(viewer_cgls_CPPFLAGS)
SUFFIXES: .x
.c.x:
	guile-snarf -o $@ $< $(snarf_opts)
.cpp.x:
	guile-snarf -o $@ $< $(snarf_opts)
endif

