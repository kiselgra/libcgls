#lib_LTLIBRARIES = libcgls.la
bin_PROGRAMS = viewer-cgls

# libconfigfiles_la_SOURCES = grammar.yy  lexer.ll  parser.h  token.h lexer.h config_creator.h configfile.h configfile.cpp
viewer_cgls_SOURCES = 	main.c \
						drawelement.c drawelement.h \
						objloader.c objloader.h \
						scene.c \
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
viewer_cgls_CPPFLAGS = -D PACKAGE=\"$(PACKAGE)\" -D VERSION=\"$(VERSION)\" -D CGL_GL_VERSION=GL3 -D_GNU_SOURCE

