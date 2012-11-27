lib_LTLIBRARIES = libcgls.la
noinst_PROGRAMS = viewer-cgls #tsm

BUILT_SOURCES = drawelement.xx scene.xx material.xx
BUILT_SOURCES += objloader.x cmdline.x material.x drawelement.x scene.x

# libconfigfiles_la_SOURCES = grammar.yy  lexer.ll  parser.h  token.h lexer.h config_creator.h configfile.h configfile.cpp
libcgls_la_SOURCES = 	drawelement.c drawelement.h \
						objloader.c objloader.h \
						scene.c scene.h \
						slist.h \
						basename.h \
						material.c material.h

viewer_cgls_SOURCES = $(libcgls_la_SOURCES) \
					    main.c \
                        cmdline.cpp cmdline.h

#tsm_SOURCES = $(libcgls_la_SOURCES) tsm.cpp tsm-cmdline.h tsm-cmdline.cpp

# for bison/flex
# AM_CPPFLAGS = -Isrc/
# AM_YFLAGS = -d -p__yacc_prefix__
# AM_LFLAGS = -P__lex_prefix__ -o lex.yy.c

library_includedir = $(includedir)/$(PACKAGE)-$(VERSION)
library_include_HEADERS = drawelement.h objloader.h scene.h slist.h basename.h material.h

# setting c++ flags:
# AM_CXXFLAGS = -Wall -ansi -pedantic -ggdb -pg

# setting c preprocessor flags for main project
viewer_cgls_CPPFLAGS = -D PACKAGE=\"$(PACKAGE)\" -D VERSION=\"$(VERSION)\" -D DATADIR=\"$(pkgdatadir)\" -D CGL_GL_VERSION=GL3 -D_GNU_SOURCE @GUILE_DEF@ $(GUILE_CFLAGS)
viewer_cgls_LDFLAGS = $(GUILE_LDFLAGS)

#tsm_CPPFLAGS = -D PACKAGE=\"$(PACKAGE)\" -D VERSION=\"$(VERSION)\" -D DATADIR=\"$(pkgdatadir)\" -D CGL_GL_VERSION=GL3 -D_GNU_SOURCE @GUILE_DEF@ $(GUILE_CFLAGS)
#tsm_LDFLAGS = $(GUILE_LDFLAGS)

libcgls_la_CPPFLAGS = -D PACKAGE=\"$(PACKAGE)\" -D VERSION=\"$(VERSION)\" -D DATADIR=\"$(pkgdatadir)\" -D CGL_GL_VERSION=GL3 -D_GNU_SOURCE @GUILE_DEF@ $(GUILE_CFLAGS)
libcgls_la_LIBADD = $(GUILE_LDFLAGS)

SUFFIXES: .x .xx

if CAN_USE_GUILE
snarf_opts = @GUILE_DEF@ $(AM_CPPFLAGS) $(CPPFLAGS) $(viewer_cgls_CPPFLAGS)
.c.x:
	guile-snarf -o $@ $< $(snarf_opts)
.cpp.x:
	guile-snarf -o $@ $< $(snarf_opts)
endif

.c.xx: $<
	m4 @cgldata@/mm.m4 $< > $@

# install link
library_nonversioned_includedir = $(includedir)/$(PACKAGE)
install-data-hook:
	@test -L $(library_nonversioned_includedir) && echo "   !!!   The nonversioned library include dir <$(library_nonversioned_includedir)> is already covered by a link. You'll have to update yourself". || true
	test -L $(library_nonversioned_includedir) || ln -s $(library_includedir) $(library_nonversioned_includedir)


