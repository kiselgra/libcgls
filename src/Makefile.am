lib_LTLIBRARIES = libcgls.la
noinst_PROGRAMS = viewer-cgls #tsm

BUILT_SOURCES = drawelement.xx scene.xx material.xx picking.xx light.xx console.xx
BUILT_SOURCES += objloader.x cmdline.x material.x drawelement.x scene.x
BUILT_SOURCES += stock-shader.c picking.c console.cpp

EXTRA_DIST = stock-shader.c.in picking.c.in

# libconfigfiles_la_SOURCES = grammar.yy  lexer.ll  parser.h  token.h lexer.h config_creator.h configfile.h configfile.cpp
libcgls_la_SOURCES = 	\
						basename.h \
						c-utils.c c-utils.h \
						cgls-config.h \
						console.cpp console.h \
						drawelement.c drawelement.h \
						interaction.c interaction.h \
						light.c light.h \
						material.c material.h \
						objloader.c objloader.h \
						picking.c picking.h \
						scene.c scene.h \
						stock-shader.c \
						uniforms.c uniforms.h

viewer_cgls_SOURCES = $(libcgls_la_SOURCES) \
					    main.c \
                        cmdline.cpp cmdline.h

#tsm_SOURCES = $(libcgls_la_SOURCES) tsm.cpp tsm-cmdline.h tsm-cmdline.cpp

# for bison/flex
# AM_CPPFLAGS = -Isrc/
# AM_YFLAGS = -d -p__yacc_prefix__
# AM_LFLAGS = -P__lex_prefix__ -o lex.yy.c

library_includedir = $(includedir)/$(PACKAGE)-$(VERSION)
library_include_HEADERS = drawelement.h objloader.h scene.h c-utils.h basename.h material.h picking.h stock-shader.h uniforms.h cgls.h light.h interaction.h console.h cgls-config.h

# setting c++ flags:
# AM_CXXFLAGS = -Wall -ansi -pedantic -ggdb -pg

# setting c preprocessor flags for main project
viewer_cgls_CPPFLAGS = -D PACKAGE=\"$(PACKAGE)\" -D VERSION=\"$(VERSION)\" -D DATADIR=\"$(pkgdatadir)\" -D_GNU_SOURCE @GUILE_DEF@ $(GUILE_CFLAGS) $(CGL_CFLAGS)
viewer_cgls_LDFLAGS = $(GUILE_LDFLAGS) $(CGL_LIBS)

#tsm_CPPFLAGS = -D PACKAGE=\"$(PACKAGE)\" -D VERSION=\"$(VERSION)\" -D DATADIR=\"$(pkgdatadir)\" -D_GNU_SOURCE @GUILE_DEF@ $(GUILE_CFLAGS) $(CGL_CFLAGS)
#tsm_LDFLAGS = $(GUILE_LDFLAGS) $(CGL_LIBS)

libcgls_la_CPPFLAGS = -D PACKAGE=\"$(PACKAGE)\" -D VERSION=\"$(VERSION)\" -D DATADIR=\"$(pkgdatadir)\" -D_GNU_SOURCE @GUILE_DEF@ $(GUILE_CFLAGS) $(CGL_CFLAGS)
libcgls_la_LIBADD = $(GUILE_LDFLAGS) $(CGL_LIBS)

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

.cpp.xx: $<
	m4 @cgldata@/mm.m4 $< > $@

stock-shader.c: stock-shader.c.in
	awk -f shaderhack.awk stock-shader.c.in > stock-shader.c

picking.c: picking.c.in
	awk -f shaderhack.awk picking.c.in > picking.c

console.cpp: console.cpp.in
	awk -f shaderhack.awk console.cpp.in > console.cpp

# install link
library_nonversioned_includedir = $(includedir)/$(PACKAGE)
install-data-hook:
	@test -L $(library_nonversioned_includedir) && echo "   !!!   The nonversioned library include dir <$(library_nonversioned_includedir)> is already covered by a link. You'll have to update yourself". || true
	test -L $(library_nonversioned_includedir) || ln -s $(library_includedir) $(library_nonversioned_includedir)


