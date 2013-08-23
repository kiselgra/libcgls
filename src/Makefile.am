lib_LTLIBRARIES = libcgls.la
bin_PROGRAMS = viewer-cgls #tsm

BUILT_SOURCES = drawelement.xx scene.xx material.xx picking.xx light.xx console.xx skeletal.xx path.xx camera-animation.xx command-animation.xx
BUILT_SOURCES += objloader.x cmdline.x material.x drawelement.x scene.x console.x modelloader-scm.x skeletal.x path.x stock-shader.x camera-animation.x light.x command-animation.x
BUILT_SOURCES += stock-shader.c picking.c console.cpp sky.c

EXTRA_DIST = stock-shader.c.in picking.c.in console.cpp.in sky.c.in

# libconfigfiles_la_SOURCES = grammar.yy  lexer.ll  parser.h  token.h lexer.h config_creator.h configfile.h configfile.cpp
libcgls_la_SOURCES = 	\
						animation.c animation.h \
						basename.h \
						c-utils.c c-utils.h \
						camera-animation.c camera-animation.h \
						cgls.c cgls.h \
						cgls-config.h \
						command-animation.c command-animation.h \
						console.cpp console.h \
						drawelement.c drawelement.h \
						interaction.c interaction.h \
						light.c light.h \
						material.c material.h \
						modelloader.cpp modelloader-scm.c modelloader.h \
						objloader.c objloader.h \
						path.c path.h \
						picking.c picking.h \
						refs.h \
						scene.c scene.h \
						skeletal.c skeletal.h \
						sky.c sky.h \
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
library_include_HEADERS = drawelement.h objloader.h scene.h c-utils.h basename.h material.h picking.h stock-shader.h uniforms.h cgls.h light.h interaction.h console.h cgls-config.h skeletal.h path.h animation.h refs.h camera-animation.h command-animation.h

# setting c++ flags:
# AM_CXXFLAGS = -Wall -ansi -pedantic -ggdb -pg
AM_CXXFLAGS = -ffor-scope

# setting c preprocessor flags for main project
viewer_cgls_CPPFLAGS = -D PACKAGE=\"$(PACKAGE)\" -D VERSION=\"$(VERSION)\" -D DATADIR=\"$(pkgdatadir)\" -D_GNU_SOURCE @GUILE_DEF@ $(GUILE_CFLAGS) $(CGL_CFLAGS) $(AI_CFLAGS)
viewer_cgls_LDFLAGS = $(GUILE_LDFLAGS) $(CGL_LIBS) $(AI_LIBS)

#tsm_CPPFLAGS = -D PACKAGE=\"$(PACKAGE)\" -D VERSION=\"$(VERSION)\" -D DATADIR=\"$(pkgdatadir)\" -D_GNU_SOURCE @GUILE_DEF@ $(GUILE_CFLAGS) $(CGL_CFLAGS)
#tsm_LDFLAGS = $(GUILE_LDFLAGS) $(CGL_LIBS)

libcgls_la_CPPFLAGS = -D PACKAGE=\"$(PACKAGE)\" -D VERSION=\"$(VERSION)\" -D DATADIR=\"$(pkgdatadir)\" -D_GNU_SOURCE @GUILE_DEF@ $(GUILE_CFLAGS) $(CGL_CFLAGS) $(AI_CFLAGS)
libcgls_la_LIBADD = $(GUILE_LDFLAGS) $(CGL_LIBS) $(AI_LIBS)

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

sky.c: sky.c.in
	awk -f shaderhack.awk sky.c.in > sky.c

console.cpp: console.cpp.in
	awk -f shaderhack.awk console.cpp.in > console.cpp

# install link
library_nonversioned_includedir = $(includedir)/$(PACKAGE)
install-data-hook:
	@test -L $(library_nonversioned_includedir) && echo "   !!!   The nonversioned library include dir <$(library_nonversioned_includedir)> is already covered by a link. You'll have to update yourself". || true
	test -L $(library_nonversioned_includedir) || ln -s $(library_includedir) $(library_nonversioned_includedir)


