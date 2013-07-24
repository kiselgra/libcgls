
SUBDIRS = src doc

.PHONY: doc run
doc:
	cd doc; make doc

run:
	cd src; make
	./src/viewer-cgls


BUILT_SOURCES = revision

revision:
	git log -n1 | head -n1 > $@

revisiondir = $(pkgdatadir)
revision_DATA = revision

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libcgls.pc

#EXTRA_DIST += libcgl.pc revision

scheme_DATA = default.c.scm default.scm default.shader dp.scm std.scm depth-array-oit.scm tsm.scm poster.scm tess.scm
shader_DATA = tsm.shader poster.shader tess.shader

schemedir = $(pkgdatadir)
shaderdir = $(pkgdatadir)
EXTRA_DIST = $(scheme_DATA)
