
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

#pkgconfigdir = $(libdir)/pkgconfig
#pkgconfig_DATA = libcgl.pc

#EXTRA_DIST += libcgl.pc revision

scheme_DATA = default.scm default.shader dp.scm std.scm depth-array-oit.scm
schemedir = $(pkgdatadir)
EXTRA_DIST = $(scheme_DATA)
