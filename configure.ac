#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.61)
AC_INIT([libcgls], [0.0.1])
AM_INIT_AUTOMAKE

#AM_SILENT_RULES([yes])

# name one of your source files here
AC_CONFIG_SRCDIR([src/main.c])
AC_CONFIG_HEADER([config.h])
AC_CONFIG_FILES(Makefile src/Makefile doc/Makefile doc/doxyfile doc/doxygen_mainpage.h libcgls.pc)

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CC_C99
AM_PROG_CC_C_O

# when using flex/bison:
# AC_PROG_LEX
# AC_PROG_YACC

# when building libraries
AC_PROG_LIBTOOL

# Checks for libraries.
AC_CHECK_LIB([mcm], [main])
#AC_CHECK_LIB([cgl], [main])
AC_CHECK_LIB([glut], [main])

PKG_CHECK_MODULES(CGL, libcgl)
AC_SUBST(CGL_CFLAGS)
AC_SUBST(CGL_LDFLAGS)

cgldata="$(pkg-config --variable=datadir libcgl)"
AC_SUBST([cgldata])

ifdef([GUILE_FLAGS], [can_use_guile="yes"], [can_use_guile="no"])
if test "x$can_use_guile" = "xyes" ; then
	GUILE_FLAGS
	GUILE_DEF="-DWITH_GUILE"
else
	GUILE_DEF=""
fi
AC_SUBST([GUILE_DEF])
AM_CONDITIONAL([CAN_USE_GUILE], [test "x$can_use_guile" = "xyes"])

AC_ARG_ENABLE(gdb,
			[AS_HELP_STRING([--enable-gdb], [Wether to use debugging flags. Default: no.])],
			with_debug_flags="$enableval", with_debug_flags="no")
if test "x$with_debug_flags" == "xyes" ; then
	DEBUG_FLAGS="-ggdb3 -O0"
else
	DEBUG_FLAGS=""
fi
CFLAGS="$CFLAGS $DEBUG_FLAGS"
CXXFLAGS="$CXXFLAGS $DEBUG_FLAGS"

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([inttypes.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_OUTPUT
