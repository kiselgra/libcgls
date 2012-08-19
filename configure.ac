#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.61)
AC_INIT([cgls], [0.0.1])
AM_INIT_AUTOMAKE

# name one of your source files here
AC_CONFIG_SRCDIR([src/main.c])
AC_CONFIG_HEADER([config.h])
AC_CONFIG_FILES(Makefile src/Makefile doc/Makefile doc/doxyfile doc/doxygen_mainpage.h)

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
AC_CHECK_LIB([cgl], [main])
AC_CHECK_LIB([glut], [main])

ifdef([GUILE_FLAGS], [can_use_guile="yes"], [can_use_guile="no"])
if test "x$can_use_guile" = "xyes" ; then
	GUILE_FLAGS
	GUILE_DEF="-DWITH_GUILE"
else
	GUILE_DEF=""
fi
AC_SUBST([GUILE_DEF])
AM_CONDITIONAL([CAN_USE_GUILE], [test "x$can_use_guile" = "xyes"])

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([inttypes.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_OUTPUT
