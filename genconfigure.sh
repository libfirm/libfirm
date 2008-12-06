#! /bin/sh

libtoolize --copy
aclocal
autoheader
automake --include-deps --add-missing --foreign --copy
autoconf
