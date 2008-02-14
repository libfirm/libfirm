#!/bin/sh

invoke() {
	echo "$@"
	$@ || exit $?
}

# bison/flex make me crazy, all versions differ in subtle ways making it nearly
# impossible to distribute .l/.y files and integrate them into the build system
# so that they work everywhere. (All this ylwrap hackery from the automake guys
# just failed for me again). So now we do it differently: We distribute the
# generated C code and only people that actually change the .y and .l files
# invoke this script manually!
FLEX_FLAGS=
# we use -l for win32 compilers
BISON_FLAGS="-l -d"

invoke bison $BISON_FLAGS -o lc_config_parser.c lc_config_parser.y
invoke flex $FLEX_FLAGS -o lc_config_lexer.c lc_config_lexer.l
