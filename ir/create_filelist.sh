#!/bin/bash
#
# Little helper script used to create the file list for Makefile.am
# automatically

DIRS="adt ana arch be common config debug external ident ir libcore lower net obstack opt stat tr tv"

echo "libfirm_la_SOURCES = \\" > FILELIST
for dir in $DIRS; do
	for f in $dir/*.c; do
		echo -e "\t$f \\" >> FILELIST
	done
done

echo "" >> FILELIST
echo "EXTRA_DIST = \\" >> FILELIST
FILES=`find $DIRS -maxdepth 1 -name "*.h" -o -name "*.def" -o -name "*.sh"`
for f in $FILES; do
	echo -e "\t$f \\" >> FILELIST
done
for f in be/scripts/*.pl; do
	echo -e "\t$f \\" >> FILELIST
done
