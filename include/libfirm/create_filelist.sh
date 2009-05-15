#!/bin/bash
#
# Little helper script used to create the file list for Makefile.am
# automatically

rm -f FILELIST
echo 'libfirmincludedir=$(includedir)/libfirm' >> FILELIST
echo 'libfirminclude_HEADERS = \' >> FILELIST
for f in *.h; do
	echo -e "\t$f \\" >> FILELIST
done

echo "" >> FILELIST
echo "" >> FILELIST
echo 'libfirminclude_adtdir=$(includedir)/libfirm/adt' >> FILELIST
echo 'libfirminclude_adt_HEADERS = \' >> FILELIST
for f in adt/*.h; do
	echo -e "\t$f \\" >> FILELIST
done
echo "" >> FILELIST
