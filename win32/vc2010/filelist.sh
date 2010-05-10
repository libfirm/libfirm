#!/bin/bash
#
# Little helper script used to create the file list for Makefile.am
# automatically

DIRS="ir/adt ir/ana ir/be ir/common ir/debug ir/ident ir/ir ir/libcore ir/lower ir/obstack ir/opt ir/stat ir/tr ir/tv win32 include/libfirm include/libfirm/adt ir/be/ia32 ir/be/arm ir/be/mips ir/be/ppc32 ir/be/amd64 ir/be/sparc ir/be/TEMPLATE"

FILELIST="`pwd`/FILELIST"
FILELIST2="`pwd`/FILELIST2"

rm -f "$FILELIST" "$FILELIST2"
echo "  <ItemGroup>" > "$FILELIST"
pushd ../.. > /dev/null
for dir in $DIRS; do
	d=`echo $dir | tr / '\\\\'`
	for f in $dir/*.c; do
		if [ "$f" = '*.c' -o "$f" = '*.h' ]; then
			continue
		fi
		f=`echo $f | tr / '\\\\'`
		echo "    <ClCompile Include=\"\$(FirmRoot)\\$f\" />" >> "$FILELIST"

		echo "    <ClCompile Include=\"\$(FirmRoot)\\$f\">" >> "$FILELIST2"
		echo "       <Filter>$d</Filter>" >> $FILELIST2
		echo "    </ClCompile>" >> $FILELIST2
	done
	for f in $dir/*.h; do
		if [ "$f" = '*.c' -o "$f" = '*.h' ]; then
			continue
		fi
		f=`echo $f | tr / '\\\\'`
		echo "    <ClInclude Include=\"\$(FirmRoot)\\$f\" />" >> "$FILELIST"

		echo "    <ClInclude Include=\"\$(FirmRoot)\\$f\">" >> "$FILELIST2"
		echo "      <Filter>$d</Filter>" >> $FILELIST2
		echo "    </ClInclude>" >> $FILELIST2
	done
done

CUSTOMBUILD='ir/be/ia32/ia32_spec.pl ir/be/arm/arm_spec.pl ir/be/ppc32/ppc32_spec.pl ir/be/mips/mips_spec.pl ir/be/TEMPLATE/TEMPLATE_spec.pl ir/be/sparc/sparc_spec.pl ir/be/amd64/amd64_spec.pl scripts/gen_ir_io.py scripts/ir_spec.py'
echo "$CUSTOMBUILD" > /tmp/custombuilds
for f in scripts/*.py ir/be/scripts/*.pl ir/ir/irflag_t.def ir/be/*/*_spec.pl; do
	d=`dirname $f | tr / '\\\\'`

	if grep "$f" /tmp/custombuilds > /dev/null; then
		# already in custombuild ?
		continue
	fi
	f=`echo $f | tr / '\\\\'`
	echo "    <None Include=\"\$(FirmRoot)\\$f\" />" >> "$FILELIST"

	echo "    <None Include=\"\$(FirmRoot)\\$f\">" >> "$FILELIST2"
	echo "      <Filter>$d</Filter>" >> $FILELIST2
	echo "    </None>" >> $FILELIST2
done

for f in $CUSTOMBUILD; do
	d=`dirname $f | tr / '\\\\'`
	f=`echo $f | tr / '\\\\'`
	echo "    <CustomBuild Include=\"\$(FirmRoot)\\$f\">" >> "$FILELIST2"
	echo "      <Filter>$d</Filter>" >> $FILELIST2
	echo "    </CustomBuild>" >> $FILELIST2
done

echo "  </ItemGroup>" >> "$FILELIST2"
echo "</Project>" >> "$FILELIST2"

popd > /dev/null
