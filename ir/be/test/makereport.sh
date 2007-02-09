#!/bin/sh

unset LANG
unset LC_ALL
unset LC_COLLATE
ECC="eccp"
ECC_CFLAGS="${ADDCFLAGS} -O3 -c -D__builtin_memcpy=memcpy -D__builtin_memset=memset -D__builtin_strlen=strlen -D__builtin_strcpy=strcpy -D__builtin_strcmp=strcmp -DNO_TRAMPOLINES"
GCC_CFLAGS="-O3 -g -fomit-frame-pointer"
LINKFLAGS="-lm"
TIMEOUT_COMPILE=300
TIMEOUT_RUN=30

CFILES="*.c"
OUTPUTDIR="stats-`date +%d.%m.%y`"

mkdir -p build_firm
mkdir -p build_gcc
mkdir -p $OUTPUTDIR

XMLRES=$OUTPUTDIR/result.xml
cat > $XMLRES << __END__
<?xml version="1.0"?>
<results>
    <environment>
        <ECC_CFLAGS>${ECC_CFLAGS}</ECC_CFLAGS>
        <GCC_CFLAGS>${GCC_CFLAGS}</GCC_CFLAGS>
    </environment>
__END__

# so endless apps stop at some point...
#ulimit -t2

basedir=`pwd`

DIRS=". gcc-testsuite gcc-testsuite/ieee langshootout"
test -n "$1" && DIRS="$1"

for dir in $DIRS; do
	curdir=$basedir/$dir
    echo "<section name=\"$curdir/\">" >> $XMLRES
for file in $curdir/$CFILES; do
    COMPILE_RES="ok"
    LINK_RES="omitted"
    GCC_RES="ok"
    GCC_RUN_RES="omitted"
    FIRM_RUN_RES="omitted"
    DIFF_RES="omitted"

    name="`basename $file .c`"
    res="$OUTPUTDIR/buildresult_$name.txt"
    echo "Building $name"
    echo "Results for $name" > $res
    echo "*** ECC/FIRM Compile" >> $res
    CMD="ulimit -t${TIMEOUT_COMPILE} ; ${ECC} ${ECC_CFLAGS} $file"
    echo "$CMD" >> $res
    /bin/bash -c "ulimit -t${TIMEOUT_COMPILE} ; ${ECC} ${ECC_CFLAGS} $file" >> $res 2>&1 || COMPILE_RES="failed"

    if [ ${COMPILE_RES} == "ok" ]; then
        LINK_RES="ok"
        CMD="mv $curdir/$name.s build_firm/$name.s"
        echo "$CMD" >> $res
        $CMD >> $res 2>&1
        echo "*** Linking" >> $res
        CMD="${ECC} build_firm/$name.s ${LINKFLAGS} -o build_firm/$name.exe"
        echo "$CMD" >> $res
        $CMD >> $res 2>&1 || LINK_RES="failed"
    fi

    echo "*** GCC Compile" >> $res
    CMD="gcc ${GCC_CFLAGS} $file ${LINKFLAGS} -o build_gcc/$name.exe"
    echo "$CMD" >> $res
    $CMD >> $res 2>&1 || GCC_RES="failed"

    if [ ${GCC_RES} = "ok" ]; then
        GCC_RUN_RES="ok"

        echo "*** Run GCC" >> $res
        CMD="ulimit -t${TIMEOUT_RUN} ; build_gcc/$name.exe > $OUTPUTDIR/result_gcc_$name.txt 2>&1"
        echo "$CMD" >> $res
        /bin/bash -c "ulimit -t${TIMEOUT_RUN} ; build_gcc/$name.exe" > $OUTPUTDIR/result_gcc_$name.txt 2>&1 || GCC_RUN_RES="failed"
    fi

    if [ ${LINK_RES} = "ok" ]; then
        FIRM_RUN_RES="ok"

        echo "*** Run Firm" >> $res
        CMD="ulimit -t${TIMEOUT_RUN} ; build_firm/$name.exe > $OUTPUTDIR/result_gcc_$name.txt 2>&1"
        echo "$CMD" >> $res
        /bin/bash -c "ulimit -t${TIMEOUT_RUN} ; build_firm/$name.exe" > $OUTPUTDIR/result_firm_$name.txt 2>&1 || FIRM_RUN_RES="failed"
    fi

    if [ ${GCC_RUN_RES} = "ok" -a ${FIRM_RUN_RES} = "ok" ]; then
        DIFF_RES="ok"

        echo "*** Compare Results" >> $res
        CMD="diff -u $OUTPUTDIR/result_gcc_$name.txt $OUTPUTDIR/result_firm_$name.txt"
        $CMD > $OUTPUTDIR/result_diff_$name.txt 2>&1 || DIFF_RES="failed"
    fi

    cat >> $XMLRES << __END__
    <result name="$name">
        <compile>$COMPILE_RES</compile>
        <link>$LINK_RES</link>
        <gcc_compile>$GCC_RES</gcc_compile>
        <gcc_run>$GCC_RUN_RES</gcc_run>
        <firm_run>$FIRM_RUN_RES</firm_run>
        <diff>$DIFF_RES</diff>
    </result>
__END__
done
    echo "</section>" >> $XMLRES
done

echo "</results>" >> $XMLRES

xsltproc --output $OUTPUTDIR/index.html makehtml.xslt $XMLRES

# maybe execute custom actions after result has been generated
[ -e after_compile.sh ] && ./after_compile.sh "$OUTPUTDIR"
