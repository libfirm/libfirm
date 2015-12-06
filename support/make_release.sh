#!/bin/sh

set -eu

WORKDIR="release"
VERSION_MAJOR="1"
VERSION_MINOR="22"
VERSION_MICRO="0"
VERSION="${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_MICRO}"
RELEASEDIR="libfirm-$VERSION"
FULLRELEASEDIR="$WORKDIR/$RELEASEDIR"
RELEASEFILE="libfirm-$VERSION.tar.bz2"
VERSIONFILE="../ir/common/version.h"

# test if versions match
echo "Checking for version mismatch"
egrep -q "#define libfirm_VERSION_MAJOR\\s*$VERSION_MAJOR" $VERSIONFILE
egrep -q "#define libfirm_VERSION_MINOR\\s*$VERSION_MINOR" $VERSIONFILE
egrep -q "#define libfirm_VERSION_MICRO\\s*$VERSION_MICRO" $VERSIONFILE

rm -rf "$FULLRELEASEDIR"
echo "Creating $RELEASEFILE"
git archive HEAD | bzip2 > "$RELEASEFILE"
