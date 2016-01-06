#!/bin/sh
set -eu

if ! test -e .git; then
	echo "Must be started from toplevel libfirm dir"
	exit 1
fi
# Check that our git checkout is clean (remember that we use git archive
# which will miss things uncommitted changes)
if [ "$(git status --porcelain)" != "" ]; then
	echo "Git checkout not clean!"
	exit 1
fi

WORKDIR="release"
VERSION_MAJOR="1"
VERSION_MINOR="22"
VERSION_MICRO="1"
VERSION="${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_MICRO}"
RELEASEFILE="build/libfirm-$VERSION.tar.bz2"
VERSIONFILE="ir/common/version.h"

# test if versions match
echo "Checking version in $VERSIONFILE"
egrep -q "#define libfirm_VERSION_MAJOR\\s*$VERSION_MAJOR" $VERSIONFILE
egrep -q "#define libfirm_VERSION_MINOR\\s*$VERSION_MINOR" $VERSIONFILE
egrep -q "#define libfirm_VERSION_MICRO\\s*$VERSION_MICRO" $VERSIONFILE
echo "Checking version in CMakeLists.txt"
grep -q "set(libfirm_VERSION \"${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_MICRO}\")" CMakeLists.txt
echo "Checking version in NEWS.md"
grep -q "$VERSION_MAJOR.$VERSION_MINOR.$VERSION_MICRO" NEWS.md

RELEASEBZ2="build/libfirm-$VERSION.tar.bz2"
echo "creating $RELEASEBZ2"
mkdir -p "$(dirname "$RELEASEBZ2")"
git archive --prefix libfirm-$VERSION/ HEAD | bzip2 -9 > "$RELEASEBZ2"

RELEASEGZ="build/libfirm-$VERSION.tar.gz"
echo "creating $RELEASEGZ"
mkdir -p "$(dirname "$RELEASEGZ")"
git archive --prefix libfirm-$VERSION/ HEAD | gzip -9 > "$RELEASEGZ"

RELEASEXZ="build/libfirm-$VERSION.tar.xz"
echo "creating $RELEASEXZ"
mkdir -p "$(dirname "$RELEASEXZ")"
git archive --prefix libfirm-$VERSION/ HEAD | xz -9 > "$RELEASEXZ"
