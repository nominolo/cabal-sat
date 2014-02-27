#!/bin/bash

set -o errexit

# Usage: $0 

SRC="$1"

TMP=/tmp/
if [ -n ${TMPDIR} ]; then
    TMP=${TMPDIR}
fi

DIR=$(mktemp -d ${TMP}build.XXXXXX)
[ -n ${DIR} ] || exit 1

echo "=== Trying build from fresh checkout ==========="
echo "=== .. in directory: ${DIR}"
git clone ${SRC} ${DIR}/test

cd ${DIR}/test
make

echo "=== Build successful ==========================="
echo "=== .. removing directory: ${DIR}"
rm -rf ${DIR}
