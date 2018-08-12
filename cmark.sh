#!/usr/bin/env bash

set -e

CMARK_DIR="$PWD/src/content/cmark_parse/cmark"
make -C ${CMARK_DIR}

mkdir -p priv
cp -f ${CMARK_DIR}/build/src/cmark $PWD/priv/cmark
