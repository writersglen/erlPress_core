#!/usr/bin/env bash

cd `dirname $BASH_SOURCE`

echo "           _ _____"                     1>&2
echo "          | |  __ \\"                   1>&2
echo "  ___ _ __| | |__) | __ ___  ___ ___"   1>&2
echo " / _ \ '__| |  ___/ '__/ _ \/ __/ __|"  1>&2
echo "|  __/ |  | | |   | | |  __/\__ \__ \\" 1>&2
echo " \___|_|  |_|_|   |_|  \___||___/___/"  1>&2
echo                                          1>&2
echo                                          1>&2

exec erl                  \
     -pa $PWD/ebin        \
     -pa $PWD/deps/*/ebin \
     -- $@
