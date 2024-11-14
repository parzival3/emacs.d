#!/bin/sh
set -ex
CMD="env \
    LD_LIBRARY_PATH=/lib \
    SSL_CERT_DIR=/etc/ssl/certs \
    PLATFORM=emscripten \
    CONFIGURATIONS=release \
    LINKAGES=static \
    CPLUS_INCLUDE_PATH= \
    C_INCLUDE_PATH= \
    EMSDK_HOME=/home/enrico/tools/emsdk/emsdk_env.sh \
    FLUTTER_HOME=/home/enrico/tools/flutter/bin"

exec guix shell -C --pure  -F -N -E ^DISPLAY --share=/home/enrico/tools/flutter \
    --expose=/home/enrico/tools/emsdk \
    --share=/home/enrico/tools/pub-cache=/home/enrico/.pub-cache \
    --share=/home/enrico/tools/config=/home/enrico/.config \
    --share=/home/enrico/tools/dot-flutter=/home/enrico/.flutter \
    --share="$PWD" -m "$0" -- \
    $CMD bash --init-file <(echo 'source $EMSDK_HOME' && echo 'PATH=$PATH:$FLUTTER_HOME:$HOME/.pub-cache/bin'; echo "$@")

!#
(specifications->manifest (list "bash" "curl" "unzip" "git" "make" "findutils" "gzip" "python" "binutils" "coreutils" "nss-certs" "tar" "xz" "grep" "node" "glibc" "gcc-toolchain" "fd" "which" "zlib" "pkg-config"))
