#!/bin/bash
# A guide https://gist.github.com/nauhygon/f3b44f51b34e89bc54f8?permalink_comment_id=4456120

target=/c/emacs
source=/c/Git/emacs-29

# export PATH=/mingw64/bin:$PATH

mkdir build; cd build

(cd $source; ./autogen.sh)

export PKG_CONFIG_PATH=/mingw64/lib/pkgconfig

CFLAGS+=" -I/mingw64/include/noX"
CFLAGS+=" -O3"
CFLAGS+=" -fno-math-errno" \
CFLAGS+=" -funsafe-math-optimizations"
CFLAGS+=" -fno-finite-math-only"
CFLAGS+=" -fno-trapping-math"
CFLAGS+=" -freciprocal-math"
CFLAGS+=" -fno-rounding-math"
CFLAGS+=" -fno-signaling-nans"
CFLAGS+=" -fassociative-math"
CFLAGS+=" -fno-signed-zeros"
CFLAGS+=" -frename-registers"
CFLAGS+=" -funroll-loops"
CFLAGS+=" -mtune=native"
CFLAGS+=" -march=native"
CFLAGS+=" -fomit-frame-pointer"
CFLAGS+=" -fallow-store-data-races"
CFLAGS+=" -fno-semantic-interposition"
CFLAGS+=" -floop-parallelize-all"
CFLAGS+=" -ftree-parallelize-loops=4"

$source/configure \
    --build=x86_64-w64-mingw32 \
    --host=x86_64-w64-mingw32 \
    --target=x86_64-w64-mingw32 \
    --with-gnutls \
    --with-jpeg \
    --with-json \
    --with-modules \
    --with-native-compilation \
    --with-png \
    --with-rsvg \
    --with-tiff \
    --with-tree-sitter \
    --with-wide-int \
    --with-xft \
    --with-xml2 \
    --with-xpm \
    --without-compress-install \
    --without-gconf \
    --without-gsettings \
    --without-imagemagick \
    --without-pop \
    -without-dbus \
    prefix=$target


make

make install prefix=$target
