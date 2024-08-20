#!/bin/bash
# A guide https://gist.github.com/nauhygon/f3b44f51b34e89bc54f8?permalink_comment_id=4456120

target=/c/emacs
source=/c/Git/emacs-29

# export PATH=/mingw64/bin:$PATH

mkdir build; cd build

(cd $source; ./autogen.sh)

# export PKG_CONFIG_PATH=/mingw64/lib/pkgconfig

export CFLAGS='
    -I/mingw64/include/noX
    -O3
    -fno-math-errno
    -funsafe-math-optimizations
    -fno-finite-math-only
    -fno-trapping-math
    -freciprocal-math
    -fno-rounding-math
    -fno-signaling-nans
    -fassociative-math
    -fno-signed-zeros
    -frename-registers
    -funroll-loops
    -mtune=native
    -march=native
    -fomit-frame-pointer
    -fallow-store-data-races
    -fno-semantic-interposition
    -floop-parallelize-all
    -ftree-parallelize-loops=4
'

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
