#!/bin/bash
export CXXFLAGS="-O3"
export CFLAGS="-O3"
export CXX="g++-12"
export CC="gcc-12"

git pull

sudo apt update
sudo apt install -y autoconf make gcc texinfo libgtk-3-dev libxpm-dev \
     libjpeg-dev libgif-dev libtiff5-dev libgnutls28-dev libncurses5-dev \
     libjansson-dev libharfbuzz-dev libharfbuzz-bin imagemagick libmagickwand-dev libgccjit-12-dev libgccjit0 gcc-12 libjansson4 libjansson-dev xaw3dg-dev texinfo libx11-dev

./autogen.sh
./configure --enable-link-time-optimization\
            --with-json \
            --with-tree-sitter \
            --with-pgtk \
            --without-selinux \
            --with-modules \
            --with-harfbuzz \
            --with-compress-install \
            --with-native-compilation=aot \


make -j 8
