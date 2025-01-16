#!/bin/sh
FOLDERS="platform/src \
         cloud_client/src \
         externals/miniz \
         externals/pugixml \
         package_repository/src \
         dci/src \
         device_service/src \
         py_device_service/src"
# FOLDERS+="emscripten/src"

export CONFIGURATIONS=release
export LINKAGES=static
export PLATFORM=linux
export FOLDERS
# export CXX=clang++
# export CC=clang

CPLUS_INCLUDE

if [ $PLATFORM = linux ]; then
    # if CPLUS_INCLUDE_DIR is not empty, then export it
    export CPLUS_INCLUDE_PATH="/usr/include/c++:/usr/include"
    export C_INCLUDE_PATH="/usr/include"
fi

# export MAX_JOBS=1
make


if [ $PLATFORM = emscripten ]; then
   (cd motomoto/build/web; flutter build web && zip -u -r ../../go/web-server-content.zip .)
   CGO_ENABLED=0 go build -ldflags "-s -w" -o motomotoserver motomoto/go/motomoto-server.go; ./motomotoserver
fi

