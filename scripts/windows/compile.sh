#!/bin/sh
FOLDERS="platform/src \
         cloud_client/src \
         externals/miniz \
         externals/pugixml \
         package_repository/src \
         dci/src \
         device_service/src \
         emscripten/src \
         "

export CONFIGURATIONS=release
export LINKAGES=static
export PLATFORM=emscripten
export FOLDERS
# export CXX=clang++
# export CC=clang


if [ $PLATFORM = linux ]; then
    # if CPLUS_INCLUDE_DIR is not empty, then export it
    export CPLUS_INCLUDE_PATH="/usr/include/c++:/usr/include"
    export C_INCLUDE_PATH="/usr/include"
fi

# export MAX_JOBS=1
make


if [ $PLATFORM = emscripten ]; then
    rm -rf motomoto/build/web/dci
    rm -rf motomoto/web/dci

    if [ $CONFIGURATIONS = release ]; then
        (cd motomoto/build/web; flutter build web && ln -s $PWD/../../../bin/emscripten-wasm32-$CONFIGURATIONS-static $PWD/dci && zip -r ../../go/web-server-content.zip .)
        CGO_ENABLED=0 go build -ldflags "-s -w" -o motomotoserver motomoto/go/motomoto-server.go; ./motomotoserver
    else
      ln -s $PWD/bin/emscripten-wasm32-$CONFIGURATIONS-static  $PWD/motomoto/web/dci
      (cd motomoto; flutter run -d web-server --web-port=8000 --web-header="Cross-Origin-Embedder-Policy=require-corp" \
                          --web-header="Cross-Origin-Opener-Policy=same-origin" \
                          --web-header="Content-Security-Policy=default-src 'self' 'unsafe-inline' 'unsafe-eval' *.gstatic.com *.enterprise.eposaudio.com *.eposstorage.com wss://127.0.0.1:41096/; frame-ancestors 'self';")
    fi
fi
