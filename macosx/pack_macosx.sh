#!/bin/bash
set -eu

# Create Mac OS X bundle, and then dmg (disk image) file to distribute castle game.
# Compile the castle binary before calling this.

# TODO: this is unused for now: running X11 programs by double-clicking on bundle
# doesn't seem to work under Mac OS X...
# Also, this would need to be adjusted to copy our data/ inside bundle Contents/Resources/data.

. ../../scripts/create_macosx_bundle.sh

create_bundle castle ../castle castle.icns ''

# add libraries from fink
cd castle.app/Contents/MacOS/

cp_fink_lib libpng14.14.dylib
cp_fink_lib libvorbisfile.3.dylib
cp_fink_lib libvorbis.0.dylib
cp_fink_lib libogg.0.dylib

install_name_tool -change /sw/lib/libvorbis.0.dylib @executable_path/libvorbis.0.dylib libvorbisfile.3.dylib
install_name_tool -change /sw/lib/libogg.0.dylib    @executable_path/libogg.0.dylib    libvorbisfile.3.dylib
install_name_tool -change /sw/lib/libogg.0.dylib    @executable_path/libogg.0.dylib    libvorbis.0.dylib

check_libs_not_depending_on_fink castle

cd ../../../

make -f ../../scripts/macosx_dmg.makefile NAME=castle
