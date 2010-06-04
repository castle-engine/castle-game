# Use this Makefile only with GNU make
# (GNU make is the standard make on Linux,
# on Windows it comes with FPC or Cygwin or MinGW,
# on FreeBSD it is gmake).

# ------------------------------------------------------------
# Various targets.

# default: make sure that various files are up-to-date, and show info
default: info
	$(MAKE) -C source/
	$(MAKE) -C data/items/life_potion/
	$(MAKE) -C data/items/sword/
	$(MAKE) -C data/levels/

VERSION := $(shell castle --version)

info:
	@echo 'Version is '$(VERSION)

# Simple install.
# You may as well symlink to /usr/local/share/castle, for system-wide install.
install:
	rm -f $(HOME)/.castle.data
	ln -s $(shell pwd) $(HOME)/.castle.data

# ------------------------------------------------------------
# Building targets.
#
# You may wish to call target `clean' before
# calling build targets. This will make sure that everything is
# compiled with appropriate options (suitable for release or debugging).
#
# For some debug compilation features, use DEBUG=xxx make option:
# - DEBUG=t
#   makes normal debug build (debug checks, etc.)
# - DEBUG=valgrind
#   makes a compilation for profiling with valgrind (callgrind, massif).
#   This compiles -dRELEASE code, but still with debug symbols, line info etc.
#   for valgrind.
# - DEBUG=gprof
#   makes a compilation for profiling with gprof.
# Otherwise normal optimized release build will be done.

ifeq ($(DEBUG),t)
FPC_UNIX_OPTIONS := -dDEBUG
FPC_WINDOWS_OPTIONS := -dDEBUG
else

ifeq ($(DEBUG),valgrind)
FPC_UNIX_OPTIONS := -gl -gv -dRELEASE -dGLWINDOW_XLIB
FPC_WINDOWS_OPTIONS := -gl -gv -dRELEASE
else

ifeq ($(DEBUG),gprof)
FPC_UNIX_OPTIONS := -pg -dRELEASE -dGLWINDOW_XLIB
FPC_WINDOWS_OPTIONS := -pg -dRELEASE
else

FPC_UNIX_OPTIONS := -dRELEASE -dGLWINDOW_XLIB
FPC_WINDOWS_OPTIONS := -dRELEASE
endif
endif
endif

build-unix: clean-glwindow
	cd ../kambi_vrml_game_engine/ && \
	  fpc $(FPC_UNIX_OPTIONS) "$${KAMBI_FPC_OPTIONS:-}" \
	  @kambi.cfg ../castle/source/castle.lpr
	mv source/castle ./
	cd ../kambi_vrml_game_engine/ && \
	  fpc $(FPC_UNIX_OPTIONS) "$${KAMBI_FPC_OPTIONS:-}" \
	  @kambi.cfg ../castle/source/castle-process-3d-model.lpr
	mv source/castle-process-3d-model ./

build-windows: clean-glwindow
	cd ../kambi_vrml_game_engine/ && \
	  fpc $(FPC_WINDOWS_OPTIONS) "$${KAMBI_FPC_OPTIONS:-}" \
	  @kambi.cfg ../castle/source/castle.lpr
	mv source/castle.exe ./castle.exe
	cd ../kambi_vrml_game_engine/ && \
	  fpc $(FPC_WINDOWS_OPTIONS) "$${KAMBI_FPC_OPTIONS:-}" \
	  @kambi.cfg ../castle/source/castle-process-3d-model.lpr
	mv source/castle-process-3d-model.exe ./castle-process-3d-model.exe

# ------------------------------------------------------------
# Cleaning targets.

# Clean files which are easily recoverable, or just temporary trash
# (after compilers or editors).
# This does not include compiled "castle" binaries, but it *does*
# include "castle-process-3d-model" binaries (as I don't want to pack
# them in releases).
clean:
	find . -type f '(' -iname '*.ow'  -or -iname '*.ppw' -or -iname '*.aw' -or \
	                   -iname '*.o'   -or -iname '*.ppu' -or -iname '*.a' -or \
	                   -iname '*.dcu' -or -iname '*.dpu' -or \
			   -iname '*~' -or \
	                   -iname '*.~???' -or \
			   -iname 'castle.compiled' -or \
			   -iname '*.blend1' ')' -print \
	     | xargs rm -f
# I recurse into source/ subdir only if it exists ---
# this is useful because this may be called by update_archives.sh
# script inside a temporary copy of castle files, where source/
# subdirectory isn't supposed to exist.
	if [ -d source/ ]; then $(MAKE) -C source/ clean; fi
	rm -f castle-process-3d-model castle-process-3d-model.exe
	rm -Rf data/levels/fountain/fluidcache/

clean_binaries:
	rm -f castle castle.exe

# Remove private files that Michalis keeps inside his castle/trunk/,
# but he doesn't want to upload them for public.
#
# These things are *not* automatically generated (automatically generated
# stuff is removed always by `clean'). So this target is supposed to be
# used only by update_archives.sh and update_pascal_src.sh scripts,
# it does it inside temporary copy of castle/trunk/.
#
# Notes: I remove here data/sounds/intermediate/, because it's large
# and almost noone should need this. These files are downloadable from
# internet anyway, as they are just original things used to make
# some sounds.
clean_private:
	find . -type d '(' -iname '.svn' ')' -print \
	     | xargs rm -Rf
	rm -Rf data/sounds/intermediate/

# Force rebuilding GLWindow unit with proper backend.
clean-glwindow:
	$(MAKE) -C ../kambi_vrml_game_engine/ clean-glwindow

# ----------------------------------------
# Set SVN tag.

svntag:
	svn copy https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/castle \
	         https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/tags/castle/$(VERSION) \
	  -m "Tagging the $(VERSION) version of 'The Castle'."

# eof ------------------------------------------------------------
