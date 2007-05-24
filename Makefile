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

VERSION := $(shell ./castle --version)

info:
	@echo 'Version is '$(VERSION)

# ------------------------------------------------------------
# Building targets.
#
# You may wish to call target `clean' before
# calling build targets. This will make sure that everything is
# compiled with appropriate options (suitable for release or debugging,
# and that GLWindow unit uses proper backend).
#
# Call make with DEBUG=t to get debug build, otherwise release build
# will be done.

ifdef DEBUG
FPC_UNIX_OPTIONS := -dDEBUG
FPC_WIN32_OPTIONS := -dDEBUG
else
FPC_UNIX_OPTIONS := -dRELEASE -dGLWINDOW_XLIB
FPC_WIN32_OPTIONS := -dRELEASE
endif

build-unix:
	cd source/kambi_vrml_game_engine/; \
	  fpc $(FPC_UNIX_OPTIONS) @kambi.cfg ../castle.dpr
	mv source/castle ./

build-win32:
	cd source/kambi_vrml_game_engine/; \
	  fpc $(FPC_WIN32_OPTIONS) @kambi.cfg ../castle.dpr
	mv source/castle.exe ./castle.exe

# ------------------------------------------------------------
# Cleaning targets.

clean:
	find . -type f '(' -iname '*.ow'  -or -iname '*.ppw' -or -iname '*.aw' -or \
	                   -iname '*.o'   -or -iname '*.ppu' -or -iname '*.a' -or \
	                   -iname '*.dcu' -or -iname '*.dpu' -or \
			   -iname '*~' -or \
	                   -iname '*.~???' -or \
			   -iname '*.blend1' ')' -print \
	     | xargs rm -f
	rm -f castle-*.tar.gz
# I recurse into source/ subdir only if it exists ---
# this is useful because this may be called by update_archives.sh
# script inside a temporary copy of castle files, where source/
# subdirectory isn't supposed to exist.
	if [ -d source/ ]; then $(MAKE) -C source/ clean; fi

# Remove private files that Michalis keeps inside his castle/trunk/,
# but he doesn't want to upload them for public.
#
# These things are *not* automatically generated (automatically generated
# stuff is removed always by `clean'). So this target is supposed to be
# used only by `make dist', it does it inside temporary copy of castle/trunk/.
#
# Notes: I remove here data/sounds/intermediate/, because it's large
# and almost noone should need this. These files are downloadable from
# internet anyway, as they are just original things used to make
# some sounds.
clean_private:
	find . -type d '(' -iname '.svn' ')' -print \
	     | xargs rm -Rf
	rm -Rf data/sounds/intermediate/

# ------------------------------------------------------------
# Dist making.

TMP_DIST_PATH := /tmp/castle_dist_tmp/

# Uncomment to get bzip2 packed dist
#BZIP2 := t

ifdef BZIP2
DIST_EXTENSION := bz2
DIST_TAR_FILTER := --bzip2
else
DIST_EXTENSION := gz
DIST_TAR_FILTER := --gzip
endif

ifdef DIST_WITH_SRC
DIST_ARCHIVE_FILENAME := castle-with-sources-$(VERSION).tar.$(DIST_EXTENSION)
else
DIST_ARCHIVE_FILENAME := castle-$(VERSION).tar.$(DIST_EXTENSION)
endif

DOCUMENTATION_HTML_FILES := openal_notes.html \
  opengl_options.html common_options.html \
  castle.html castle-advanced.html castle-development.html castle-credits.html

# Make distribution tar.gz to upload for PGD competition.
# For now, this target is not supposed to be run by anyone
# else than me (Michalis), because it depends on some private
# scripts of mine and directory layout
# (in particular, I include here my general units, that are
# packed into tar.gz using my private script).
#
# Before doing this target, remember to
# - make sure Version in castlehelp.pas is correct
# - recompile castle for Linux and Windows
dist:
	$(MAKE) DIST_WITH_SRC=t dist-core
	$(MAKE) dist-core

# This is internal that actually does all the work of dist.
# Only tha dist target should call this.
dist-core:
# Start with empty $(TMP_DIST_PATH)
	rm -Rf $(TMP_DIST_PATH)
	mkdir -p $(TMP_DIST_PATH)
# Copy and clean castle/trunk/ directory
	cp -R ../trunk/ $(TMP_DIST_PATH)
	mv $(TMP_DIST_PATH)trunk/ $(TMP_DIST_PATH)castle
	make -C $(TMP_DIST_PATH)castle/ clean clean_private
	areFilenamesLower -i Makefile -i Makefile.common -i README $(TMP_DIST_PATH)castle/data/
# Add libpng, zlib, openal for Windows
	cp -f /win/mojewww/camelot/private/update_archives/win32_dlls/* $(TMP_DIST_PATH)castle/
# Add documentation/ subdirectory
	mkdir $(TMP_DIST_PATH)castle/documentation/
#         Trzeba najpierw zrobiæ make clean bo dotychczasowe le¿±ce tam HTMLe
#         mog³y byæ wygenerowane z innymi LOCALLY_AVAIL
	cd $(CAMELOT_LOCAL_PATH)private/local_html_versions/; \
	  $(MAKE) clean ; \
	  $(MAKE) $(DOCUMENTATION_HTML_FILES) LOCALLY_AVAIL="$(DOCUMENTATION_HTML_FILES)" ; \
	  cp $(DOCUMENTATION_HTML_FILES) $(TMP_DIST_PATH)castle/documentation/
# Setup right permissions of things (in castle/trunk/ and libpng/zlib)
# (because they are kept on FAT filesystem)
	find $(TMP_DIST_PATH) -type f -and -exec chmod 644 '{}' ';'
	find $(TMP_DIST_PATH) -type d -and -exec chmod 755 '{}' ';'
	find $(TMP_DIST_PATH) -type f -and -iname '*.sh' -and -exec chmod 755 '{}' ';'
	chmod 755 $(TMP_DIST_PATH)castle/castle
# Copy and clean kambi_vrml_game_engine sources
ifdef DIST_WITH_SRC
	cd /win/mojewww/camelot/private/update_archives/; ./update_pascal_src.sh kambi_vrml_game_engine
	cp /win/mojewww/camelot/src/pascal/kambi_vrml_game_engine-src.tar.gz $(TMP_DIST_PATH)castle/source/
	cd $(TMP_DIST_PATH)castle/source/; tar xzf kambi_vrml_game_engine-src.tar.gz
	rm -f $(TMP_DIST_PATH)castle/source/kambi_vrml_game_engine-src.tar.gz
	mv $(TMP_DIST_PATH)castle/source/COPYING $(TMP_DIST_PATH)castle/COPYING
else
	cp /usr/share/common-licenses/GPL-2 $(TMP_DIST_PATH)castle/COPYING
endif
# If not with sources, clean some things that should be only in sources
ifndef DIST_WITH_SRC
	rm -Rf $(TMP_DIST_PATH)castle/source/
	find $(TMP_DIST_PATH)castle/ \
	  '(' '(' -type f -iname '*.blend' ')' -or \
	      '(' -type f -iname 'Makefile' ')' -or \
	      '(' -type f -iname '*.xcf' ')' -or \
	      '(' -type f -iname '*.sh' ')' -or \
	      '(' -type f -iname '*.el' ')' \
	  ')' -exec rm -f '{}' ';'
endif
# Pack things
	cd $(TMP_DIST_PATH); tar -c $(DIST_TAR_FILTER) -f \
	  $(DIST_ARCHIVE_FILENAME) castle/
	mv $(TMP_DIST_PATH)$(DIST_ARCHIVE_FILENAME) .

# ----------------------------------------
# Set SVN tag.

svntag:
	svn copy file:///home/michal/svn/kambi-svn-repos/castle/trunk/ \
	         file:///home/michal/svn/kambi-svn-repos/castle/tags/$(VERSION) \
	  -m "Tagging the $(VERSION) version of 'The Castle'."

# eof ------------------------------------------------------------