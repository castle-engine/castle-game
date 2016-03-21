# This Makefile uses castle-engine build tool for most operations.
# See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .
#
# Use this Makefile only with GNU make
# (GNU make is the standard make on Linux,
# on Windows it comes with FPC or Cygwin or MinGW,
# on FreeBSD it is gmake).

default: build

MODE:=debug

# Building target.
# You may wish to call target `clean' before
# calling build. This will make sure that everything is
# compiled with appropriate options (suitable for release or debugging).
#TODO: pass this -dCASTLE_WINDOW_BEST_NOGUI
#TODO: clean-window to force rebuilding CastleWindow unit with proper backend.
build:
	castle-engine compile --mode=$(MODE) $(CASTLE_ENGINE_TOOL_OPTIONS)

.PHONY: android
android:
	castle-engine package --os=android --cpu=arm --mode=$(MODE) --verbose
	castle-engine install --os=android --cpu=arm
	castle-engine run --os=android --cpu=arm

.PHONY: release-android
release-android:
	$(MAKE) android MODE=release

clean:
	castle-engine clean
	rm -Rf data/levels/fountain/fluidcache/

# make sure that various files are up-to-date
update:
	$(MAKE) -C code/
	$(MAKE) -C data/items/life_potion/
	$(MAKE) -C data/items/sword/
	$(MAKE) -C data/levels/

# install --------------------------------------------------------------------

# Standard installation dirs, following conventions on
# http://www.gnu.org/prep/standards/html_node/Directory-Variables.html#Directory-Variables
PREFIX=$(DESTDIR)/usr/local
EXEC_PREFIX=$(PREFIX)
BINDIR=$(EXEC_PREFIX)/bin
DATAROOTDIR=$(PREFIX)/share
DATADIR=$(DATAROOTDIR)

# Simple install.
.PHONY: install
install:
	install castle $(BINDIR)
	cd data/ && \
	  find . -type f -exec install -D '{}' $(DATADIR)/castle/'{}' ';'

.PHONY: uninstall
uninstall:
	rm -f  $(BINDIR)/castle
	rm -Rf $(DATADIR)/castle
