# This Makefile uses castle-engine build tool for most operations.
# See https://sourceforge.net/p/castle-engine/wiki/Build%20tool/ .
#
# Use this Makefile only with GNU make
# (GNU make is the standard make on Linux,
# on Windows it comes with FPC or Cygwin or MinGW,
# on FreeBSD it is gmake).

default: build

# Building target.
# You may wish to call target `clean' before
# calling build. This will make sure that everything is
# compiled with appropriate options (suitable for release or debugging).
#TODO: pass this -dCASTLE_WINDOW_BEST_NOGUI
#TODO: clean-window to force rebuilding CastleWindow unit with proper backend.
build:
	castle-engine compile

clean:
	castle-engine clean
	rm -Rf data/levels/fountain/fluidcache/

# make sure that various files are up-to-date
update:
	$(MAKE) -C source/
	$(MAKE) -C data/items/life_potion/
	$(MAKE) -C data/items/sword/
	$(MAKE) -C data/levels/

# Simple install.
# You may as well symlink data to /usr/local/share/castle,
# for system-wide install.
install:
	rm -f $(HOME)/.local/share/castle
	ln -s $(shell pwd)/data $(HOME)/.local/share/castle

system-install:
	sudo rm -f /usr/local/share/castle
	sudo ln -s $(shell pwd)/data /usr/local/share/castle
