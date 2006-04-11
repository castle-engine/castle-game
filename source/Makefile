.PHONY: default apidoc clean

default:
	@echo 'Nothing to do by default in this Makefile right now'

UNITS_BASE_PATH := /win/mojepasy/units/

# Generate docs.
# This is just for fun (I'm a pasdoc developer after all...),
# this documentation is not really polished now (@noAutoLinkHere
# should be added in many places).
apidoc:
	mkdir -p apidoc/
	pasdoc --output=apidoc/ \
	  --define LINUX \
	  --define UNIX \
	  --define FPC \
	  --define VER2 \
	  --define VER2_0 \
	  --define VER2_0_2 \
	  --write-uses-list \
	  --title 'The Castle units' \
	  --auto-abstract \
	  --auto-link \
	  --include $(UNITS_BASE_PATH)base/templates/ \
	  *.pas

clean:
	rm -Rf apidoc