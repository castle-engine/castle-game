.PHONY: default apidoc clean

default:
	@echo 'Nothing to do by default in this Makefile right now'

# Generate docs.
# This is just for fun (I'm a pasdoc developer after all...),
# this documentation is not really polished now (@noAutoLinkHere
# should be added in many places).
apidoc:
	mkdir -p apidoc/
	pasdoc --output=apidoc/ \
	  --include $(CASTLE_ENGINE_PATH)/castle_game_engine/src/base/ \
	  --define PASDOC \
	  --define LINUX \
	  --define UNIX \
	  --define FPC \
	  --define VER2 \
	  --define VER2_6 \
	  --define VER2_6_4 \
	  --write-uses-list \
	  --title 'The Castle units' \
	  --auto-abstract \
	  --auto-link \
	  --graphviz-classes --graphviz-uses \
	  --link-gv-classes jpg --link-gv-uses jpg \
	  *.pas
	dot -Grankdir=LR -Tjpg -oapidoc/GVClasses.jpg apidoc/GVClasses.dot
	dot -Grankdir=LR -Tjpg -oapidoc/GVUses.jpg apidoc/GVUses.dot

clean:
	rm -Rf apidoc *.o *.ppu
