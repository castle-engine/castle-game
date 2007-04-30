ALL_OUTPUT := equipped.png image.png

include ../Makefile.common

all: $(ALL_OUTPUT)

equipped.png: equipped.wrl sword.wrl
	rayhunter classic 3 800 600 $< $@
	convert $@ -transparent black -crop 300x200+500+400 $@

image.png: image.wrl sword.wrl
	$(IMAGE_RAYHUNTER_CALL) $< $@
	convert $@ -transparent black $@

clean:
	rm -f $(ALL_OUTPUT)