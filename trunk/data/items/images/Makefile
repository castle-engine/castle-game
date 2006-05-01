# Generating images in this directory that can be automatically generated.
#
# Note that some images cannot be generated automatically,
# see comments in their VRML files.

ALL_OUTPUTS := sword.png

all: $(ALL_OUTPUTS)

IMAGE_WIDTH := 95
IMAGE_HEIGHT := $(IMAGE_WIDTH)

RAYHUNTER_CALL := rayhunter classic 3 $(IMAGE_WIDTH) $(IMAGE_HEIGHT)

sword.png: sword.wrl ../models/sword.wrl
	$(RAYHUNTER_CALL) $< $@
	convert $@ -transparent black $@

clean:
	rm -f $(ALL_OUTPUTS)