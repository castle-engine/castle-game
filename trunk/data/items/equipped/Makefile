ALL_OUTPUT := sword.png

all: $(ALL_OUTPUT)

sword.png: sword.wrl ../models/sword.wrl
	rayhunter classic 3 800 600 $< $@
	convert $@ -transparent black -crop 300x200+500+400 $@

clean:
	rm -f $(ALL_OUTPUT)