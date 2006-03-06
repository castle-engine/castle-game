ALL_OUTPUTS := sword.png life_potion.png

all: $(ALL_OUTPUTS)

RAYHUNTER_CALL := rayhunter classic 3 95 95

sword.png: sword.wrl ../models/sword.wrl
	$(RAYHUNTER_CALL) $< $@
	convert $@ -transparent black $@

life_potion.png: life_potion.wrl ../models/life_potion_processed.wrl
	$(RAYHUNTER_CALL) $< $@
	convert $@ -transparent black $@

clean:
	rm -f $(ALL_OUTPUTS)