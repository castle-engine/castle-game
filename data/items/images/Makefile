ALL_OUTPUTS := sword.png flask_red.png

all: $(ALL_OUTPUTS)

RAYHUNTER_CALL := rayhunter classic 3 95 95

sword.png: ../models/sword.wrl
	$(RAYHUNTER_CALL) sword.wrl $@
	convert $@ -transparent black $@

flask_red.png: ../models/flask_red_processed.wrl
	$(RAYHUNTER_CALL) flask_red.wrl $@
	convert $@ -transparent black $@

clean:
	rm -f $(ALL_OUTPUTS)