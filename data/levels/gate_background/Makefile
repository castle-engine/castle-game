ALL_OUTPUT := gate_processed.wrl water_1_processed.wrl water_2_processed.wrl

all: $(ALL_OUTPUT)

gate_processed.wrl: gate.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-gate

water_1_processed.wrl: water_1.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-gate-water-1

water_2_processed.wrl: water_2.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-gate-water-2

clean:
	rm -f $(ALL_OUTPUT)
