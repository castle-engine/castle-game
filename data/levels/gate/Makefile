ALL_OUTPUT := gate_processed.wrl

all: $(ALL_OUTPUT)

gate_processed.wrl: gate.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-gate

clean:
	rm -f $(ALL_OUTPUT)
