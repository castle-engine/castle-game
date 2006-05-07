ALL_OUTPUT := castle_hall_processed.wrl gate_processed.wrl \
  end_sequence_processed.wrl

all: $(ALL_OUTPUT)
	$(MAKE) -C cages/ all

castle_hall_processed.wrl: castle_hall.wrl
	emacs --batch -l ../kambi-castle-utils.el $< \
	  -f kam-process-castle-hall

gate_processed.wrl: gate.wrl
	emacs --batch -l ../kambi-castle-utils.el $< \
	  -f kam-process-gate

end_sequence_processed.wrl: end_sequence.wrl
	emacs --batch -l ../kambi-castle-utils.el $< \
	  -f kam-process-end-sequence

clean:
	rm -f $(ALL_OUTPUT)
	$(MAKE) -C cages/ clean