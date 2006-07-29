ALL_OUTPUT := end_sequence_processed.wrl

all: $(ALL_OUTPUT)

end_sequence_processed.wrl: end_sequence.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-end-sequence

clean:
	rm -f $(ALL_OUTPUT)
