ALL_OUTPUT := fountain_processed.wrl

all: $(ALL_OUTPUT)

fountain_processed.wrl: fountain.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-fountain

clean:
	rm -f $(ALL_OUTPUT)
