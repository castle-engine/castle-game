ALL_OUTPUT := cages_processed.wrl

all: $(ALL_OUTPUT)

cages_processed.wrl: cages.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-cages

clean:
	rm -f $(ALL_OUTPUT)
