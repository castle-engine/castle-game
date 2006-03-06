ALL_OUTPUT := life_potion_processed.wrl

all: $(ALL_OUTPUT)

life_potion_processed.wrl: life_potion.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-life-potion

clean:
	rm -f $(ALL_OUTPUT)