ALL_OUTPUT := model_processed.wrl

all: $(ALL_OUTPUT)

model_processed.wrl: model.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-life-potion

clean:
	rm -f $(ALL_OUTPUT)