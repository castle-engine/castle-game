ALL_OUTPUT := castle_hall_processed.wrl

all: $(ALL_OUTPUT)

castle_hall_processed.wrl: castle_hall.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-castle-hall

clean:
	rm -f $(ALL_OUTPUT)
