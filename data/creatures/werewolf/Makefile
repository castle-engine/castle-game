ALL_OUTPUT := werewolf_attack_1_processed.wrl \
	      werewolf_attack_2_processed.wrl \
	      werewolf_dead_processed.wrl \
	      werewolf_hurt_processed.wrl \
	      werewolf_still_processed.wrl \
	      werewolf_walk_2_processed.wrl

all: $(ALL_OUTPUT)

werewolf_attack_1_processed.wrl: werewolf_attack_1.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-werewolf-attack_1

werewolf_attack_2_processed.wrl: werewolf_attack_2.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-werewolf-attack_2

werewolf_dead_processed.wrl: werewolf_dead.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-werewolf-dead

werewolf_hurt_processed.wrl: werewolf_hurt.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-werewolf-hurt

werewolf_still_processed.wrl: werewolf_still.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-werewolf-still

werewolf_walk_2_processed.wrl: werewolf_walk_2.wrl
	emacs --batch -l ../../kambi-castle-utils.el $< \
	  -f kam-process-werewolf-walk_2

clean:
	rm -f $(ALL_OUTPUT)
