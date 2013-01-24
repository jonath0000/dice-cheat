
MODULES = dice_cheat test_dice_cheat

%.beam: %.erl
	erlc -Wall $<

all: $(MODULES:%=%.beam)

clean:
	rm -f *.beam

.PHONY: all clean
