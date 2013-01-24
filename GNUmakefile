
MODULES = dice_cheat test_dice_cheat

%.beam: %.erl
	erlc -Wall $<

all: $(MODULES:%=%.beam)

test: all
	ERL_CRASH_DUMP=/dev/null \
	  erl -noinput \
	      -boot start_clean \
	      -eval 'test_dice_cheat:run_tests(), halt(0)'

clean:
	rm -f *.beam

.PHONY: all clean test
