<<<<<<< HEAD

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
=======

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
>>>>>>> 1bf03039c255f653dc6335728a96b020f80f5cbb
