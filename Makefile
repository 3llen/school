.PHONY: all
all:
	stow -t $(HOME) jakemath
	make -C math-254
	make -C math-248
	make -C math-317
	make -C math-323
	make -C math-340
	make -C math-417
	make -C math-488
	make -C comp-230
	make -C comp-621
	make -C comp-523
	make -C comp-525
	make -C grants/nserc
	make -C grants/frqnt
	make -C mcgill-msc-app/personal-statement
