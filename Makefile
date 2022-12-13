SHELL := bash

# [make all] compiles your project.

.PHONY: all
all:
	@ dune build --display short src/Main.exe

# [make clean] cleans up.

.PHONY: clean
clean:
	@ rm -rf _build
	@ git clean -fX

# Settings for the tests that follow.

COMMAND := ./main.sh

# [make test] combines [make human] and [make auto].
.PHONY: test
test:
	@ make human
	@ make auto

# [make human] processes every file of the form test/input/*.s
# using the command $(COMMAND) defined above.

.PHONY: human
human: all
	@ echo "Running human-written tests..."
	@ $(patsubst %,$(COMMAND) % &&, $(wildcard test/inputs/*.s)) true

# [make auto] processes a number of randomly-generated programs
# of increasing sizes, and stops at the first failure, displaying
# an error message.

.PHONY: auto
auto: all
	@ echo "Running randomly-generated tests..."
	@ $(COMMAND) --budget 100

# [make auto_resilient] processes a number of randomly-generated
# programs of increasing sizes and does not stop at the first failure.
# The total number of failures is reported instead.

.PHONY: auto_resilient
auto_resilient: all
	@ echo "Running randomly-generated tests (in resilient mode)..."
	@ $(COMMAND) --budget 100 --resilient

# [make typeset] creates the file sujet/simple.txt,
# fragments of which are read by LaTeX while typesetting the assignment.
.PHONY: typeset
typeset: all
	@ $(COMMAND) --show-surface --show-linear --mark test/inputs/simple.s > sujet/simple.txt
