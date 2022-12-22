#!/bin/sh

# Add or remove arguments if desired.
#
# To test forward mode only, use this:
# ARGS="--forward-mode --test-forward"
#
# To test both forward mode and reverse mode, use this:
ARGS="--reverse-mode --test-forward --test-reverse"
#
# The flag --test-reverse (above) tests the combination
# of unzipping and transposition. To test unzipping in
# isolation, use this:
# ARGS="--reverse-mode --test-unzipping"

exec dune exec src/Main.exe -- ${ARGS} "$@"
