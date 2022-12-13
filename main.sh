#!/bin/sh

# Add or remove arguments if desired.
#
# To test forward mode only, use this:
# ARGS="--forward-mode --test-forward"
#
# To test both forward mode and reverse mode, use this:
ARGS="--reverse-mode --test-forward --test-reverse"

exec dune exec src/Main.exe -- ${ARGS} "$@"
