#!/usr/bin/env bash

set -euo pipefall

# We have to do a bit of a song-and-dance around building the haddock
# documentation /first/, then doing the "for real" build, so that the
# haddock documentation is included in the build we run and test.
stack haddock --haddock-arguments="--odir=./docs"
stack build

# From here, we could also run `stack run` to both run code and
# reference documentation.
