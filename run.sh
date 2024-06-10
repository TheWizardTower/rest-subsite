#!/usr/bin/env bash

set -euo pipefail

# This server needs an environment variable named DATABASE_URL
# defined, with a psql connection string. That string also needs to be
# valid, as it'll be used to create a connection (representing a
# connection pool) at server start.

if [ -z "${DATABASE_URL+xxx}" ]
then
    echo "DATABASE_URL is undefined"
    exit -1
fi

# We also need the documentation to be generated and current, which
# necessitates a haddock run.
stack haddock --haddock-arguments="--odir=./docs"

# At long last, we can start our web server.
stack run

