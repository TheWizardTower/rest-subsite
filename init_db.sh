#!/usr/bin/env

set -euo pipefail

# As with the run.sh file, this script requires a DATABASE_URL environment
# variable, populated with a valid PostgreSQL connection string. Running this
# script will initialize the todos table, which is used in the server project.
# So, test if the variable exists. If it doesn't, print a helpful message and
# exit immediately. If it is defined, try to run the init_todos.sql script.
if [ -z "${DATABASE_URL+xxx}" ]
then
    echo "DATABASE_URL is undefined"
    exit -1
fi

psql $DATABASE_URL -f init_todos.sql
