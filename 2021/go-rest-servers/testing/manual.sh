#!/usr/bin/bash

set -eux
set -o pipefail

SERVERPORT=4112
SERVERADDR=localhost:${SERVERPORT}

# Start by deleting all existing tasks on the server
curl -iL -w "\n" -X DELETE ${SERVERADDR}/task/

# Add some tasks
curl -iL -w "\n" -X POST -H "Content-Type: application/json" --data '{"text":"task first","tags":["todo", "life"], "due":"2016-01-02T15:04:05+00:00"}' ${SERVERADDR}/task/
curl -iL -w "\n" -X POST -H "Content-Type: application/json" --data '{"text":"buy milk","tags":["todo"], "due":"2016-01-03T15:04:05+00:00"}' ${SERVERADDR}/task/

# Get tasks by tag
curl -iL -w "\n" ${SERVERADDR}/tag/todo/

# Get tasks by due
curl -iL -w "\n" ${SERVERADDR}/due/2016/01/03
