#!/usr/bin/bash

set -eux
set -o pipefail

SERVERPORT=4112

# Start by deleting all existing tasks on the server
curl -iL -w "\n" -X DELETE localhost:${SERVERPORT}/task/

# Add some tasks
curl -iL -w "\n" -X POST -H "Content-Type: application/json" --data '{"text":"task first","tags":["todo", "life"], "due":"2016-01-02T15:04:05+00:00"}' localhost:${SERVERPORT}/task/
curl -iL -w "\n" -X POST -H "Content-Type: application/json" --data '{"text":"buy milk","tags":["todo"], "due":"2016-01-03T15:04:05+00:00"}' localhost:${SERVERPORT}/task/

# Get tasks by tag
curl -iL -w "\n" localhost:${SERVERPORT}/tag/todo/
