#!/usr/bin/bash

set -eux
set -o pipefail

SERVERPORT=8080
SERVERADDR=localhost:${SERVERPORT}

curl http://${SERVERADDR}/query \
    -w "\n" -H 'Content-Type: application/json' \
    --data-binary '{"query":"mutation {\n  createTask(input:\n    {Text:\"Buy milk\",\n     Tags:[\"todo\", \"shopping\"],\n      Due: \"2021-08-01T15:04:05Z\"\n      })\n  {\n    Id\n  }\n}"}'

curl http://${SERVERADDR}/query \
    -w "\n" -H 'Content-Type: application/json' \
    --data-binary '{"query":"mutation {\n  createTask(input:\n    {Text:\"Pay taxes\",\n     Tags:[\"todo\", \"bills\"],\n      Due: \"2021-08-01T15:04:05Z\"\n      })\n  {\n    Id\n  }\n}"}'
