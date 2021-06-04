#!/usr/bin/bash

set -eux
set -o pipefail

SERVERPORT=8080
SERVERADDR=localhost:${SERVERPORT}
GQLPATH=http://${SERVERADDR}/query

# Clean up the task store initially
curl ${GQLPATH} \
    -w "\n" -H 'Content-Type: application/json' \
    --data-binary '{"query":"mutation {\n  deleteAllTasks\n}\n"}'

curl ${GQLPATH} \
    -w "\n" -H 'Content-Type: application/json' \
    --data-binary '{"query":"mutation {\n  createTask(input:\n    {Text:\"Buy milk\",\n     Tags:[\"todo\", \"shopping\"],\n      Due: \"2021-08-01T15:04:05Z\"\n      })\n  {\n    Id\n  }\n}"}'

curl ${GQLPATH} \
    -w "\n" -H 'Content-Type: application/json' \
    --data-binary '{"query":"mutation {\n  createTask(input:\n    {Text:\"Pay taxes\",\n     Tags:[\"todo\", \"bills\"],\n      Due: \"2021-08-01T15:04:05Z\"\n      })\n  {\n    Id\n  }\n}"}'

curl ${GQLPATH} \
    -w "\n" -H 'Content-Type: application/json' \
    --data-binary '{"query":"mutation {\n  createTask(input:\n    {Text:\"Buy stuff some day\",\n     Tags:[\"shopping\"],\n      Due: \"2021-08-01T15:04:05Z\",\n      Attachments: [\n        {Name:\"file1\", Date:\"2021-06-01T15:04:05Z\", Contents:\"hello file\"},\n        {Name:\"folder2\", Date:\"2021-05-11T15:04:05Z\", Contents:\"hello folder\"}\n      ],\n      })\n  {\n    Id\n  }\n}"}'
