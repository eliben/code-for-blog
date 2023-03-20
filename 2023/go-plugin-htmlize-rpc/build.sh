#!/bin/bash

set -eux
set -o pipefail

mkdir -p plugin-binaries
go build -o ./plugin-binaries/tt ./sample-plugins/tt/
go build -o ./plugin-binaries/nacrissist ./sample-plugins/narcissist/
go build -o ./main ./main.go

