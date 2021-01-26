#!/usr/bin/bash

# Helper script to run generic code in pre-1.18 Go; update GO2DIR to point to
# where you have a checkout of the dev.go2go branch.

set -eux
set -o pipefail

GO2DIR=$HOME/eli/golang-go
GO2BIN=$GO2DIR/bin/go

GO2PATH=$GO2DIR/src/cmd/go2go/testdata/go2path $GO2BIN tool go2go run "$@"
