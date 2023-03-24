#!/bin/bash

set -eux
set -o pipefail

go build -o ./plugin/counter ./plugin/pluginmain.go 
go build -o basic-bidir .
