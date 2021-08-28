#!/usr/bin/bash
#
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.

set -eux
set -o pipefail

go build -buildmode=plugin ./plugins/narcissist/
go build -buildmode=plugin ./plugins/tt/
