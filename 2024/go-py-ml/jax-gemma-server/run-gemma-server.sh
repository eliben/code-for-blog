#!/bin/bash

set -eu
set -o pipefail

PORT=:20200

source $HOME/bin/venv/gemma-jax-gpu/bin/activate

export XLA_PYTHON_CLIENT_PREALLOCATE=false
export MODEL_CHECKPOINT=/home/eliben/Downloads/2b-it/
export MODEL_TOKENIZER=/home/eliben/Downloads/tokenizer.model

gunicorn 'gemmaserver:app' -b $PORT -w 1

