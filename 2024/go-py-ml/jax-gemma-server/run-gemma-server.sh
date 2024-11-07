#!/bin/bash

set -eu
set -o pipefail

PORT=:20200

# Activate the virtualenv where JAX with GPU support is installed, as
# well as the other dependencies (flask, gunicorn, etc.)
source $HOME/bin/venv/gemma-jax-gpu/bin/activate

# In my case the display GPU is used for the inference, so these env vars
# are needed to make sure JAX/XLA doesn't run out of GPU memory. YMMV.
export XLA_PYTHON_CLIENT_PREALLOCATE=false
export XLA_PYTHON_CLIENT_ALLOCATOR=platform

# Point to the model checkpoint and tokenizer file downloaded from Kaggle.
export MODEL_CHECKPOINT=/home/eliben/Downloads/2b-it/
export MODEL_TOKENIZER=/home/eliben/Downloads/tokenizer.model

gunicorn 'gemmaserver:app' -b $PORT -w 1
