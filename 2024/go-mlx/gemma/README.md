The Gemma2 2b weights have to be downloaded from
https://www.kaggle.com/models/google/gemma-2/flax/gemma2-2b, selecting the
Flax version tarball. Untar into $DIR. This creates a $DIR/gemma2-2b directory
and $DIR/tokenizer.model

Then, from a virtual env that has JAX installed, run
https://github.com/gomlx/gemma/blob/main/cmd/convert_checkpoint.py on
$DIR/gemma2-2b; this creates $DIR/gemma2-2b/raw

With this done, we can now run:

    go run . -vocab $DIR/tokenizer.model -data $DIR/gemma2-2b
