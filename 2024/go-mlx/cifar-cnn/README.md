To run this code, start by installing GoMLX and its deps by following the
instructions in their README: https://github.com/gomlx/gomlx

Then, download the CIFAR dataset and place it in some $DIR:

    go run ./download-cifar -data $DIR

Next, we can train the model, providing it with $DIR and another directory
to store/load its checkpoints:

    go run ./train-cnn -data $DIR -checkpoint $DIR/checkpoints -nsteps 50000

The `classify-cnn` command will harvest all PNG files from $PWD, so we should
extract some PNGs from the CIFAR test set first:

    go run ./sample-images -data $DIR

This should dump a bunch of PNGs into $PWD. Now the classifier is ready to run:

    go run ./classify-cnn  -checkpoint $DIR/checkpoints .
