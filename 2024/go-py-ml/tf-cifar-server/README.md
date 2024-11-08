This sample requires running all Python code in a virtual env where TensorFlow
for GPU nas been installed.

    pip install tensorflow[and-cuda]
    pip install matplotlib

To train the model, run `train.py` from the virtual env where these libraries
were installed. It saves weights to a `.keras` file.
Then `predict.py` loads the weights from this file and runs predictions.

----

`image-server.py` listens on a Unix domain socket for requests to classify
images; it then runs the classifier model and returns the label to the
same socket.

The socket communication uses a simple length-prefix protocol. Each packet
consists of:
    - 4 bytes: message length (type + length)
    - 1 byte: message type
    - N bytes: message body

For the supported types, see the source of `image-server.py`.
