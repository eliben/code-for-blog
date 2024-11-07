This sample requires running all Python code in a virtual env where TensorFlow
for GPU nas been installed.

    pip install tensorflow[and-cuda]
    pip install matplotlib

To train the model, run `train.py` from the virtual env where these libraries
were installed. It saves weights to a `.keras` file.
Then `predict.py` loads the weights from this file and runs predictions.
