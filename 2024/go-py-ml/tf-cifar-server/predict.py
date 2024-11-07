import time
import tensorflow as tf
from tensorflow.keras import datasets, layers, models
import matplotlib.pyplot as plt

# Info on the format of CIFAR10 data:
# * https://keras.io/api/datasets/cifar10/
# * https://www.cs.toronto.edu/~kriz/cifar.html

model = models.load_model("trained-weights.keras")

(train_images, train_labels), (test_images, test_labels) = datasets.cifar10.load_data()

# Normalize pixel values to be between 0 and 1
train_images, test_images = train_images / 255.0, test_images / 255.0

print(test_images.shape, test_labels.shape)

label_classes = [
    "airplane",
    "automobile",
    "bird",
    "cat",
    "deer",
    "dog",
    "frog",
    "horse",
    "ship",
    "truck",
]

# for small predicts, calling the model object directly works
# Alternatively, can call predict:
#   model.predict(test_images[:1])
# Doc: https://www.tensorflow.org/api_docs/python/tf/keras/Model

# Predict the first num images; softmax converts them into probabilities,
# and we use argmax to find the most likely class for each one.
num = 20

# Do it in the loop one by one because we want to measure the prediction
# latency for a single image after warmump (model cachedi in GPU, etc.)
for i in range(num):
    time_start = time.time()
    prediction = model(test_images[i : i + 1])
    probs = tf.nn.softmax(prediction)
    predindices = tf.argmax(probs, axis=1).numpy()
    time_end = time.time()

    predidx = predindices[0]
    testidx = test_labels[i][0]
    plt.imshow(test_images[i])
    plt.savefig(f"test_image_{i}.png")

    print(
        f"{i:2d} Predicted: {label_classes[predidx]}, Actual: {label_classes[testidx]}    (elapsed: {time_end - time_start:.6f} seconds)"
    )
