# Train a simple CNN on CIFAR-10 data using TF+Keras.
# Based on the tutorial: https://www.tensorflow.org/tutorials/quickstart/beginner
#
# Has to be run from inside a virtualenv created following the README
# instructions.
#
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.
import tensorflow as tf

from tensorflow.keras import datasets, layers, models
import matplotlib.pyplot as plt

(train_images, train_labels), (test_images, test_labels) = datasets.cifar10.load_data()

# Normalize pixel values to be between 0 and 1
train_images, test_images = train_images / 255.0, test_images / 255.0

print(f"number of training images: {len(train_images)}")
print(f"number of labels: {len(train_labels)}")

# Define the model architecture
model = models.Sequential()
model.add(layers.Conv2D(32, (3, 3), activation="relu", input_shape=(32, 32, 3)))
model.add(layers.MaxPooling2D((2, 2)))
model.add(layers.Conv2D(64, (3, 3), activation="relu"))
model.add(layers.MaxPooling2D((2, 2)))
model.add(layers.Conv2D(64, (3, 3), activation="relu"))
model.add(layers.Flatten())
model.add(layers.Dense(64, activation="relu"))
model.add(layers.Dense(10))

print(model.summary())

# Compile and train the model
model.compile(
    optimizer="adam",
    loss=tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True),
    metrics=["accuracy"],
)

history = model.fit(
    train_images, train_labels, epochs=50, validation_data=(test_images, test_labels)
)

savefile = "trained-model.keras"
model.save(savefile)
print(f"Model saved to {savefile}")
