import matplotlib.pyplot as plt

# Seaborn style
plt.style.use("seaborn-v0_8-darkgrid")

# Data
x = ["x$_1$", "x$_2$", "x$_3$", "x$_4$", "x$_5$"]
probabilities = [1, 0, 0, 0, 0]

# Smaller plot size
plt.figure(figsize=(5, 4))

# Narrower bars
plt.bar(x, probabilities, width=0.5)

# Axis and title
plt.ylim(0, 1)
plt.xlabel("Discrete Values")
plt.ylabel("Probability")
plt.title("Probability Distribution")

plt.show()
