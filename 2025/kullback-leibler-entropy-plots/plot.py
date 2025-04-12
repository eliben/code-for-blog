import matplotlib.pyplot as plt

plt.style.use("seaborn-v0_8-darkgrid")

x = ["x$_1$", "x$_2$", "x$_3$", "x$_4$", "x$_5$"]

# probabilities = [1, 0, 0, 0, 0]
probabilities = [0.2, 0.2, 0.2, 0.2, 0.2]

plt.figure(figsize=(5, 4))
plt.bar(x, probabilities, width=0.5)
plt.ylim(0, 1)
plt.xlabel("Discrete Values")
plt.ylabel("Probability")
plt.title("Probability Distribution")

plt.show()
