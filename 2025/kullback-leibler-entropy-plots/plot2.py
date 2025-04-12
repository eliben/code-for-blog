import numpy as np
import matplotlib.pyplot as plt

plt.style.use("seaborn-v0_8-darkgrid")

labels = ["x$_1$", "x$_2$", "x$_3$", "x$_4$", "x$_5$"]
p = [0.1, 0.2, 0.4, 0.2, 0.1]
# q = [0.2, 0.2, 0.2, 0.2, 0.2]
q = [0.15, 0.175, 0.35, 0.175, 0.15]

xent = -np.sum(np.where(p != 0, p * np.log2(q), 0))
print(f"sum(p): {np.sum(p)}")
print(f"sum(q): {np.sum(q)}")
print(f"Cross-entropy: {xent:.4f}")

x = np.arange(len(labels))
width = 0.25  # bar width

fig, ax = plt.subplots(figsize=(5, 4))
# Bars for p and q
ax.bar(x - width / 2, p, width, label="p")
ax.bar(x + width / 2, q, width, label="q")

ax.set_ylabel("Probability")
ax.set_xlabel("Discrete Values")
ax.set_title("Two Probability Distributions")
ax.set_xticks(x)
ax.set_xticklabels(labels)
ax.set_ylim(0, 1)
ax.legend()

plt.show()
