import numpy as np

import matplotlib.pyplot as plt

# Define the domain
x = np.linspace(-2, 2, 400)

# Compute function values
y1 = x**2
y2 = np.cos(x)

# Create subplots
fig, axes = plt.subplots(1, 2, figsize=(7, 4))
# Update the domain
x = np.linspace(-6, 6, 400)
y1 = x**2
y2 = np.cos(x)

# Draw x and y axes for both subplots
for ax in axes:
    ax.axhline(0, color='black', linewidth=1)
    ax.axvline(0, color='black', linewidth=1)
# Left subplot: y = x^2
axes[0].plot(x, y1, color='royalblue', linewidth=2)
axes[0].set_title(r"$y = x^2$")
axes[0].set_xlabel("x")
axes[0].set_ylabel("y")
axes[0].grid(True)
axes[0].set_xlim(-2, 2)
axes[0].set_ylim(0, 4.5)

# Right subplot: y = cos(x)
axes[1].plot(x, y2, color='darkorange', linewidth=2)
axes[1].set_title(r"$y = \cos(x)$")
axes[1].set_xlabel("x")
axes[1].set_ylabel("y")
axes[1].grid(True)
axes[1].set_xlim(-5, 5)
axes[1].set_ylim(-1.1, 1.1)

plt.tight_layout()

# Save the figure to PNG, 600 pixels wide
plt.savefig("evenfuncs.png", dpi=100, bbox_inches='tight')

plt.show()

# Create subplots for odd functions: y = x^3 and y = sin(x)
fig2, axes2 = plt.subplots(1, 2, figsize=(7, 4))

# Domain for odd functions
x_odd = np.linspace(-6, 6, 400)
y3 = x_odd**3
y4 = np.sin(x_odd)

# Draw x and y axes for both subplots
for ax in axes2:
    ax.axhline(0, color='black', linewidth=1)
    ax.axvline(0, color='black', linewidth=1)
# Left subplot: y = x^3
axes2[0].plot(x_odd, y3, color='seagreen', linewidth=2)
axes2[0].set_title(r"$y = x^3$")
axes2[0].set_xlabel("x")
axes2[0].set_ylabel("y")
axes2[0].grid(True)
axes2[0].set_xlim(-2, 2)
axes2[0].set_ylim(-8, 8)

# Right subplot: y = sin(x)
axes2[1].plot(x_odd, y4, color='crimson', linewidth=2)
axes2[1].set_title(r"$y = \sin(x)$")
axes2[1].set_xlabel("x")
axes2[1].set_ylabel("y")
axes2[1].grid(True)
axes2[1].set_xlim(-5, 5)
axes2[1].set_ylim(-1.1, 1.1)

plt.tight_layout()

# Save the figure to PNG, 600 pixels wide
plt.savefig("oddfuncs.png", dpi=100, bbox_inches='tight')

plt.show()
