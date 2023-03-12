# Script to plot sinusoids for blog post. Requires numpy and matplotlib.
#
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.
import numpy as np
import matplotlib.pyplot as plt

x = np.arange(0, 4*np.pi, 0.1)

# Two sinusoids of the same frequency, but different amplitudes and phases
y = np.sin(x)
y2 = 0.66 * np.sin(x - 0.3*np.pi)

yy = y + y2

fig, ax = plt.subplots(1)
ax.plot(x, y, x, y2, x, yy)

from matplotlib.ticker import FuncFormatter, MultipleLocator
ax.xaxis.set_major_formatter(FuncFormatter(
   lambda val, pos: '{:.1g}$\pi$'.format(val/np.pi) if val !=0 else '0'
))
ax.xaxis.set_major_locator(MultipleLocator(base=np.pi))
ax.set_ylim([-2.0, 2.0])
ax.grid()

fig.tight_layout()
plt.show()
