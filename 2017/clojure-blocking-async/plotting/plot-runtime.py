# Helper script for making plots.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import numpy as np
import matplotlib.pyplot as plt
import seaborn

go_times = [
        11.4, 11.05, 10.37, 10.34, 10.35, 10.41, 10.54, 10.43,
        20.48, 20.44, 21.11, 21.19, 21.11, 21.37, 20.75, 20.65,
        31.08, 31.15]

thread_times = [
        11.20, 10.61, 10.21, 10.35, 10.44, 10.77, 11.02, 11.01,
        10.48, 10.99, 11.02, 10.53, 10.21, 11.41, 10.25, 10.31,
        10.48, 10.98]

_, ax = plt.subplots()
ax.plot(range(1, 19), thread_times, 'o', ms=7)

ax.set_xlim(0, 20)
ax.set_ylim(0, 33)
ax.set_xticks([2 * n for n in range(10)])

fig = plt.gcf()
fig.set_tight_layout(True)
fig.set_size_inches((8, 6))

plt.savefig('plot-runtime.png', dpi=80)
plt.show()
