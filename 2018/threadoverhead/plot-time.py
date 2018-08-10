# Helper script for making run-time plots.
#
# Requires a Python installation with the full numeric stack (Numpy, Matplotlib)
# including Seaborn (for prettier plots).
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import numpy as np
import matplotlib.pyplot as plt
import seaborn

measures = (
    ('Context switch\n(pinned)', 1.2),
    ('Context switch\n(unpinned)', 2.2),
    ('memcpy 64K', 3),
    ('Thread launch', 5),
    ('Process launch', 22))
pos = np.arange(len(measures))

plt.bar(pos, [n for _, n in measures], align='center', alpha=0.5)
plt.xticks(pos, [name for name, _ in measures])
plt.ylabel('Time (us)')

fig = plt.gcf()
fig.set_tight_layout(True)
fig.set_size_inches((8, 6))

plt.savefig('plot-launch.png', dpi=80)

plt.show()
