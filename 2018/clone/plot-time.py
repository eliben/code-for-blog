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

launch_0mb = (5.5, 22.3)
launch_2mb = (5.5, 34.5)
launch_4mb = (5.5, 44.7)
launch_8mb = (5.5, 66.8)

N = len(launch_0mb)

ind = np.arange(N)  # the x locations for the groups
width = 0.13        # the width of the bars

fig, ax = plt.subplots()
rects4 = ax.bar(ind, launch_0mb, width, color='#7c9acc')
rects5 = ax.bar(ind + 1 * width, launch_2mb, width, color='#5c8add')
rects6 = ax.bar(ind + 2 * width, launch_4mb, width, color='#3c7aee')
rects7 = ax.bar(ind + 3 * width, launch_8mb, width, color='#1c6aff')

# add some text for labels, title and axes ticks
ax.set_ylabel('Launch-time (usec)', fontsize=14)
ax.set_xticks(ind + 2 * width)
ax.set_xticklabels(('thread', 'fork'), fontsize=14)

ax.legend((
           rects4[0],
           rects5[0],
           rects6[0],
           rects7[0],
           ),
           (
            '0 MB',
            '2 MB',
            '4 MB',
            '8 MB',
            ), fontsize=14, loc='best')

fig = plt.gcf()
fig.set_tight_layout(True)
fig.set_size_inches((8, 6))

plt.savefig('plot-launch.png', dpi=80)

plt.show()
