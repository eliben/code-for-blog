# Helper script for making run-time plots.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import numpy as np
import matplotlib.pyplot as plt
import seaborn

simple_runtime = (38.6, 16.5)
opt_runtime = (18.4, 6.7)
opt2_runtime = (11.9, 3.7)
opt3_runtime = (3.9, 1.97)

N = len(simple_runtime)

ind = np.arange(N)  # the x locations for the groups
width = 0.18       # the width of the bars

fig, ax = plt.subplots()
rects1 = ax.bar(ind, simple_runtime, width)
rects2 = ax.bar(ind + width, opt_runtime, width, color='y')
rects3 = ax.bar(ind + width * 2, opt2_runtime, width, color='orange')
rects4 = ax.bar(ind + width * 3, opt3_runtime, width, color='red')

# add some text for labels, title and axes ticks
ax.set_ylabel('Run-time (sec)', fontsize=14)
ax.set_xticks(ind + width * 2.0)
ax.set_xticklabels(('mandelbrot', 'factor'), fontsize=14)

ax.legend((rects1[0],
           rects2[0],
           rects3[0],
           rects4[0]),
           ('simpleinterp',
            'optinterp',
            'optinterp2',
            'optinterp3'
            ), fontsize=14)

fig = plt.gcf()
fig.set_tight_layout(True)
fig.set_size_inches((8, 6))

plt.savefig('plot-runtime.png', dpi=80)

plt.show()
