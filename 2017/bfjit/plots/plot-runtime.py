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
sjit_runtime = (2.89, 0.94)
optjit_runtime = (0.93, 0.3)

N = len(opt3_runtime)

ind = np.arange(N)  # the x locations for the groups
width = 0.13        # the width of the bars

fig, ax = plt.subplots()
rects1 = ax.bar(ind, simple_runtime, width)
rects2 = ax.bar(ind + width, opt_runtime, width, color='y')
rects3 = ax.bar(ind + 2 * width, opt2_runtime, width, color='orange')
rects4 = ax.bar(ind + 3 * width, opt3_runtime, width, color='r')
rects5 = ax.bar(ind + 4 * width, sjit_runtime, width, color='g')
rects6 = ax.bar(ind + 5 * width, optjit_runtime, width, color='lightgreen')

# add some text for labels, title and axes ticks
ax.set_ylabel('Run-time (sec)', fontsize=14)
ax.set_xticks(ind + 3 * width)
ax.set_xticklabels(('mandelbrot', 'factor'), fontsize=14)

ax.legend((rects1[0],
           rects2[0],
           rects3[0],
           rects4[0],
           rects5[0],
           rects6[0],
           ),
           (
            'simpleinterp',
            'optinterp',
            'optinterp2',
            'optinterp3',
            'simplejit',
            'optasmjit',
            ), fontsize=14)

fig = plt.gcf()
fig.set_tight_layout(True)
fig.set_size_inches((8, 6))

plt.savefig('plot-runtime.png', dpi=80)

plt.show()
