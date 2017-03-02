import numpy as np
import matplotlib.pyplot as plt
import seaborn

mandelbrot_runtime = (38.6, 18.4)
factor_runtime = (16.5, 6.7)

N = len(mandelbrot_runtime)

ind = np.arange(N)  # the x locations for the groups
width = 0.25       # the width of the bars

fig, ax = plt.subplots()
rects1 = ax.bar(ind, mandelbrot_runtime, width)

rects2 = ax.bar(ind + width, factor_runtime, width, color='y')

# add some text for labels, title and axes ticks
ax.set_ylabel('Run-time (sec)', fontsize=14)
ax.set_xticks(ind + width)
ax.set_xticklabels(('mandelbrot', 'factor'), fontsize=14)

ax.legend((rects1[0], rects2[0]), ('simpleinterp', 'optinterp'), fontsize=14)

fig = plt.gcf()
fig.set_tight_layout(True)
fig.set_size_inches((8, 6))

plt.savefig('plot-runtime.png', dpi=80)

plt.show()
