import matplotlib.pyplot as plt
import matplotlib.cm as cm
import numpy as np

candidates = ['by-col', 'by-row-nonvec', 'by-row-vec']
parameters = ['64x64', '128x128', '256x256', '512x512']

dpoints = np.array([
    [1.56, 0.39, 0.22, 0.17],
    [1.81, 1.95, 2.01, 2.03],
    [5.26, 4.84, 4.08, 4.11]
    ])

fig = plt.figure()
ax = fig.add_subplot(111)

space = 0.3
num_candidates = len(candidates)
width = (1.0 - space) / num_candidates

for i, candidate in enumerate(candidates):
    indices = range(1, len(parameters) + 1)
    vals = dpoints[i, :]
    print vals
    pos = [j - (1.0 - space) / 2 + i * width for j in indices]
    ax.bar(pos, vals, width=width, label=candidate,
           color=cm.Accent(float(i) / num_candidates))

ax.set_xticks(indices)
ax.set_xticklabels(parameters)
ax.grid(True, which='both')

ax.set_xlabel('matrix size')
ax.set_ylabel('10^9 items/sec')

handles, labels = ax.get_legend_handles_labels()
ax.legend(handles[::-1], labels[::-1], loc='upper right')

plt.show()
