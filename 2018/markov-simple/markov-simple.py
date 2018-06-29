# TODO: remove all debugging code for presentation
from collections import defaultdict, Counter
import random
import sys

# This is the length of the "state" the current character is predicted from.
# For Markov chains with memory, this is the "order" of the chain. For n-grams,
# n is STATE_LEN+1 since it includes the predicted character as well.
STATE_LEN = 4


# TODO: mention possible optimization -- randrange is slow, we could precompute
# sum total and most_common (binary search in post)
# It likely doesn't matter because we only invoke this for sampling which isn't
# too common (also we'd have to precompute sum/most_common for every possible
# state counter)
def weighted_from_counter(c):
    total = sum(c.values())
    idx = random.randrange(total)
    for elem, count in c.most_common():
        idx -= count
        if idx < 0:
            return elem


data = sys.stdin.read()
model = defaultdict(Counter)

print('Learning model...')
for i in range(len(data) - STATE_LEN - 1):
    state = data[i:i + STATE_LEN]
    next = data[i + STATE_LEN]
    model[state][next] += 1

print('Model has {0} states'.format(len(model)))
j = 0
for k, v in model.items():
    print(k, v)
    if j > 9:
        break
    j += 1

print('Sampling...')
state = random.choice(list(model))
out = list(state)
for i in range(400):
    out.append(weighted_from_counter(model[state]))
    state = state[1:] + out[-1]
print(''.join(out))
