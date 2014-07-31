import random
import math

def setup ():
    items = {1:1, 2:2, 3:3, 4:4}
    sf = sum(items) + 0.0
    pairs = {}
    for i,vi in items.items():
        for j,vj in items.items():
            if i != j:
                pairs[(i, j)] = vi * vj / sf / (sf + 0.0 - vi)
    return items, pairs

def rs(items, pairs, rsMethod, maxExps=10000000):
    results = {}
    for p in pairs:
        results[p] = 0
    for i in xrange(maxExps):
        r = rsMethod(items)
        results[r] += 1
    for p in pairs:
        print p, "\t{0:5.2f}\t{1:5.2f}".format(pairs[p], results[p] / (maxExps + 0.0))

def wrs(items):
    d = {}
    for i in items:
        u = 1 - random.random() # u \in (0, 1]
        k = math.pow(u, 1.0/items[i])
        d[i] = k
    a = max(d, key=d.get)
    del d[a]
    b = max(d, key=d.get)
    return (a, b)

def prs(items):
    d = {}
    for i in items:
        u = 1 - random.random() # u \in (0, 1]
        k = (0.0 + items[i]) / u
        d[i] = k
    a = max(d, key=d.get)
    del d[a]
    b = max(d, key=d.get)
    return (a, b)

items, pairs = setup()
rs(items, pairs, wrs)
print ""
rs(items, pairs, prs)
