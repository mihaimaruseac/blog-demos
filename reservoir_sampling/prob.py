import math
import random

prec = 100000000
rprec = 5

d = {}

for i in xrange(1, prec + 1):
    u = (i + 0.0) / (prec + 0.0)
    u = 1/u
    u = math.log(u) # 1
    if u <= 0: continue
    u = math.log(u) # 2
    u = round(u, rprec)
    d[u] = d.get(u, 0) + 1

for u in sorted(d.keys()):
    print u, d[u], '\\n'
