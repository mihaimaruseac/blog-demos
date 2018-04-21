def next_q(x):
    return [a for a in [x-1, x, x+1] if 0 <= a < 8]

def next_at(qs, i):
    rqs = []
    for v in next_q(qs[i]):
        nq = list(qs)
        nq[i] = v
        rqs.append(nq)
    return rqs

def next(qs):
    rqs = []
    for i in range(8):
        rqs.extend(next_at(qs, i))
    return rqs

def score(qs):
    return sum([abs(x-y) for x, y in zip(qs, [4, 2, 0, 6, 1, 7, 5, 3])])

def search(qs):
    while score(qs) > 0:
        qs = min(next(qs), key=score)
    return qs

print(search(range(8)))
