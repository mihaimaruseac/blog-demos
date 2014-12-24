import itertools
import timeit

MAX = 25
l = range(MAX)

def test(n):
    le = 0
    for c in itertools.combinations(l, n):
        le += 1
    return le

for n in xrange(3, MAX):
    print n, timeit.timeit(lambda : test(n), number=15)
