import random

a = 0     # a_0 = 0
a_sum = a # will be \sum_{i=1}^{n-1}{a_i}

for i in range(10):
    a = 1 + 1.0 / (i + 1) * a_sum
    print(i + 1, ":", a)
    a_sum += a

PRINT_EVERY = 100000

def jump(end=10):
    pos = 0
    ret = 0
    while pos != end:
        pos = random.randrange(pos, end) + 1
        ret += 1
    return ret

sum_jumps = 0
num_tries = 0

while True:
    for _ in range(PRINT_EVERY):
        sum_jumps += jump()
        num_tries += 1
    print("Avg {:.8f} ({} / {})".format(
        (sum_jumps + 0.0) / num_tries, sum_jumps, num_tries))
