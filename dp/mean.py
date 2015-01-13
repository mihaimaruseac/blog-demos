#!/usr/bin/env python
# -*- coding: utf8 -*-

import itertools
import numpy
import random

def add_noise(x, sensitivity, epsilon):
    return x + numpy.random.laplace(scale=sensitivity/epsilon)

def env_setup(seed):
    numpy.set_printoptions(formatter={'float': '{: 0.3f}'.format})
    random.seed(seed)
    numpy.random.seed(seed)

def test(values, count, M, epsilon_v, epsilon_c):
    elems = []
    for i in xrange(100000000):
        noisy_values = map(lambda x: add_noise(x, M, epsilon_v), values)
        noisy_count = add_noise(count, 1, epsilon_c)
        elems.append(sum(noisy_values) / noisy_count)
    return sum(elems)/len(elems)

if __name__ == '__main__':
    #env_setup(42)
    M = 10
    epsilon_v = 0.1
    epsilon_c = 0.1

    values = [random.randint(0, M) for i in xrange(10)]
    count = len(values)
    mean = sum(values) / (count + 0.0)

    metric = test(values, count, M, epsilon_v, epsilon_c)
    expected = mean * (1 + 2 / ((count ** 2) * (epsilon_c ** 2)))#sum(values)#2 / (epsilon_v ** 2)
    print metric, expected, mean
