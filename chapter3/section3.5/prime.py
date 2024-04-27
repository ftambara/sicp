from typing import Callable
import sympy
import time


def time_fn(fn: Callable) -> float:
    start = time.time()
    fn()
    return time.time() - start

def sum_of_primes(start: int, end: int) -> int:
    total = 0
    for prime in sympy.ntheory.generate.primerange(start, end):
        total += prime
    return total

print(time_fn(lambda: sum_of_primes(1, 1_000_000)))
