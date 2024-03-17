from collections.abc import Callable
import random
import math

def cesaro() -> bool:
    a = random.randint(0, 1<<16)
    b = random.randint(0, 1<<16)
    return math.gcd(a, b) == 1

def monte_carlo(trials: int, experiment: Callable[[], bool]) -> float:
    passed = 0
    for _ in range(trials):
        if experiment():
            passed +=1

    return passed/trials

def estimate_pi(trials: int) -> float:
    return (6 / monte_carlo(trials, cesaro)) ** 0.5
