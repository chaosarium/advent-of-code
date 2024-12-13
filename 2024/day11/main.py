import math
import functools

def blink(n: int) -> list[int]:
    if n == 0:
        return [1]

    num_digits = math.ceil(math.log(n+1, 10))
    if num_digits % 2 == 0:
        powo10 = (10 ** (num_digits // 2))
        left = n // powo10
        right = n - left * powo10
        return [left, right]
    
    return [n * 2024]

def step(stones: list[int]):
    stepped = []
    for stone in stones:
        stepped.extend(blink(stone))
    return stepped
    
def steps(stones: list[int], num_steps: int):
    for i in range(num_steps):
        stones = step(stones)
    return stones
    
@functools.cache
def dp(stone: int, num_steps: int):
    if num_steps == 0:
        return 1
    blinked = blink(stone)
    
    return sum(map(lambda new_stone: dp(new_stone, num_steps - 1), blinked))

print("p1 test:", len(steps(
    map(lambda x: int(x), open("test.txt").read().split(" ")), 
    25
)))
print("p1 real:", len(steps(
    map(lambda x: int(x), open("input.txt").read().split(" ")), 
    25
)))
print("p2 test:", sum(
    map(lambda x: dp(int(x), 75), open("test.txt").read().split(" ")), 
))
print("p2 real:", sum(
    map(lambda x: dp(int(x), 75), open("input.txt").read().split(" ")), 
))
