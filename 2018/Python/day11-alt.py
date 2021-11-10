"""Advent of Code 2018 Day 11"""
from functools import lru_cache

PUZZLE_INPUT = 3628
MAX_SIZE = 300
# PUZZLE_INPUT = 42

def power_level(x, y, grid_serial):
    return ((((x + 10) * (((x + 10) * y) + grid_serial)) // 100) % 10) - 5

grid = [[power_level(j, i, PUZZLE_INPUT) for j in range(1, MAX_SIZE + 1)] for i in range(1, MAX_SIZE + 1)]

@lru_cache(maxsize=None)
def point_value(x, y):
    if x < 0 or y < 0:
        return 0

    return point_value(x, y - 1) + point_value(x - 1, y) + grid[y][x] - point_value(x - 1, y - 1)

def square_value(x, y, n):
    x = x - 1
    y = y - 1
    return sum([ point_value(x + n, y + n)
               , point_value(x, y)
               , - point_value(x, y + n)
               , - point_value(x + n, y)])

def run(n, m):
    curr_max = 0
    point = None
    for n in range(n, m):
        for x in range(1, 300 - n + 1):
            for y in range(1, 300 - n + 1):
                val = square_value(x, y, n)
                if val > curr_max:
                    curr_max = val
                    point = (x + 1, y + 1, n)
    return point

def part1():
    (x, y, _) = run(3, 4)
    print(f'{x},{y}')

def part2():
    (x, y, n) = run(1, 300)
    print(f'{x},{y},{n}')

def main():
    part1()
    part2()

if __name__ == "__main__":
    main()
