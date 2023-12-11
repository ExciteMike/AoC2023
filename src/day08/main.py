# ..\otherpeoplescode\pypy3.10-v7.3.13-win64\pypy .\src\main.py 8
from functools import reduce
from itertools import cycle

EXAMPLE = """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"""

def parse_line(line: str):
    node, rest = line.split(' = ')
    left, right = rest.strip("()").split(', ')
    return node, (left, right)

def p1(steps, instructions, data, starts, end_pred):
    ghosts = starts
    instructions = cycle(enumerate(instructions))
    for i, direction in instructions:
        if all(end_pred(g) for g in ghosts):
            break
        ghosts = set(data[g][direction] for g in ghosts)
        steps += 1
        if steps > 1000000000:
            break
    return steps

def p2_cycle_detect(instructions, data, start):
    ghost = start
    instructions = cycle(enumerate(instructions))
    visited = dict()
    steps = 0
    for i, direction in instructions:
        if ghost.endswith('Z'):
            key = (i, ghost)
            if key in visited:
                return visited[key], (steps - visited[key])
            else:
                visited[key] = steps
        ghost = data[ghost][direction]
        if ghost is None:
            raise Exception("null ghost")
        steps += 1
    raise Exception("unreachable")

def gcd(x, y):
    while y:
        x, y = y, x % y
    return x

def lcm(x, y):
    return (x*y) // gcd(x, y)

def main(input: str):
    instructions, data = input.split('\n\n')
    instructions = [1 if x=='R' else 0 for x in instructions]
    data = dict(parse_line(line) for line in data.strip().splitlines())
    print(p1(0, instructions, data, {'AAA'}, lambda x: x=='ZZZ')) # 16343
    starts = [k for k in data.keys() if k.endswith('A')]
    cycle_data = [p2_cycle_detect(instructions, data, start) for start in starts]
    if not all(x==y for x, y in cycle_data):
        raise AssertionError("they don't all loop neatly back to the starting conditions")
    periods = [p for _, p in cycle_data]
    period = reduce(lcm, periods, 1)
    print(period) # 15299095336639
