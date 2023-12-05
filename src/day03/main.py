from collections import namedtuple

PartNumber = namedtuple("PartNumber", "row start end value")

is_symbol = lambda c: c!='.' and not c.isdecimal()
is_neighbor = lambda part, gear: (part.row-1 <= gear[0] <= part.row+1) and (part.start-1 <= gear[1] <= part.end+1)
neighbor_coords = lambda part: [*((part.row-1, c) for c in range(part.start-1, part.end+2)),
                                (part.row, part.start-1),
                                (part.row, part.end+1),
                                *((part.row+1, c) for c in range(part.start-1, part.end+2))]

def get_numbers(lines, symbols):
    n = None
    ns = []
    should_emit = lambda n: n is not None and any(x in symbols for x in neighbor_coords(n))
    for row, line in enumerate(lines):
        for col, c in enumerate(line):
            if n is not None and c.isdecimal():
                n = PartNumber(row, n.start, col, n.value * 10 + int(c))
            elif n is None and c.isdecimal():
                n = PartNumber(row, col, col, int(c))
            else:
                n = ns.append(n) if should_emit(n) else None
        # check end of line, too
        n = ns.append(n) if should_emit(n) else None
    return ns

def score_gear(gear, numbers):
    match [n.value for n in numbers if is_neighbor(n, gear)]:
        case [a, b]: return a * b
    return 0

def main(input: str):
    lines = input.splitlines()
    symbols = set((row,col) for row, line in enumerate(lines) for col, c in enumerate(line) if is_symbol(c))
    gears = set((row,col) for row, line in enumerate(lines) for col, c in enumerate(line) if c=='*')
    numbers = get_numbers(lines, symbols)
    p1 = sum(n.value for n in numbers)
    p2 = sum(score_gear(gear, numbers) for gear in gears)
    print(f'{p1}\n{p2}') # 527369 73074886
