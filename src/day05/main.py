from dataclasses import dataclass
from functools import reduce
from itertools import chain

@dataclass
class ResMap:
    dst: int
    src: int
    range: int

def convert(x: int, ms: list[ResMap]) -> int:
    for m in ms:
        if m.src <= x <= m.src + m.range:
            return x + m.dst - m.src
    return x

def convert_range(x: tuple[int, int], ms: list[ResMap]) -> list[tuple[int, int]]:
    lo, hi = x
    for m in ms:
        src_end = m.src + m.range - 1
        offset = m.dst - m.src
        dst_end = src_end + offset
        if (hi < m.src) or (lo > src_end):
            # fully outside
            continue
        if lo >= m.src and hi <= src_end:
            # fully contained
            return [(lo + offset, hi + offset)]
        if lo < m.src:
            # overlaps into src range from below...
            if hi <= src_end:
                # ..and ends within
                return [*convert_range((lo, m.src-1), ms), (m.dst, hi + offset)]
            # and beyond
            return [*convert_range((lo, m.src-1), ms), (m.dst, dst_end), *convert_range((src_end + 1, hi), ms)]
        if hi > src_end:
            # overlaps into src range from above (and began within)
            return [(lo + offset, src_end + offset), *convert_range((src_end+1, hi), ms)]
        raise Exception(f'unhandled: {lo}, {hi} vs {m.src}, {src_end}')
    return [x]

parse_map = lambda block: [ResMap(*[int(x) for x in l.split()]) for l in block.splitlines()[1:]]
concat = lambda ls: list(chain(*ls))
firsts = lambda xs: [x for x, _ in xs]
pairs = lambda iterable: zip(*([iter(iterable)]*2))
parse_ranges = lambda xs: [(start, start+length-1) for start, length in pairs(xs)]
convert_ranges_step = lambda xs, ms: concat(convert_range(x, ms) for x in xs)
convert_ranges = lambda convs, ranges: reduce(convert_ranges_step, convs, ranges)

def main(input: str):
    seeds, *blocks = input.split('\n\n')
    seeds = [int(x) for x in seeds[7:].split()]
    conversions = [parse_map(x) for x in blocks]
    print(min(reduce(lambda xs, ms: [convert(x, ms) for x in xs], conversions, seeds))) # 525792406
    print(min(firsts(convert_ranges(conversions, parse_ranges(seeds))))) # 79004094
    