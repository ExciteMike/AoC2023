"""Day 12"""
# python .\src\main.py 12
# or maybe ..\otherpeoplescode\pypy3.10-v7.3.13-win64\pypy .\src\main.py 12
from functools import cache


@cache
def count_ways(springs: str, group_in_progress: int, groups: tuple[int]) -> int:
    """number of ways to satisfy"""
    if springs:
        spring = springs[0]
        if groups:
            group = groups[0]
            if group_in_progress >= group:
                if spring in '.?':
                    # finished a group
                    return count_ways(springs[1:], 0, groups[1:])
                # too many #
                return 0
            if group_in_progress > 0:
                if spring == '.':
                    # in-progress group doesn't fit
                    return 0
                # continue group
                return count_ways(springs[1:], group_in_progress+1, groups)
            # no group started
            if spring == '.':
                # still not starting one
                return count_ways(springs[1:], 0, groups)
            if spring == '#':
                # start one
                return count_ways(springs[1:], 1, groups)
            # branch
            return (count_ways(springs[1:], 0, groups) +
                    count_ways(springs[1:], 1, groups))
        # no groups
        if spring == '#':
            # can't match this #
            return 0
        # others are ok
        return count_ways(springs[1:], group_in_progress, groups)
    # no springs left
    if groups:
        if group_in_progress >= groups[0]:
            return count_ways(springs, 0, groups[1:])
        # no springs to match group
        return 0
    # satisfied
    return 1


def parse(line: str):
    """input line to springs, groups"""
    springs, groups = line.split()
    groups = tuple(int(x) for x in groups.split(','))
    return springs, groups


def main(puzzle_input: str):
    """day12"""
    pairs = [parse(line) for line in puzzle_input.splitlines()]
    print(sum(count_ways(s, 0, g) for s, g in pairs))  # 7260
    print(sum(count_ways('?'.join([s] * 5), 0, g * 5)
          for (s, g) in pairs))  # 1909291258644
