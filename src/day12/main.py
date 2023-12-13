"""Day 12"""
# python .\src\main.py 12
# or maybe ..\otherpeoplescode\pypy3.10-v7.3.13-win64\pypy .\src\main.py 12
from functools import cache
from typing import Iterable

EXAMPLE = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"""

DEBUG_PRINT = False


def count_groups(springs):
    """lengths of runs of non-'.'"""
    return [len(run) for run in springs.split('.')]


def spring_list_into_groups(springs: str):
    """from things like '.#...#....###.' ro a list of lengths of runs of non-'.'"""
    groups = [len(s) for s in springs.split('.') if s]
    return groups


def test_spring(springs: str, groups: list[int]) -> int:
    """count ways to fill unknowns that match the picross groups"""
    stack: list[tuple[int, int, int]] = [(0, 0, 0)]
    ends = len(springs)
    endg = len(groups)
    total = 0
    while stack:
        s, g, current = stack.pop()
        if DEBUG_PRINT:
            print(f'{springs[s:]}, {groups[g:]}, {current}:  ', end="")
        if g >= endg:
            if -1 == springs.find('#', s):
                if DEBUG_PRINT:
                    print('  YES (A)')
                total += 1
            else:
                if DEBUG_PRINT:
                    print('  nope: groups full but "#"s remain')
            continue
        curgroup = groups[g]
        if s >= ends:
            if (g == endg-1) and (curgroup == current):
                if DEBUG_PRINT:
                    print('  YES (B)')
                total += 1
            else:
                if DEBUG_PRINT:
                    print('  nope: spring list ended before groups satisfied')
            continue
        if current > curgroup:
            if DEBUG_PRINT:
                print('  nope: group too long')
            continue
        if current == curgroup:
            match springs[s]:
                case '#':
                    if DEBUG_PRINT:
                        print('  nope: needed a "." now')
                case '.' | '?':
                    if DEBUG_PRINT:
                        print('  case B.1: ok')
                    stack.append((s+1, g+1, 0))
                case _: raise RuntimeError(f'unhandled "{springs[s]}"')
            continue
        if current == 0:
            max_group_end = springs.find('.', s)
            if max_group_end == -1:
                max_group_end = ends
            max_group_len = ends - s
            if max_group_len < curgroup:
                print('  nope: chunk too small for group')
                continue
            min_group_end = springs.find('?', s)
            if min_group_end == -1:
                min_group_end = ends
            min_group_end = min(min_group_end, max_group_end)
            if min_group_end > curgroup:
                if DEBUG_PRINT:
                    print('  nope: chunk too big for group')
                continue
            s2 = s+curgroup
            if springs[s2] in '.?':
                # group could go here
                if DEBUG_PRINT:
                    print('  case E.1: group could fit in this chunk')
                stack.append((s2+1, g+1, 0))
                if '#' not in springs[s:s2]:
                    # group doesn't HAVE TO be here
                    if DEBUG_PRINT:
                        print('  case E.2: could skip chunk')
                    stack.append((s2+1, g, 0))
                continue
        match springs[s]:
            case '#' | '?':
                if DEBUG_PRINT:
                    print('  case A.1: part of current group')
                while (s < ends) and (springs[s] in '#?') and (current < curgroup):
                    s, current = s + 1, current + 1
                stack.append((s, g, current))
                continue
            case '.':
                if DEBUG_PRINT:
                    print('  nope: group ended early')
                continue
            case _: raise RuntimeError(f'unhandled "{springs[s]}"')
        raise RuntimeError('unreachable')
    return total


@cache
def highest_offset(chunk: str, group: int) -> int:
    """how late in chunk can group start?"""
    wiggle_room = len(chunk) - group
    if wiggle_room < 0:
        return 0
    i = chunk.find('#')
    if i == -1:
        return wiggle_room
    return min(i, wiggle_room)

DEBUG_PRINT = False

@cache
def count_ways(springs: str, group_in_progress: int, groups: tuple[int]) -> int:
    """number of ways to satisfy"""
    if DEBUG_PRINT:
        print(f'count_ways("{springs}", {group_in_progress}, {groups})')
    if springs:
        spring = springs[0]
        if groups:
            group = groups[0]
            if group_in_progress >= group:
                if spring in '.?':
                    # finished a group
                    if DEBUG_PRINT:
                        print('finished a group')
                    return count_ways(springs[1:], 0, groups[1:])
                # too many #
                if DEBUG_PRINT:
                    print('too many #')
                return 0
            if group_in_progress > 0:
                if spring == '.':
                    # in-progress group doesn't fit
                    if DEBUG_PRINT:
                        print("in-progress group doesn't fit")
                    return 0
                # continue group
                if DEBUG_PRINT:
                    print("continue group")
                return count_ways(springs[1:], group_in_progress+1, groups)
            # no group started
            if spring == '.':
                # still not starting one
                if DEBUG_PRINT:
                    print("still not starting one")
                return count_ways(springs[1:], 0, groups)
            if spring == '#':
                # start one
                if DEBUG_PRINT:
                    print("start one")
                return count_ways(springs[1:], 1, groups)
            # branch
            if DEBUG_PRINT:
                print('branch')
            return (count_ways(springs[1:], 0, groups) +
                    count_ways(springs[1:], 1, groups))
        # no groups
        if spring == '#':
            # can't match this #
            if DEBUG_PRINT:
                print("can't match this #")
            return 0
        # others are ok
        if DEBUG_PRINT:
            print('others are ok')
        return count_ways(springs[1:], group_in_progress, groups)
    # no springs left
    if groups:
        if group_in_progress >= groups[0]:
            if DEBUG_PRINT:
                print('finished a group')
            return count_ways(springs, 0, groups[1:])
        # no springs to match group
        if DEBUG_PRINT:
            print('no springs to match group')
        return 0
    # satisfied
    if DEBUG_PRINT:
        print('satisfied')
    return 1


def parse(line: str):
    """input line to springs, groups"""
    springs, groups = line.split()
    groups = tuple(int(x) for x in groups.split(','))
    return springs, groups


def unfold(ss: Iterable[str]) -> Iterable[str]:
    """for part 2"""
    ss = list(ss)
    ss2 = ss[:-1] + [ss[-1]+'?']
    return tuple(ss2 * 4 + ss)


def main(puzzle_input: str):
    """day12"""
    # puzzle_input = EXAMPLE
    pairs = [parse(line) for line in puzzle_input.splitlines()]
    p1_counts = [count_ways(s, 0, g) for s, g in pairs]
    print(sum(p1_counts)) # 7260
    p2 = 0
    for i, (s, g) in enumerate(pairs):
        p2 += count_ways('?'.join([s] * 5), 0, g * 5)
    print(p2)

assert count_ways('???.###',             0, (1,1,3))   == 1
assert count_ways('.??..??...?##.',      0, (1,1,3))   == 4
assert count_ways('?#?#?#?#?#?#?#?',     0, (1,3,1,6)) == 1
assert count_ways('????.#...#...',       0, (4,1,1))   == 1
assert count_ways('????.######..#####.', 0, (1,6,5))   == 4
assert count_ways('?###????????',        0, (3,2,1))   == 10
