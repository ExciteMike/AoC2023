"""Day 12"""
# python .\src\main.py 12
# or maybe ..\otherpeoplescode\pypy3.10-v7.3.13-win64\pypy .\src\main.py 12

EXAMPLE = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"""

DEBUG_PRINT = True


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
                if DEBUG_PRINT:
                    print('  nope: chunk too small for group')
                continue
            min_group_end = springs.find('?', s)
            if min_group_end == -1:
                min_group_end = ends
            min_group_end = min(min_group_end, max_group_end)
            if min_group_end > curgroup+1:
                if DEBUG_PRINT:
                    print(f'  nope: chunk too big for group. min_group_end={min_group_end}')
                continue
            s2 = s+curgroup
            if springs[s2] in '.?':
                # group could go here
                if DEBUG_PRINT:
                    print('  case E.1: group could fit in this chunk')
                stack.append( (s2+1, g+1, 0) )
                if '#' not in springs[s:s2]:
                    # group doesn't HAVE TO be here
                    if DEBUG_PRINT:
                        print('  case E.2: could skip chunk')
                    stack.append( (s+1, g, 0) )
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


def parse(line: str):
    """input line to springs, groups"""
    springs, groups = line.split()
    groups = [int(x) for x in groups.split(',')]
    return springs, groups


def main(puzzle_input: str):
    """day12"""
    puzzle_input = EXAMPLE
    pairs = [parse(line) for line in puzzle_input.splitlines()]
    p1_counts = [test_spring(s, g) for s, g in pairs]
    print(sum(p1_counts))
    print(p1_counts)
    #p2 = 0
    #for i, ((s, g), c) in enumerate(zip(pairs, p1_counts)):
    #    print(f'{i / len(pairs)}')
    #    if c==1:
    #        p2 += 1
    #    else:
    #        p2 += test_spring('?'.join([s]*5), g * 5)
    #print(p2)
