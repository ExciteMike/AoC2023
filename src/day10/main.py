r"""run with `python .\src\main.py 10`"""
from collections import deque

EXAMPLE = """FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L"""


def test(ds, pipes, xy):
    """answers whether xy is enclosed in the pipes"""
    (x, y) = xy
    if xy in ds:
        return False
    is_inside = False
    wall_entered_with = None
    for y2 in range(y):
        if (x, y2) in ds:
            pipe = pipes[(x, y2)]
            match pipe:
                case '━':
                    is_inside = not is_inside

                case '┗':
                    if wall_entered_with == '┓':
                        is_inside = not is_inside
                    wall_entered_with = None

                case '┛':
                    if wall_entered_with == '┏':
                        is_inside = not is_inside
                    wall_entered_with = None

                case '┏' | '┓':
                    wall_entered_with = pipe

                case '┃' | '.': pass

                case _: raise RuntimeError(f'unhandled case \"{pipe}\"')
            # print(pipes[(x, y2)], is_inside, wall_entered_with)
    return is_inside


PIPE_CONVERSION = {'S': '┓', 'F': '┏', '-': '━',
                   '7': '┓', '|': '┃', 'J': '┛', 'L': '┗', '.': ' '}


def read_pipes(puzzle_input: str):
    """puzzle input to a dict mapping coord tuples to converted pipe characters,
     and a starting coord"""
    pipes = {}
    start = (0, 0)
    for y, line in enumerate(puzzle_input.splitlines()):
        for x, pipe in enumerate(line):
            if pipe == 'S':
                start = (x, y)
            pipes[(x, y)] = PIPE_CONVERSION[pipe]
    return pipes, start


def main(puzzle_input: str):
    """main"""
    pipes, start = read_pipes(puzzle_input)
    distances = {}
    queue: deque[tuple[tuple[int, int] | tuple[int, int], int]] = deque([
                                                                        (start, 0)])
    while 0 != len(queue):
        (x, y), dist = queue.popleft()
        if (x, y) in distances:
            continue
        if (x, y) not in pipes:
            continue
        distances[(x, y)] = dist
        match pipes[(x, y)]:
            case None:
                raise RuntimeError(f'missing ({x}, {y})')
            case '┃':
                queue.append(((x, y-1), dist+1))
                queue.append(((x, y+1), dist+1))
            case '━':
                queue.append(((x-1, y), dist+1))
                queue.append(((x+1, y), dist+1))
            case '┗':
                queue.append(((x, y-1), dist+1))
                queue.append(((x+1, y), dist+1))
            case '┛':
                queue.append(((x, y-1), dist+1))
                queue.append(((x-1, y), dist+1))
            case '┓':
                queue.append(((x, y+1), dist+1))
                queue.append(((x-1, y), dist+1))
            case '┏':
                queue.append(((x, y+1), dist+1))
                queue.append(((x+1, y), dist+1))
            case '.': pass
            case c:
                raise RuntimeError(f'unhandled "{c}"')

    # test(distances, pipes, (13,5))
    for y in range(141):
        for x in range(141):
            xy = (x, y)
            if xy in distances:
                print(pipes[xy], end='')
            elif test(distances, pipes, xy):
                print('x', end='')
            else:
                print(' ', end='')
        print()

    print(max(distances.values()))  # 6907
    print(sum(1 for xy in pipes
              if test(distances, pipes, xy)))  # 541
