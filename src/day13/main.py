"""Day 13"""
# python .\src\main.py 13
# or maybe ..\otherpeoplescode\pypy3.10-v7.3.13-win64\pypy .\src\main.py 13
from dataclasses import dataclass


@dataclass
class Pattern:
    mirrors: set[tuple[int, int]]
    max_x: int
    max_y: int

    def check_h_reflect(self, p, i):
        p2 = (p[0], 2*i - p[1] - 1)
        return (p2[1] < 0) or (p2[1] >= self.max_y) or (p2 in self.mirrors)

    def check_v_reflect(self, p1, i):
        p2 = (2*i - p1[0] - 1, p1[1])
        return (p2[0] < 0) or (p2[0] >= self.max_x) or (p2 in self.mirrors)

    def find_h_reflection_line(self) -> int:
        for i in range(1, self.max_y):
            if all(self.check_h_reflect(p1, i) for p1 in self.mirrors):
                return i
        return 0

    def find_v_reflection_line(self) -> int:
        for i in range(1, self.max_x):
            if all(self.check_v_reflect(p1, i) for p1 in self.mirrors):
                return i
        return 0

    def find_h_reflection_line2(self) -> int:
        for i in range(1, self.max_y):
            if len(self.mirrors)-1 == sum(1 for p1 in self.mirrors if self.check_h_reflect(p1, i)):
                return i
        return 0

    def find_v_reflection_line2(self) -> int:
        for i in range(1, self.max_x):
            if len(self.mirrors)-1 == sum(1 for p1 in self.mirrors if self.check_v_reflect(p1, i)):
                return i
        return 0

    def summarize(self):
        v = self.find_v_reflection_line()
        return v or 100 * self.find_h_reflection_line()

    def summarize2(self):
        v = self.find_v_reflection_line2()
        return v or 100 * self.find_h_reflection_line2()


def parse(pattern):
    lines = pattern.splitlines()
    mirrors = set((x, y) for y, line in enumerate(lines)
                  for x, c in enumerate(line) if c == '#')
    return Pattern(mirrors, len(lines[0]), len(lines))


def main(puzzle_input: str):
    """day13"""
    patterns = [parse(p) for p in puzzle_input.split('\n\n')]
    print(sum(pattern.summarize() for pattern in patterns))  # 33122
    print(sum(pattern.summarize2() for pattern in patterns))  # 32312
