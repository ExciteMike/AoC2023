# ..\otherpeoplescode\pypy3.10-v7.3.13-win64\pypy .\src\main.py 8
def deltas(xs):
    prev = xs[0]
    for x in xs[1:]:
        yield x-prev
        prev = x

def extrapolate(xs):
    if all(x==0 for x in xs):
        return 0
    else:
        delta = extrapolate(list(deltas(xs)))
        return xs[-1] + delta

def extrapolate2(xs):
    if all(x==0 for x in xs):
        return 0
    else:
        return xs[0] - extrapolate2(list(deltas(xs)))

EXAMPLE = """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"""

def main(input: str):
    #input = EXAMPLE
    print(sum(extrapolate([int(x) for x in line.split()])
              for line in input.splitlines())) # 1884768153
    print(sum(extrapolate2([int(x) for x in line.split()])
              for line in input.splitlines())) # 1031
