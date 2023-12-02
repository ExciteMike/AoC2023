DIGIT_WORDS = list(enumerate("zero one two three four five six seven eight nine".split()))

def p1(line: str):
    xs = [int(c) for c in line if c.isdecimal()]
    return xs[0] * 10 + xs[-1]

def p2Digits(line):
    while len(line):
        c = line[0]
        if c.isdecimal():
            yield int(c)
            line = line[1:]
        else:
            for i, prefix in DIGIT_WORDS:
                if line.startswith(prefix):
                    yield i
                    line = line[len(prefix):]
                    break
            else:
                line = line[1:]

def p2(line):
    xs = list(p2Digits(line))
    return xs[0] * 10 + xs[-1]

def main(input: str):
    print(f'{sum(p1(line) for line in input.splitlines())}')
    print(f'{sum(p2(line) for line in input.splitlines())}')