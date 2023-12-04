def check_card(line: str):
    _, numbers = line.split(':')
    winning_numbers, have_numbers = numbers.split('|')
    winning_numbers = set(int(x) for x in winning_numbers.split())
    have_numbers = set(int(x) for x in have_numbers.split())
    n = len(winning_numbers.intersection(have_numbers))
    v = (2 ** (n - 1)) if n else 0
    return (v, n)

def p2(cards: list[int]):
    hist = [1 for _ in cards]
    for i in range(len(hist)):
        n = hist[i]
        if n:
            begin = i + 1
            end = min(len(cards), i+cards[i]+1)
            for j in range(begin, end):
                hist[j] += n
    return sum(hist)
    
def main(input: str):
    cards = [check_card(line) for line in input.splitlines()]
    print(f'{sum(card[0] for card in cards)}') # 33950
    print(f'{p2([card[1] for card in cards])}') # 14814534
