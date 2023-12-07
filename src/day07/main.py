from collections import Counter
card_value = { 'T': 10, 'J': 11, 'Q': 12, 'K': 13, 'A': 14, **{ str(i): i for i in range(2,10) }}
card_value2 = { **card_value, 'J': 1 }

def hand_value(hand) -> list[int]:
    vs = [card_value[c] for c in hand]
    match Counter(hand).most_common():
        case [(_, 5), *_]:         return [7] + vs
        case [(_, 4), *_]:         return [6] + vs
        case [(_, 3), (_, 2), *_]: return [5] + vs
        case [(_, 3), *_]:         return [4] + vs
        case [(_, 2), (_, 2), *_]: return [3] + vs
        case [(_, 2), *_]:         return [2] + vs
        case _:
            return [1] + vs
        
def hand_value_p2(hand) -> list[int]:
    vs = [card_value2[c] for c in hand]
    counter = Counter(hand)
    joker_count = counter['J']
    del counter['J']
    match (counter.most_common(), joker_count):
        # five of a kind
        case (_, 5):
            return [7] + vs
        case ([(_, a), *_], b) if (a+b)>=5:
            return [7] + vs
        
        # four of a kind
        case ([(_, a), *_], b) if (a+b)>=4:
            return [6] + vs
        
        # full house
        case ([(_, a), (_, 2), *_], b) if (a + b) >= 3:
            return [5] + vs
        
        # three of a kind
        case ([(_, a), *_], b) if (a+b)>=3:
            return [4] + vs
        
        # two pair
        case ([(_, 2), (_, 2), *_], _):
            return [3] + vs
        
        # one pair
        case ([(_, a), *_], b) if (a+b)>=2:
            return [2] + vs
        
        # high card
        case _:
            return [1] + vs

def parse_line(line):
    hand, bid = line.split()
    return hand, int(bid)

def main(input):
    hands = [parse_line(l) for l in input.splitlines()]
    hands.sort(key=lambda pair: hand_value(pair[0]))
    p1 = sum((i+1) * bid for i, (_, bid) in enumerate(hands))
    hands.sort(key=lambda pair: hand_value_p2(pair[0]))
    p2 = sum((i+1) * bid for i, (_, bid) in enumerate(hands))
    print(f'{p1}\n{p2}') # 253866470 254494947