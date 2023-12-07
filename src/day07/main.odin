// how I'm running it:
//    run 'x64 Native Tools Command Prompt for VS20XX'
//    set PATH=%PATH%;D:\dev\otherpeoplescode\Odin\
//    cd to project folder
//    odin run .\src\day07\ -out .\src\day07\main.odin.exe
package main

import "core:fmt"
import "core:slice"
import "core:sort"
import "core:strconv"
import "core:strings"
import "core:unicode"
import "core:unicode/utf8"
import "core:os"

Hand :: struct {
    p1_score: i64,
    p2_score: i64,
    hand: string,
    bid: i64
}

p1_sort :: proc(a: Hand) -> i64 {
    return a.p1_score
}

p2_sort :: proc(a: Hand) -> i64 {
    return a.p2_score
}

card_value :: proc(c: rune) -> i64 {
    switch c {
        case '2'..='9': return i64(strconv.atoi(utf8.runes_to_string([]rune{c})))
        case 'T':  return 10
        case 'J':  return 11
        case 'Q':  return 12
        case 'K':  return 13
        case 'A':  return 14
        case: return 0
    }
}

card_value2 :: proc(c: rune) -> i64 {
    if c=='J' {
        return 1
    } else {
        return card_value(c)
    }
}

calc_p1_score :: proc(hand: string) -> i64 {
    // for each card, how many cards of that rank
    card_hist: map[rune]int
    for c in hand {
        card_hist[c] = card_hist[c] + 1
    }
    score := score_card_hist(card_hist)
    // tie breakers
    for c in hand {
        score = score * 100 + card_value(c)
    }
    return score
}

score_card_hist :: proc(card_hist: map[rune]int) -> i64 {
    score: i64
    // for <number of cards>, how many different cards had that many
    count_hist: map[int]int
    for card, count in card_hist {
        count_hist[count] += 1
    }
    switch {
        // five of a kind
        case count_hist[5] > 0: score = 7
        // four of a kind
        case count_hist[4] > 0: score = 6
        // full house
        case (count_hist[3] > 0) && (count_hist[2] > 0): score = 5
        // three of a kind
        case count_hist[3] > 0: score = 4
        // two pair
        case count_hist[2] > 1: score = 3
        // two of a kind
        case count_hist[2] > 0: score = 2
        // high card
        case: score = 1
    }
    return score
}

calc_p2_score :: proc(hand: string) -> i64 {
    // for each card, how many cards of that rank
    card_hist: map[rune]int
    for c in hand {
        card_hist[c] = card_hist[c] + 1
    }
    js := card_hist['J']
    count_js_as : rune
    best_count : int
    if (1<=js) && (js<5) {
        delete_key(&card_hist, 'J')
        for card, count in card_hist {
            if count > best_count {
                best_count = count
                count_js_as = card
            }
        }
        card_hist[count_js_as] = card_hist[count_js_as] + js
    }
    score := score_card_hist(card_hist)
    // tie breakers
    for c in hand {
        score = score * 100 + card_value2(c)
    }
    return score
}

main :: proc() {
    input, err := os.read_entire_file(".\\puzzle_input\\day07")
    hands: [dynamic]Hand
    for line in strings.split_lines(strings.trim(string(input), " \r\n")) {
        parts := strings.split(line, " ")
        bid, _ := strconv.parse_i64(parts[1])
        hand := parts[0]
        p1_score := calc_p1_score(hand)
        p2_score := calc_p2_score(hand)
        append(&hands, Hand{p1_score, p2_score, hand, bid})
    }
    slice.sort_by_key(hands[:], p1_sort)
    p1: i64 = 0
    for h, i in hands {
        p1 = p1 + i64(i+1) * h.bid
    }
    p2: i64 = 0
    slice.sort_by_key(hands[:], p2_sort)
    for h, i in hands {
        p2 = p2 + i64(i+1) * h.bid
    }
    fmt.println(p1, p2) // 253866470 254494947
}
