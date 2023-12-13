#!node
const fs = require("fs");

const toFakeTuple = (x, y) => x * 1000 + y;
const fromFakeTuple = v => [0 | (v / 1000), v % 1000];
const pairAdd = (a, b) => [a[0] + b[0], a[1] + b[1]];

function checkHLine(p1, pat, i) {
    const [x1, y1] = fromFakeTuple(p1);
    const y2 = 2 * i - y1 - 1;
    return (y2 < 0) || (y2 >= pat.max_y) || pat.mirrors.has(toFakeTuple(x1, y2))
}

function checkVLine(p1, pat, i) {
    const [x1, y1] = fromFakeTuple(p1);
    const x2 = 2 * i - x1 - 1;
    return (x2 < 0) || (x2 >= pat.max_x) || pat.mirrors.has(toFakeTuple(x2, y1))
}

function vLine(pat, missing) {
    for (let i = 1; i < pat.max_x; ++i) {
        n = [...pat.mirrors].filter(p1 => checkVLine(p1, pat, i)).length;
        if ((pat.mirrors.size - missing) == n) {
            return i;
        }
    }
    return 0;
}

function hLine(pat, missing) {
    for (let i = 1; i < pat.max_y; ++i) {
        n = [...pat.mirrors].filter(p1 => checkHLine(p1, pat, i)).length;
        if ((pat.mirrors.size - missing) == n) {
            return i;
        }
    }
    return 0;
}

function parse(s) {
    const lines = s.split('\r\n');
    const mirrors = new Set();
    lines.forEach((line, y) =>
        [...line].forEach((c, x) => {
            if (c == '#') {
                mirrors.add(toFakeTuple(x, y));
            }
        })
    );
    return { mirrors, max_x: lines[0].trim().length, max_y: lines.length };
};

const summarize = pat => [vLine(pat, 0) || (100 * hLine(pat, 0)),
vLine(pat, 1) || (100 * hLine(pat, 1))];

// [ 33122, 32312 ]
fs.readFile("puzzle_input/day13", (_, buf) =>
    console.log(buf
        .toString()
        .trim()
        .split("\r\n\r\n")
        .map(parse)
        .map(summarize)
        .reduce(pairAdd, [0, 0])))