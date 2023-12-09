#!node
const fs = require("fs");

function extrapolate(xs) {
  if (xs.every((x) => x == 0)) {
    return [0, 0];
  }
  const deltas = [...Array(xs.length - 1).keys()].map((i) => xs[i + 1] - xs[i]);
  const [fwd, back] = extrapolate(deltas);
  return [xs[xs.length - 1] + fwd, xs[0] - back];
}

fs.readFile("puzzle_input/day09", (_, buf) => {
  const pairAdd = (a, b) => [a[0] + b[0], a[1] + b[1]];
  const answer = buf
    .toString()
    .trim()
    .split("\n")
    .map((s) => s.split(" ").map(Number))
    .map(extrapolate)
    .reduce(pairAdd, [0, 0]); // [ 1884768153, 1031 ]
  console.log(answer);
});
