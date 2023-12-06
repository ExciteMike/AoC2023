from functools import reduce
from math import ceil, floor, sqrt

def count_ways(race_time: int, distance_to_beat: int) -> int:
    test = race_time*race_time - 4 * distance_to_beat
    if test < 0:
        return 0
    lo = ceil(0.5 * (race_time - sqrt(test)))
    hi = floor(0.5 * (race_time + sqrt(test)))
    lo = lo + 1 if lo * (race_time - lo) == distance_to_beat else lo
    hi = hi - 1 if hi * (race_time - hi) == distance_to_beat else hi
    return 1 + hi - lo

def main(input: str):
    times, distances = input.splitlines()
    times = [int(time) for time in times.split()[1:]]
    distances = [int(distance) for distance in distances.split()[1:]]
    counts = [count_ways(t, d) for t, d in zip(times, distances)]
    print(reduce(lambda a, b: a * b, counts, 1)) # 252000
    time = int("".join(str(time) for time in times))
    distance = int("".join(str(d) for d in distances))
    print(count_ways(time, distance)) # 36992486
    