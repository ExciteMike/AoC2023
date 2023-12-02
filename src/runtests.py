from datetime import date
from subprocess import run
import sys

year = 2023
today = date.today()
days = [int(x) for x in sys.argv[1:]] if len(sys.argv) > 1 else range(1,26)
for day in days:
    if today >= date.fromisoformat(f"{year}-12-{day:02d}"):
        print(f'day{day:02d}')
        run(f'cargo test day{day:02d} --bin aoc{year} --quiet')
print('done')