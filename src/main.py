from datetime import date
from importlib.util import spec_from_file_location, module_from_spec
import os
import sys

def path_import(name, path):
    spec = spec_from_file_location(name, path)
    mod = module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod

year = 2023
today = date.today()
days = [int(x) for x in sys.argv[1:]] if len(sys.argv) > 1 else [
    day for day in range(1, 26) if today >= date.fromisoformat(f"{year}-12-{day:02d}")]
puzzle_input = path_import(
    'get_puzzle_input', 'src/shared/get_puzzle_input.py').puzzle_input

for day in days:
    path = f'src/day{day:02d}/main.py'
    if os.path.exists(path):
        input = puzzle_input(day)
        path_import(f'day{day:02d}', path).main(input)
