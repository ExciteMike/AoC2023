def puzzle_input(day):
    import urllib.request

    year = 2022
    fname = f"puzzle_input/day{day:02}"
    f = None
    try:
        f = open(fname, "r")
    except FileNotFoundError:
        session_cookie = open("session_cookie", "r").read()
        url = f"https://adventofcode.com/{year}/day/{day}/input"
        opener = urllib.request.build_opener()
        opener.addheaders = [("Cookie", f"session={session_cookie}")]
        urllib.request.install_opener(opener)
        urllib.request.urlretrieve(url, fname)
        f = open(fname, "r")
    return f.read().rstrip()