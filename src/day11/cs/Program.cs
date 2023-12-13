using System.Collections.Generic;
using System.Linq;

using Coord = (ulong x, ulong y);

string Input()
{
    using var f = new StreamReader("..\\..\\..\\..\\..\\..\\puzzle_input\\day11");
    return f.ReadToEnd();
}

IEnumerable<Coord> Galaxies()
{
    ulong y = 0;
    foreach (string line in Input().Split("\n"))
    {
        string line2 = line.Trim();
        if (line2.Length == 0)
        {
            continue;
        }
        ulong x = 0;
        foreach (char c in line2)
        {
            if (c == '#')
            {
                yield return (x, y);
            }
            x++;
        }
        y++;
    }
}

ulong Distance(Coord a, Coord b)
{
    var x1 = a.x;
    var y1 = a.y;
    var x2 = b.x;
    var y2 = b.y;
    return (x1 > x2 ? x1 - x2 : x2 - x1) +
           (y1 > y2 ? y1 - y2 : y2 - y1);
}

ulong Score(List<Coord> galaxies)
{
    ulong total = 0;
    for (int i = 0; i < galaxies.Count(); ++i)
    {
        for (int j = i + 1; j < galaxies.Count; ++j)
        {
            total += Distance(galaxies[i], galaxies[j]);
        }
    }
    return total;
}

List<ulong> ExpansionRemapping(
    ulong max, HashSet<ulong> occupied, ulong scale
)
{
    ulong corrected = 0;
    var buf = new List<ulong>();
    for (ulong i = 0; i < max + 1; ++i)
    {
        buf.Add(corrected);
        if (occupied.Contains(i))
        {
            corrected += 1;
        }
        else
        {
            corrected += scale;
        }
    }
    return buf;
}


List<(ulong, ulong)> Expand(
    List<Coord> galaxies,
    HashSet<ulong> occupiedRows,
    HashSet<ulong> occupiedCols,
    ulong scale
)
{
    var last_row = occupiedRows.Max();
    var last_col = occupiedCols.Max();
    var remap_y = ExpansionRemapping(last_row, occupiedRows, scale);
    var remap_x = ExpansionRemapping(last_col, occupiedCols, scale);
    var buf = new List<Coord>(galaxies.Count);
    foreach (var (x, y) in galaxies)
    {
        buf.Add((remap_x[(int)x], remap_y[(int)y]));
    }
    return buf;
}

var galaxies = Galaxies().ToList();
var occupiedRows = new HashSet<ulong>(from coord in galaxies select coord.Item2);
var occupiedCols = new HashSet<ulong>(from coord in galaxies select coord.Item1);
ulong p1 = Score(Expand(galaxies, occupiedRows, occupiedCols, 2));
ulong p2 = Score(Expand(galaxies, occupiedRows, occupiedCols, 1_000_000));

// 10292708 790194712336
Console.WriteLine("{0}\n{1}", p1, p2);