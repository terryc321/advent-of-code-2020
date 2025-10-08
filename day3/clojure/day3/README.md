# day3

some trees '#' and some open space '.'
start top left , move to right 3 places , move down 1 place , look at character find there

if find '#' record as a tree , if empty space '.' do not record a tree

keep a tally of number of trees encounter

the pattern repeats indefinitely horizontally , but does not repeat vertically

once fall off the pattern vertically , the computation stops and total number of trees encountered is
returned

```
.#......#..####.....#..#.......
#.#...#...#..#.#...#.#...##.##.
#.#....#..........#...##.....##
#.#.#.....##......#.#.......###
..#..###....#.#....#.#.#..#....
.......#.#....##..##...#...#...
..#..#..#..###.......#.....#.#.
.#.......#...##...##.##......##
#.#.##..##.#..#....#..###..#.#.
#.....#.#.........#.....##.#.#.
..#.#....##..#...#...##........
......#....#..##.#.#......###..
.......#.......#......##...#...
.##.....#.......#...###.....##.
.#...#.##..##.#..##....#.......
..#......##...#..#...#.#.##.###
```

can we give a visual representation to user to see the trees encountered

can we verify this is indeed correct

off by 1 errors as using clojure strings zero based

