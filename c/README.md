# Compiling
The files are written in plain C without any dependences (other than the
standard library). You can compile with or without OpenMP support using
`clang` or `gcc`. For example:
```
gcc -fopenmp -O3 -flto newtonian_black_hole.c -o test.out -lm
```
To compile without OpenMP just remove the `-fopenmp` option.

Neither `clang` nor `gcc` give any warnings, even with all enabled. Both
```
gcc -Wall -Wextra -Wpedantic -fopenmp -O3 -flto newtonian_black_hole.c -o test.out -lm
```
and
```
clang -Weverything -fopenmp -O3 -flto newtonian_black_hole.c -o test.out -lm
```
pass without any messages printed to the screen, so hopefully this project is
portable (but I haven't tested it on macOS or Windows).

# License
    newtonian_black_holes is free software: you can redistribute it and/or
    modify it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    newtonian_black_holes is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with newtonian_black_holes.  If not, see <https://www.gnu.org/licenses/>.
