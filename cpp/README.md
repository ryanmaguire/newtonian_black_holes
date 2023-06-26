# Compiling
The files are written in plain C++ without any dependences (other than the
standard library). You can compile with or without OpenMP support using
`clang++` or `g++`. For example:
```
clang++ -fopenmp -O3 -flto newtonian_black_hole.cpp -o test.out
```
To compile without OpenMP just remove the `-fopenmp` option. Parallelization is
recommended. On an old potato-of-a-computer (2011 Mac Mini, 4-core, 8 threads,
2.7 GHz, running Debian GNU/Linux 11 Bullseye) the computation takes 10 seconds
with OpenMP and 73 without.

Neither `clang++` nor `g++` give any warnings, even with all enabled. Both
```
g++ -Wall -Wextra -Wpedantic -fopenmp -O3 -flto newtonian_black_hole.cpp -o test.out
```
and
```
clang++ -Weverything -fopenmp -O3 -flto newtonian_black_hole.cpp -o test.out
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
