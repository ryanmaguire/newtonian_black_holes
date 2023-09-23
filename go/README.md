# Compiling
The files are written in go without any dependences (other than the
standard library). You can build with `go` or `gccgo`. For example:
```
gccgo -O3 nbh*.go newtonian_black_hole.go -o main
```
Similarly:
```
go build -o main nbh*.go newtonian_black_hole.go
```

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
