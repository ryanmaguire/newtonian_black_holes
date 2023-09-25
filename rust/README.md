# Compiling
The files are written in Rust without any dependencies. It's a small project
so `cargo` isn't really needed, just run:
```
rustc -C opt-level=3 -C lto=on nbh.rs
rustc -C opt-level=3 -C lto=on --extern nbh=libnbh.rlib main.rs
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
