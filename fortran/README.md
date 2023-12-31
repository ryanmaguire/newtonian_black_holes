# Compiling
The files are written in Fortran 90 without any dependencies. To compile using
`gfortran` type:
```
gfortran -O3 -flto nbh_euclid.f90 nbh_euler.f90 nbh_setup.f90 \
    nbh_colors.f90 nbh.f90 newtonian_black_holes.f90 -o nbh
```
The benchmark times in the README use 8-byte reals (double in C) to make the
comparison with other languages fair. To compile with this setting, type:
```
gfortran -O3 -flto -fdefault-real-8 nbh_euclid.f90 nbh_euler.f90 \
    nbh_setup.f90 nbh_colors.f90 nbh.f90 newtonian_black_holes.f90 -o nbh
```
You can also use `flang`, the LLVM-based Fortran compiler.
```
flang -O3 -flto nbh_euclid.f90 nbh_euler.f90 nbh_setup.f90 \
    nbh_colors.f90 nbh.f90 newtonian_black_holes.f90 -o nbh
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
