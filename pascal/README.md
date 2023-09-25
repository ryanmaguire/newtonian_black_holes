# Compiling
The files are written in Free Pascal without any dependencies. An attempt was
made to write it in **portable** Pascal but this may not have been accomplished.
To compile using `fpc` run:
```
fpc -vw -O3 -FW./feedback.wpo -OWall -CX -XX -Xs- newtonian_black_holes.pas 
fpc -vw -O3 -Fw./feedback.wpo -Owall newtonian_black_holes.pas 
```
This enables whole-program-optimization. To clean up, type:
```
rm -f *.ppu *.o *.wpo
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
