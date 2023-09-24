(******************************************************************************
 *                                  LICENSE                                   *
 ******************************************************************************
 *  This file is part of newtonian_black_holes.                               *
 *                                                                            *
 *  newtonian_black_holes is free software: you can redistribute it and/or    *
 *  modify it under the terms of the GNU General Public License as published  *
 *  by the Free Software Foundation, either version 3 of the License, or      *
 *  (at your option) any later version.                                       *
 *                                                                            *
 *  newtonian_black_holes is distributed in the hope that it will be useful   *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *
 *  GNU General Public License for more details.                              *
 *                                                                            *
 *  You should have received a copy of the GNU General Public License         *
 *  along with newtonian_black_holes.  If not, see                            *
 *  <https://www.gnu.org/licenses/>.                                          *
 ******************************************************************************)
PROGRAM Hello;

TYPE
    EnumType = 1..3;
    Vec3 = Array[EnumType] of Real;

VAR
   V : Vec3;

Function Vec3Norm(V: Vec3) : Real;
BEGIN
    Vec3Norm := Sqrt(V[1]*V[1] + V[2]*V[2] + V[3]*V[3])
END;

Function Vec3Rect(x, y, z: Real) : Vec3;
BEGIN
   Vec3Rect[1] := x;
   Vec3Rect[2] := y;
   Vec3Rect[3] := z;
END;

Procedure Vec3Print(V : Vec3);
BEGIN
    WriteLn('(', V[1], ',', V[2], ',', V[3], ')');
END;

BEGIN
    V := Vec3Rect(1, 2, 3);
    Vec3Print(V);
    WriteLn('Hello, World!');
END.