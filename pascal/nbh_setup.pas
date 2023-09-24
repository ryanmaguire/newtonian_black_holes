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
UNIT nbh_Setup;

INTERFACE

USES nbh_Vec3, nbh_Vec6;

CONST
    XSIZE: Integer = 1024;
    YSIZE: Integer = 1024;
    Z_SRC: Real = -10.0;
    Z_DETECTOR: Real = 10.0;
    Z_DETECTOR_SQ: Real = 100.0;
    START: Real = -10.0;
    FINISH: Real = 10.0;
    BLACK_HOLE_RADIUS: Real = 1.0;
    BLACK_HOLE_RADIUS_SQ: Real = 1.0;
    PX_FACTOR: Real = 0.019550342130987292;
    PY_FACTOR: Real = 0.019550342130987292;

Function PixelToPoint(const X, Y: Integer) : Vec3;
Function Gravity(const P: Vec3) : Vec3;
Function Stop(const U: Vec6) : Boolean;

IMPLEMENTATION

Function PixelToPoint(const X, Y: Integer) : Vec3;
VAR
    XPT, YPT: Real;
BEGIN
    XPT := START + PX_FACTOR*X;
    YPT := START + PY_FACTOR*Y;
    PixelToPoint := Vec3Rect(XPT, YPT, Z_SRC);
END;

Function Gravity(const P: Vec3) : Vec3;
VAR
    Factor, Norm, NormSq: Real;
BEGIN
    Norm := Vec3Norm(P);
    NormSq := Norm*Norm;
    Factor := -1.0 / (Norm * NormSq);
    Gravity := Vec3Scale(Factor, P);
END;

Function Stop(const U: Vec6) : Boolean;
VAR
    P: Vec3;
BEGIN
    P := U[0];

    IF (P[2] >= Z_DETECTOR) THEN
        Stop := True
    ELSE IF (Vec3NormSq(P) <= BLACK_HOLE_RADIUS_SQ) THEN
        Stop := True
    ELSE
        Stop := False;
END;

END.
