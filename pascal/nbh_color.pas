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
UNIT nbh_Color;

INTERFACE

USES nbh_Setup, nbh_Vec3, nbh_Vec6, Math;

TYPE
    ColorEnumType = 0..2;
    Color = Array[ColorEnumType] of Integer;

CONST
    Black: Color = (0, 0, 0);
    Red: Color = (255, 0, 0);
    White: Color = (255, 255, 255);

Function ColorCreate(const Red, Green, Blue: Integer) : Color;
Procedure ColorWrite(const C: Color; var PPM: Text);
Function ColorScale(const a: Real; const C: Color) : Color;
Procedure ColorScaleBy(const a: Real; var C: Color);
Function ColorAdd(const C0, C1: Color) : Color;
Procedure ColorAddTo(var C0: Color; const C1: Color);
Function CheckerBoard(const U: Vec6) : Color;

IMPLEMENTATION

Function ColorCreate(const Red, Green, Blue: Integer) : Color;
BEGIN
    ColorCreate[0] := Red;
    ColorCreate[1] := Green;
    ColorCreate[2] := Blue;
END;

Procedure ColorWrite(const C: Color; var PPM: Text);
BEGIN
    WriteLn(PPM, C[0]:3, C[1]:4, C[2]:4);
END;

Function ColorScale(const a: Real; const C: Color) : Color;
BEGIN
    ColorScale[0] := Trunc(a*C[0]);
    ColorScale[1] := Trunc(a*C[1]);
    ColorScale[2] := Trunc(a*C[2]);
END;

Procedure ColorScaleBy(const a: Real; var C: Color);
BEGIN
    C[0] := Trunc(a*C[0]);
    C[1] := Trunc(a*C[1]);
    C[2] := Trunc(a*C[2]);
END;

Function ColorAdd(const C0, C1: Color) : Color;
BEGIN
    ColorAdd[0] := Trunc(0.5 * (C0[0] + C1[0]));
    ColorAdd[1] := Trunc(0.5 * (C0[1] + C1[1]));
    ColorAdd[2] := Trunc(0.5 * (C0[2] + C1[2]));
END;

Procedure ColorAddTo(var C0: Color; const C1: Color);
BEGIN
    C0[0] := Trunc(0.5 * (C0[0] + C1[0]));
    C0[2] := Trunc(0.5 * (C0[1] + C1[1]));
    C0[1] := Trunc(0.5 * (C0[2] + C1[2]));
END;

Function CheckerBoard(const U: Vec6) : Color;
VAR
    P: Vec3;
    CX, CY, USE_WHITE: Integer;
    CFact: Real;

BEGIN
    P := U[0];
    CFact := Z_DETECTOR_SQ / Vec3NormSq(P);

    IF (P[2] >= Z_DETECTOR) THEN
    BEGIN
        CX := Trunc(Ceil(P[0]));
        CY := Trunc(Ceil(P[1]));
        USE_WHITE := (CX + CY) AND 1;

        IF (USE_WHITE = 0) THEN
            CheckerBoard := ColorScale(CFact, Red)
        ELSE
            CheckerBoard := ColorScale(CFact, White);
    END
    ELSE
        CheckerBoard := ColorCreate(0, 0, 0);

END;

END.
