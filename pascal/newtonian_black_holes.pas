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
USES
    nbh_Color,
    nbh_Euler,
    nbh_FunctionTypes,
    nbh_Setup,
    nbh_Vec3,
    nbh_Vec6;

CONST
    InitialVelocity: Vec3 = (0.0, 0.0, 1.0);
    PPM_FILE_NAME = 'newtonian_black_holes.ppm';

VAR
    P, V: Vec3;
    U: Vec6;
    X, Y: Integer;
    C: Color;
    PPM: Text;
    ProgFactor: Real;

BEGIN
    ProgFactor := 100.0 / YSIZE;
    Assign(PPM, PPM_FILE_NAME);
    Rewrite(PPM);
    WriteLn(PPM, 'P3');
    WriteLn(PPM, XSIZE:4, YSIZE:5);
    WriteLn(PPM, '255');

    FOR Y:= 0 TO YSIZE-1 DO
    BEGIN
        FOR X:= 0 TO XSIZE-1 DO
        BEGIN
            P := PixelToPoint(X, Y);
            V := InitialVelocity;
            U := Vec6FromVectors(P, V);
            nbh_Euler.EulerPath(U, @Gravity, @Stop);
            C := CheckerBoard(U);
            ColorWrite(C, PPM);
        END;
        IF (Y MOD 20 = 0) THEN WriteLn('Progress: ', ProgFactor*Y:2:4, '%');
    END;
    WriteLn('Done');
    Close(PPM);
END.
