;------------------------------------------------------------------------------;
;                                   LICENSE                                    ;
;------------------------------------------------------------------------------;
;   This file is part of newtonian_black_holes.                                ;
;                                                                              ;
;   newtonian_black_holes is free software: you can redistribute it and/or     ;
;   modify it under the terms of the GNU General Public License as published   ;
;   by the Free Software Foundation, either version 3 of the License, or       ;
;   (at your option) any later version.                                        ;
;                                                                              ;
;   newtonian_black_holes is distributed in the hope that it will be useful    ;
;   but WITHOUT ANY WARRANTY; without even the implied warranty of             ;
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              ;
;   GNU General Public License for more details.                               ;
;                                                                              ;
;   You should have received a copy of the GNU General Public License          ;
;   along with newtonian_black_holes.  If not, see                             ;
;   <https://www.gnu.org/licenses/>.                                           ;
;------------------------------------------------------------------------------;
;   Purpose:                                                                   ;
;       Provides raytracing routines to render black holes.                    ;
;------------------------------------------------------------------------------;
;   Author: Ryan Maguire                                                       ;
;   Date:   2023/09/20                                                         ;
;------------------------------------------------------------------------------;
PRO MAIN
    COMPILE_OPT IDL2
    START_TIME = SYSTIME(/SECONDS)
    XSIZE = 1024
    YSIZE = 1024
    INITIAL_VELOCITY = [0.0D, 0.0D, 1.0D]

    ; Factor for printing a status update.
    PROG_FACTOR = 100.0D / DOUBLE(YSIZE)

    ; Open a file for the output PPM.
    OPENW, 1, "newtonian_black_holes.ppm"

    ; Using text mode to make life easier.
    PRINTF, 1, 'P3'
    PRINTF, 1, '1024 1024'
    PRINTF, 1, '255'

    ; Loop over the Y coordinates of the pixels.
    FOR Y = 1, YSIZE DO BEGIN

        ; And loop over the X coordinates of the pixels.
        FOR X = 1, XSIZE DO BEGIN

            ; Get the point in space corresponding to this pixel.
            P = PIXEL_TO_POINT(X, Y)

            ; The initial velocity is constant across the detector.
            V = INITIAL_VELOCITY

            ; Raytrace where the light came from.
            EULER, P, V

            ; Get the color corresponding to the point.
            C = CHECKER_BOARD(P)

            ; Add this color to the PPM in plain-text format.
            PRINTF, 1, C, FORMAT = "(I3,I4,I4)"
        ENDFOR

        ; Print a status update.
        IF (Y MOD 20) EQ 0 THEN BEGIN
            PRINT, "Progress: ", DOUBLE(Y) * PROG_FACTOR, "%"
        ENDIF
    ENDFOR

    ; Close the file and exit the program.
    CLOSE, 1

    END_TIME = SYSTIME(/SECONDS)
    PRINT, "Total Time: ", DOUBLE(END_TIME - START_TIME)
END
