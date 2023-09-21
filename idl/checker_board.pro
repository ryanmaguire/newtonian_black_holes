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
;       Creates a checker-board pattern on the detector.                       ;
;------------------------------------------------------------------------------;
;   Author: Ryan Maguire                                                       ;
;   Date:   2023/09/21                                                         ;
;------------------------------------------------------------------------------;
FUNCTION CHECKER_BOARD, P
    ON_ERROR, 2
    BLACK = [0, 0, 0]
    WHITE = [255, 255, 255]
    RED = [255, 0, 0]
    Z_DETECTOR = 10.0D
    Z_DETECTOR_SQUARED = 100.0D
    COLOR_FACTOR = Z_DETECTOR_SQUARED / NORM_SQUARED(P)

    IF P[2] LT Z_DETECTOR THEN BEGIN
        RETURN, BLACK
    ENDIF ELSE BEGIN
        CX = LONG(CEIL(P[0]))
        CY = LONG(CEIL(P[1]))
        USE_WHITE = (CX + CY) AND 1

        IF USE_WHITE EQ 0 THEN BEGIN
            RETURN, SCALE_COLOR(COLOR_FACTOR, RED)
        ENDIF ELSE BEGIN
            RETURN, SCALE_COLOR(COLOR_FACTOR, WHITE)
        ENDELSE
    ENDELSE
END
