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
;       Provides a stopping condition for Euler's method.                      ;
;------------------------------------------------------------------------------;
;   Author: Ryan Maguire                                                       ;
;   Date:   2023/09/21                                                         ;
;------------------------------------------------------------------------------;
FUNCTION HALT, P
    COMPILE_OPT IDL2
    ON_ERROR, 2
    Z_DETECTOR = 10.0D
    BLACK_HOLE_RADIUS_SQUARED = 1.0D

    IF P[2] GE Z_DETECTOR THEN BEGIN
        RETURN, 1
    ENDIF ELSE IF L2_NORM_SQUARED(P) LE BLACK_HOLE_RADIUS_SQUARED THEN BEGIN
        RETURN, 1
    ENDIF

    RETURN, 0
END
