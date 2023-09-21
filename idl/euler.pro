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
;       Provides Euler's method for numerically solving ODE's.                 ;
;------------------------------------------------------------------------------;
;   Author: Ryan Maguire                                                       ;
;   Date:   2023/09/21                                                         ;
;------------------------------------------------------------------------------;
PRO EULER, P, V
    ON_ERROR, 2
    EULER_MAX_ITERS = 65535
    EULER_TIME_INCREMENT = 0.01
    ITERS = 0

    WHILE ITERS LT EULER_MAX_ITERS DO BEGIN
        IF HALT(P) THEN RETURN

        ; Compute the acceleration due to gravity at the current point.
        A = GRAVITY(P)

        ; Update the position and velocity using Euler's method.
        P = V*EULER_TIME_INCREMENT + P
        V = A*EULER_TIME_INCREMENT + V

        ; Avoid an infinite loop. If ITERS gets too large, abort.
        ITERS = ITERS + 1
    ENDWHILE

    RETURN
END
