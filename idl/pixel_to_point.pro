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
;       Converts a pixel on the PPM to a point in space.                       ;
;------------------------------------------------------------------------------;
;   Author: Ryan Maguire                                                       ;
;   Date:   2023/09/21                                                         ;
;------------------------------------------------------------------------------;
FUNCTION PIXEL_TO_POINT, X, Y
    COMPILE_OPT IDL2
    ON_ERROR, 2
    PXFACTOR = 0.019550342130987292D
    PYFACTOR = 0.019550342130987292D
    START = -10.0
    Z_SRC = -10.0
    XPT = START + DOUBLE(X)*PXFACTOR
    YPT = START + DOUBLE(Y)*PYFACTOR
    ZPT = Z_SRC
    RETURN, [XPT, YPT, ZPT]
END
