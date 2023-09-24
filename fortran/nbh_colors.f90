!------------------------------------------------------------------------------!
!                                   LICENSE                                    !
!------------------------------------------------------------------------------!
!   This file is part of newtonian_black_holes.                                !
!                                                                              !
!   newtonian_black_holes is free software: you can redistribute it and/or     !
!   modify it under the terms of the GNU General Public License as published   !
!   by the Free Software Foundation, either version 3 of the License, or       !
!   (at your option) any later version.                                        !
!                                                                              !
!   newtonian_black_holes is distributed in the hope that it will be useful    !
!   but WITHOUT ANY WARRANTY; without even the implied warranty of             !
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              !
!   GNU General Public License for more details.                               !
!                                                                              !
!   You should have received a copy of the GNU General Public License          !
!   along with newtonian_black_holes.  If not, see                             !
!   <https://www.gnu.org/licenses/>.                                           !
!------------------------------------------------------------------------------!
!   Purpose:                                                                   !
!       Provides tools for working with colors in RGB format.                  !
!------------------------------------------------------------------------------!
!   Author: Ryan Maguire                                                       !
!   Date:   2023/09/20                                                         !
!------------------------------------------------------------------------------!
MODULE NBH_COLORS
    IMPLICIT NONE

    ! Common constant colors. Used for drawing the black hole and detector.
    INTEGER :: BLACK(3) = (/ 0,     0,   0 /)
    INTEGER :: WHITE(3) = (/ 255, 255, 255 /)
    INTEGER :: RED(3)   = (/ 255,   0,   0 /)
    INTEGER :: GREEN(3) = (/   0, 255,   0 /)
    INTEGER :: BLUE(3)  = (/   0,   0, 255 /)

    CONTAINS

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       SCALE_COLOR:                                                       !
    !   Purpose:                                                               !
    !       Scales the intensity of a color by a real number.                  !
    !   Arguments:                                                             !
    !       SCALE_FACTOR (REAL):                                               !
    !           A real number, the scale factor to the colors intensity.       !
    !       C (INTEGER(3)):                                                    !
    !           A color in RGB format.                                         !
    !   Outputs:                                                               !
    !       SCALE_COLOR (INTEGER(3)):                                          !
    !           The color C with each color channel scaled by SCALE_FACTOR.    !
    !--------------------------------------------------------------------------!
    FUNCTION SCALE_COLOR(SCALE_FACTOR, C)
        IMPLICIT NONE
        REAL, INTENT(IN) :: SCALE_FACTOR
        INTEGER, INTENT(IN) :: C(3)
        INTEGER :: SCALE_COLOR(3)

        ! Scale each color channel and convert back to int.
        SCALE_COLOR(1) = INT(SCALE_FACTOR * C(1))
        SCALE_COLOR(2) = INT(SCALE_FACTOR * C(2))
        SCALE_COLOR(3) = INT(SCALE_FACTOR * C(3))
    END FUNCTION SCALE_COLOR

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       CHECKER_BOARD:                                                     !
    !   Purpose:                                                               !
    !       Creates a checker-board pattern for the detector.                  !
    !   Arguments:                                                             !
    !       P (REAL(3)):                                                       !
    !           A point on the detector.                                       !
    !   Outputs:                                                               !
    !       CHECKER_BOARD (INTEGER(3)):                                        !
    !           A checker-board pattern as a function or P.                    !
    !--------------------------------------------------------------------------!
    FUNCTION CHECKER_BOARD(P)
        USE NBH_SETUP
        USE NBH_EUCLID
        IMPLICIT NONE
        REAL, INTENT(IN) :: P(3)
        INTEGER :: CHECKER_BOARD(3)
        INTEGER :: USE_WHITE
        INTEGER :: CX, CY
        REAL :: COLOR_FACTOR

        ! Scale factor for darkening the detector for points far away.
        COLOR_FACTOR = Z_DETECTOR_SQUARED / NORM_SQUARED(P)

        ! If the photon didn't make it to the detector, color the point black.
        IF (P(3) .LT. Z_DETECTOR) THEN
            CHECKER_BOARD = BLACK

        ! Otherwise create a checker-board pattern from the xy components.
        ELSE
            ! Bitwise trick to get a checker board pattern.
            CX = INT(CEILING(P(1)))
            CY = INT(CEILING(P(2)))
            USE_WHITE = IAND((CX + CY), 1)

            ! Create darkened red squares for half of the points.
            IF (USE_WHITE .EQ. 0) THEN
                CHECKER_BOARD = SCALE_COLOR(COLOR_FACTOR, RED)

            ! And darkened white squares for the rest.
            ELSE
                CHECKER_BOARD = SCALE_COLOR(COLOR_FACTOR, WHITE)
            END IF
        END IF
    END FUNCTION CHECKER_BOARD
END MODULE NBH_COLORS
