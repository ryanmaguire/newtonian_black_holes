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
    INTEGER :: BLACK(3)   = (/ 0,     0,   0 /)
    INTEGER :: WHITE(3)   = (/ 255, 255, 255 /)
    INTEGER :: RED(3)     = (/ 255,   0,   0 /)
    INTEGER :: GREEN(3)   = (/   0, 255,   0 /)
    INTEGER :: BLUE(3)    = (/   0,   0, 255 /)
    INTEGER :: CYAN(3)    = (/   0, 255, 255 /)
    INTEGER :: YELLOW(3)  = (/ 255, 255,   0 /)
    INTEGER :: MAGENTA(3) = (/ 255,   0, 255 /)

    CONTAINS

    !--------------------------------------------------------------------------!
    !   Subroutine:                                                            !
    !       WRITE_COLOR                                                        !
    !   Purpose:                                                               !
    !       Writes a color to a PPM file.                                      !
    !   Arguments:                                                             !
    !       FILE_NUMBER (INTEGER):                                             !
    !           An integer representing an already open file.                  !
    !       C (INTEGER(3)):                                                    !
    !           A color in RGB format.                                         !
    !--------------------------------------------------------------------------!
    SUBROUTINE WRITE_COLOR(FILE_NUMBER, C)
        INTEGER, INTENT(IN) :: FILE_NUMBER
        INTEGER, INTENT(IN) :: C(3)

        ! The colors are 8-bits wide, which has a max value of 255. Format the
        ! output as xxx xxx xxx, 3 characters wide for each color channel.
        WRITE(FILE_NUMBER, FMT = "(I3,I4,I4)") C
    END SUBROUTINE WRITE_COLOR

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       ADD_COLORS                                                         !
    !   Purpose:                                                               !
    !       Adds two colors by averaging over the color channels.              !
    !   Arguments:                                                             !
    !       C0 (INTEGER(3)):                                                   !
    !           A color in RGB format.                                         !
    !       C1 (INTEGER(3)):                                                   !
    !           Another color in RGB format.                                   !
    !   Outputs:                                                               !
    !       ADD_COLORS (INTEGER(3)):                                           !
    !           The average of the colors C0 and C1.                           !
    !--------------------------------------------------------------------------!
    FUNCTION ADD_COLORS(C0, C1)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: C0(3)
        INTEGER, INTENT(IN) :: C1(3)
        INTEGER :: ADD_COLORS(3)

        ! Average each color channel and convert back to int.
        ADD_COLORS(1) = INT(0.5 * (C0(1) + C1(1)))
        ADD_COLORS(2) = INT(0.5 * (C0(2) + C1(2)))
        ADD_COLORS(3) = INT(0.5 * (C0(3) + C1(3)))
    END FUNCTION ADD_COLORS

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       SCALE_COLOR                                                        !
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
    !       SCALED_CHECKER_BOARD                                               !
    !   Purpose:                                                               !
    !       Creates a checker-board pattern for the detector.                  !
    !   Arguments:                                                             !
    !       COLOR_FACTOR (REAL):                                               !
    !           A factor for darkening the checker board.                      !
    !       P (REAL(3)):                                                       !
    !           A point on the detector.                                       !
    !   Outputs:                                                               !
    !       SCALED_CHECKER_BOARD (INTEGER(3)):                                 !
    !           A darkened checker-board pattern as a function or P.           !
    !--------------------------------------------------------------------------!
    FUNCTION SCALED_CHECKER_BOARD(COLOR_FACTOR, P)
        USE NBH_SETUP
        IMPLICIT NONE
        REAL, INTENT(IN) :: P(3)
        REAL, INTENT(IN) :: COLOR_FACTOR
        INTEGER :: SCALED_CHECKER_BOARD(3)
        INTEGER :: USE_WHITE
        INTEGER :: CX, CY

        ! If the photon didn't make it to the detector, color the point black.
        IF (P(3) .LT. Z_DETECTOR) THEN
            SCALED_CHECKER_BOARD = BLACK

        ! Otherwise create a checker-board pattern from the xy components.
        ELSE
            ! Bitwise trick to get a checker board pattern.
            CX = INT(CEILING(P(1)))
            CY = INT(CEILING(P(2)))
            USE_WHITE = IAND((CX + CY), 1)

            ! Create darkened red squares for half of the points.
            IF (USE_WHITE .EQ. 0) THEN
                SCALED_CHECKER_BOARD = SCALE_COLOR(COLOR_FACTOR, RED)

            ! And darkened white squares for the rest.
            ELSE
                SCALED_CHECKER_BOARD = SCALE_COLOR(COLOR_FACTOR, WHITE)
            END IF
        END IF
    END FUNCTION SCALED_CHECKER_BOARD

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       CHECKER_BOARD                                                      !
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
        REAL :: COLOR_FACTOR
        INTEGER :: CHECKER_BOARD(3)

        ! Scale factor for darkening the detector for points far away.
        COLOR_FACTOR = Z_DETECTOR_SQUARED / NORM_SQUARED(P)

        ! Use the general checker board function to get the color.
        CHECKER_BOARD = SCALED_CHECKER_BOARD(COLOR_FACTOR, P)
    END FUNCTION CHECKER_BOARD

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       BRIGHT_CHECKER_BOARD                                               !
    !   Purpose:                                                               !
    !       Creates a brighter checker-board pattern for the detector.         !
    !   Arguments:                                                             !
    !       P (REAL(3)):                                                       !
    !           A point on the detector.                                       !
    !   Outputs:                                                               !
    !       BRIGHT_CHECKER_BOARD (INTEGER(3)):                                 !
    !           A brighter checker-board pattern as a function or P.           !
    !--------------------------------------------------------------------------!
    FUNCTION BRIGHT_CHECKER_BOARD(P)
        USE NBH_SETUP
        USE NBH_EUCLID
        IMPLICIT NONE
        REAL, INTENT(IN) :: P(3)
        REAL :: COLOR_FACTOR
        INTEGER :: BRIGHT_CHECKER_BOARD(3)

        ! Scale factor for darkening the detector for points far away. This is
        ! bounded below by a positive number, so it is brighter than the before.
        COLOR_FACTOR = 0.5 * (Z_DETECTOR_SQUARED / NORM_SQUARED(P) + 1.0)

        ! Use the general checker board function to get the color.
        BRIGHT_CHECKER_BOARD = SCALED_CHECKER_BOARD(COLOR_FACTOR, P)
    END FUNCTION BRIGHT_CHECKER_BOARD

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       SCALED_CHECKER_BOARD_FOUR                                          !
    !   Purpose:                                                               !
    !       Creates a checker-board pattern for the detector.                  !
    !   Arguments:                                                             !
    !       COLOR_FACTOR (REAL):                                               !
    !           A factor for darkening the checker board.                      !
    !       P (REAL(3)):                                                       !
    !           A point on the detector.                                       !
    !   Outputs:                                                               !
    !       SCALED_CHECKER_BOARD_FOUR (INTEGER(3)):                            !
    !           A darkened checker-board pattern as a function or P.           !
    !--------------------------------------------------------------------------!
    FUNCTION SCALED_CHECKER_BOARD_FOUR(COLOR_FACTOR, P)
        USE NBH_SETUP

        REAL, INTENT(IN) :: P(3)
        REAL, INTENT(IN) :: COLOR_FACTOR
        INTEGER :: CX, CY, IND
        INTEGER :: COLOR(3)
        INTEGER :: SCALED_CHECKER_BOARD_FOUR(3)

        ! If the photon didn't make it to the detector, color the point black.
        IF (P(3) .LT. Z_DETECTOR) THEN
            SCALED_CHECKER_BOARD_FOUR = BLACK

        ! Otherwise create a checker-board pattern from the xy components.
        ELSE
            CX = IAND(INT(CEILING(P(1))), 1)
            CY = IAND(INT(CEILING(P(2))), 1)
            IND = CX + 2*CY

            SELECT CASE (IND)
                CASE (0)
                    COLOR = WHITE
                CASE (1)
                    COLOR = CYAN
                CASE (2)
                    COLOR = BLUE
                CASE DEFAULT
                    COLOR = MAGENTA
            END SELECT

            ! Scale the chosen color by the color factor to finish.
            SCALED_CHECKER_BOARD_FOUR = SCALE_COLOR(COLOR_FACTOR, COLOR)
        END IF
    END FUNCTION SCALED_CHECKER_BOARD_FOUR

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       CHECKER_BOARD_FOUR                                                 !
    !   Purpose:                                                               !
    !       Creates a checker-board pattern for the detector.                  !
    !   Arguments:                                                             !
    !       P (REAL(3)):                                                       !
    !           A point on the detector.                                       !
    !   Outputs:                                                               !
    !       CHECKER_BOARD_FOUR (INTEGER(3)):                                   !
    !           A checker-board pattern as a function or P.                    !
    !--------------------------------------------------------------------------!
    FUNCTION CHECKER_BOARD_FOUR(P)
        USE NBH_SETUP
        USE NBH_EUCLID
        IMPLICIT NONE
        REAL, INTENT(IN) :: P(3)
        REAL :: COLOR_FACTOR
        INTEGER :: CHECKER_BOARD_FOUR(3)

        ! Scale factor for darkening the detector for points far away.
        COLOR_FACTOR = Z_DETECTOR_SQUARED / NORM_SQUARED(P)

        ! Use the general checker board function to get the color.
        CHECKER_BOARD_FOUR = SCALED_CHECKER_BOARD_FOUR(COLOR_FACTOR, P)
    END FUNCTION CHECKER_BOARD_FOUR
END MODULE NBH_COLORS
