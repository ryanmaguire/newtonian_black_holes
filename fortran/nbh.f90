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
!       Provides raytracing routines to render black holes.                    !
!------------------------------------------------------------------------------!
!   Author: Ryan Maguire                                                       !
!   Date:   2023/09/20                                                         !
!------------------------------------------------------------------------------!
PROGRAM MAIN
    USE COLORS
    USE SETUP
    USE EULER
    IMPLICIT NONE
    REAL :: P(3)
    REAL :: V(3)
    REAL :: PROG_FACTOR
    INTEGER :: X, Y
    INTEGER :: C(3)

    ! Factor for printing a status update.
    PROG_FACTOR = 100.0 / REAL(YSIZE)

    ! Open a file for the output PPM.
    OPEN(1, FILE = "newtonian_black_holes.ppm")

    ! No idea how to write binary data in Fortran. Using text mode for now.
    WRITE(1, FMT = "(A2)") "P3"
    WRITE(1, FMT = "(I4,I5)") XSIZE, YSIZE
    WRITE(1, FMT = "(I3)") 255

    ! Loop over the Y coordinates of the pixels.
    DO Y = 1, XSIZE
        ! And loop over the X coordinates of the pixels.
        DO X = 1, YSIZE
            ! Get the point in space corresponding to this pixel.
            P = PIXEL_TO_POINT(X, Y)

            ! The initial velocity is constant across the detector.
            V = INITIAL_VELOCITY

            ! Raytrace where the light came from.
            CALL PATH(P, V, GRAVITY, HALT)

            ! Get the color corresponding to the point.
            C = CHECKER_BOARD(P)

            ! Add this color to the PPM in plain-text format.
            WRITE(1, FMT = "(I3,I4,I4)") C
        END DO

        ! Print a status update every now-and-again.
        IF (MOD(Y, 20) .EQ. 0) THEN
            PRINT "(a,F5.2,a)", "Progress: ", REAL(Y) * PROG_FACTOR, "%"
        END IF
    END DO

    ! Close the file and exit the program.
    CLOSE(1)
END PROGRAM MAIN
