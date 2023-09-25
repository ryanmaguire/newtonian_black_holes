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
MODULE NBH
    IMPLICIT NONE
    CONTAINS

    !--------------------------------------------------------------------------!
    !   Subroutine:                                                            !
    !       RUN                                                                !
    !   Purpose:                                                               !
    !       Performs all of the raytracing for rendering a black hole.         !
    !   Arguments:                                                             !
    !       ACCELERATION (FUNCTION):                                           !
    !           A vector-valued function describing the equation of motion.    !
    !       STOPPER (FUNCTION):                                                !
    !           Stopping condition for when to halt the numerical ODE method.  !
    !       COLORER (FUNCTION):                                                !
    !           Coloring function for coloring the detector.                   !
    !       RAYTRACER (SUBROUTINE):                                            !
    !           Subroutine that determines which numerical ODE solver to use.  !
    !--------------------------------------------------------------------------!
    SUBROUTINE RUN(ACCELERATION, STOPPER, COLORER, RAYTRACER)
        USE NBH_SETUP
        USE NBH_COLORS
        IMPLICIT NONE
        REAL :: P(3)
        REAL :: V(3)
        REAL :: PROG_FACTOR
        INTEGER :: X, Y
        INTEGER :: C(3)
        INTEGER :: PPM

        ! Interface for the acceleration function. 3D vector in, 3D vector out.
        INTERFACE
            FUNCTION ACCELERATION(P)
                REAL, INTENT(IN) :: P(3)
                REAL :: ACCELERATION(3)
            END FUNCTION ACCELERATION
        END INTERFACE

        ! Interface for the stopping condition. Outputs a Boolean.
        INTERFACE
            FUNCTION STOPPER(P)
                REAL, INTENT(IN) :: P(3)
                LOGICAL :: STOPPER
            END FUNCTION STOPPER
        END INTERFACE

        ! Interface for the coloring function. Vector in, color out.
        INTERFACE
            FUNCTION COLORER(P)
                REAL, INTENT(IN) :: P(3)
                INTEGER :: COLORER(3)
            END FUNCTION COLORER
        END INTERFACE

        ! File number for the PPM file.
        PPM = 1

        ! Factor for printing a status update.
        PROG_FACTOR = 100.0 / REAL(YSIZE)

        ! Open a file for the output PPM.
        OPEN(PPM, FILE = "newtonian_black_holes.ppm")

        ! No idea how to write binary data in Fortran. Using text mode for now.
        WRITE(PPM, FMT = "(A2)") "P3"
        WRITE(PPM, FMT = "(I4,I5)") XSIZE, YSIZE
        WRITE(PPM, FMT = "(I3)") 255

        ! Loop over the Y coordinates of the pixels.
        DO Y = 1, XSIZE

            ! And loop over the X coordinates of the pixels.
            DO X = 1, YSIZE

                ! Get the point in space corresponding to this pixel.
                P = PIXEL_TO_POINT(X, Y)

                ! The initial velocity is constant across the detector.
                V = INITIAL_VELOCITY

                ! Raytrace where the light came from.
                CALL RAYTRACER(P, V, ACCELERATION, STOPPER)

                ! Get the color corresponding to the point.
                C = COLORER(P)

                ! Add this color to the PPM file.
                CALL WRITE_COLOR(PPM, C)
            END DO

            ! Print a status update every now-and-again.
            IF (MOD(Y, 20) .EQ. 0) THEN
                PRINT "(A,F5.2,A)", "Progress: ", REAL(Y) * PROG_FACTOR, "%"
            END IF
        END DO

        ! Close the file and exit the program.
        CLOSE(PPM)
    END SUBROUTINE RUN
END MODULE NBH
