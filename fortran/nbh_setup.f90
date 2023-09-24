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
!       Provides setup parameters for drawing the black holes.                 !
!------------------------------------------------------------------------------!
!   Author: Ryan Maguire                                                       !
!   Date:   2023/09/20                                                         !
!------------------------------------------------------------------------------!
MODULE NBH_SETUP
    IMPLICIT NONE

    ! Radius of the black hole, and the square of this value.
    REAL :: BLACK_HOLE_RADIUS = 1.0
    REAL :: BLACK_HOLE_RADIUS_SQUARED = 1.0

    ! Number of pixels in the x and y axes for the PPM file.
    INTEGER :: XSIZE = 1024
    INTEGER :: YSIZE = 1024

    ! Start and end values for the x and y components of the PPM file.
    REAL :: START = -10.0
    REAL :: FINISH = 10.0

    ! Scale factors for converting from pixel to point in space.
    REAL :: PXFACTOR = 0.019550342130987292
    REAL :: PYFACTOR = 0.019550342130987292

    ! Starting z value. The location of the "source" of light.
    REAL :: Z_SRC = -10.0

    ! Final z value. The location of the "detector" of light.
    REAL :: Z_DETECTOR = 10.0

    ! The square of this value is also used.
    REAL :: Z_DETECTOR_SQUARED = 100.0

    ! Initial velocity vector. This corresponds to the velocity vector of light.
    REAL :: INITIAL_VELOCITY(3) = (/0.0, 0.0, 1.0/)

    CONTAINS

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       PIXEL_TO_POINT:                                                    !
    !   Purpose:                                                               !
    !       Computes the point in space corresponding to a pixel in the PPM.   !
    !   Arguments:                                                             !
    !       X (INTEGER):                                                       !
    !           The x value of the pixel.                                      !
    !       Y (INTEGER):                                                       !
    !           The y value of the pixel.                                      !
    !   Outputs:                                                               !
    !       P (REAL(3)):                                                       !
    !           The point in space corresponding to the (X, Y) pixel.          !
    !--------------------------------------------------------------------------!
    FUNCTION PIXEL_TO_POINT(X, Y) RESULT(P)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: X
        INTEGER, INTENT(IN) :: Y
        REAL :: P(3)

        ! The x and y components can be computed from the scale factors.
        P(1) = START + X*PXFACTOR
        P(2) = START + Y*PYFACTOR

        ! The z coordinate is always the value of the light source.
        P(3) = Z_SRC
    END FUNCTION PIXEL_TO_POINT

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       GRAVITY:                                                           !
    !   Purpose:                                                               !
    !       Computes the acceleration due to gravity. G*M is taken to be 1.    !
    !   Arguments:                                                             !
    !       P (REAL(3)):                                                       !
    !           The location of the point under consideration.                 !
    !   Outputs:                                                               !
    !       GRAVITY (REAL(3)):                                                 !
    !           The acceleration due to gravity at the point P.                !
    !   Method:                                                                !
    !       Use the inverse square law. A = -P / ||P||^3 = -P_hat / ||P||^2.   !
    !--------------------------------------------------------------------------!
    FUNCTION GRAVITY(P)
        USE NBH_EUCLID
        IMPLICIT NONE
        REAL, INTENT(IN) :: P(3)
        REAL :: GRAVITY(3)
        REAL :: NORM_P, NORM_P_SQUARED

        ! We need to compute ||P||^3. Compute ||P|| and ||P||^2.
        NORM_P = NORM(P)
        NORM_P_SQUARED = NORM_P * NORM_P

        ! Use the inverse square law to compute the acceleration.
        GRAVITY = -P / (NORM_P * NORM_P_SQUARED)
    END FUNCTION GRAVITY

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       HALT:                                                              !
    !   Purpose:                                                               !
    !       Basic stopping function for Euler's method or RK4.                 !
    !   Arguments:                                                             !
    !       P (REAL(3)):                                                       !
    !           The location of the point under consideration.                 !
    !   Outputs:                                                               !
    !       HALT (LOGICAL):                                                    !
    !           Boolean for whether or not a photon is still in motion.        !
    !   Method:                                                                !
    !       Return True if the Photon hit the detector or was absorbed by the  !
    !       black hole. Return false otherwise (don't halt).                   !
    !--------------------------------------------------------------------------!
    FUNCTION HALT(P)
        USE NBH_EUCLID
        IMPLICIT NONE
        REAL, INTENT(IN) :: P(3)
        LOGICAL :: HALT

        ! Case 1: The photon reached the detector. Return true.
        IF (P(3) .GE. Z_DETECTOR) THEN
            HALT = .TRUE.

        ! Case 2: The photon is inside the black hole. Return true.
        ELSE IF (NORM_SQUARED(P) .LE. BLACK_HOLE_RADIUS_SQUARED) THEN
            HALT = .TRUE.

        ! Case 3: The photon is still moving. Return false.
        ELSE
            HALT = .FALSE.
        END IF
    END FUNCTION HALT
END MODULE NBH_SETUP
