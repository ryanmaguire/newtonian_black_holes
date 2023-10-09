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
!       NBH all in one file. Silly comparison for link-time optimizations when !
!       using gfortran.                                                        !
!------------------------------------------------------------------------------!
!   Author: Ryan Maguire                                                       !
!   Date:   2023/09/20                                                         !
!------------------------------------------------------------------------------!
MODULE NBH
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

    ! Maximum number of iterations allowed in Euler's method.
    INTEGER :: EULER_MAX_ITERS = 65535

    ! Step-size for Euler's method.
    REAL :: EULER_TIME_INCREMENT = 0.01

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
    !   Function:                                                              !
    !       NORM:                                                              !
    !   Purpose:                                                               !
    !       Computes the Euclidean norm, or L2 norm, of a 3D vector.           !
    !   Arguments:                                                             !
    !       V (REAL(3)):                                                       !
    !           A three-dimensional vector.                                    !
    !   Outputs:                                                               !
    !       NORM (REAL):                                                       !
    !           The magnitude of the vector V.                                 !
    !   Method:                                                                !
    !       Use the Pythagorean formula. ||V|| = SQRT(SUM(V^2))                !
    !--------------------------------------------------------------------------!
    FUNCTION NORM(V)
        IMPLICIT NONE
        REAL, INTENT(IN) :: V(3)
        REAL :: NORM

        ! The norm is computed by the square-root of the sum of the squares.
        NORM = SQRT(SUM(V ** 2))
    END FUNCTION NORM

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       NORM_SQUARED:                                                      !
    !   Purpose:                                                               !
    !       Computes the square of the Euclidean, or L2, norm of a 3D vector.  !
    !   Arguments:                                                             !
    !       V (REAL(3)):                                                       !
    !           A three-dimensional vector.                                    !
    !   Outputs:                                                               !
    !       NORM_SQUARED (REAL):                                               !
    !           The magnitude of the vector V.                                 !
    !   Method:                                                                !
    !       Use the Pythagorean formula. ||V||^2 = SUM(V^2)                    !
    !--------------------------------------------------------------------------!
    FUNCTION NORM_SQUARED(P)
        REAL, INTENT(IN) :: P(3)
        REAL :: NORM_SQUARED

        ! By Pythagoras, compute the sum of the squares.
        NORM_SQUARED = SUM(P ** 2)
    END FUNCTION NORM_SQUARED

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       RHO:                                                               !
    !   Purpose:                                                               !
    !       Computes the magnitude of the cylindrical part of a 3D vector.     !
    !   Arguments:                                                             !
    !       V (REAL(3)):                                                       !
    !           A three-dimensional vector.                                    !
    !   Outputs:                                                               !
    !       RHO (REAL):                                                        !
    !           The magnitude of the cylindrical part of V.                    !
    !   Method:                                                                !
    !       Use the Pythagorean formula for the XY component of V.             !
    !--------------------------------------------------------------------------!
    FUNCTION RHO(V)
        IMPLICIT NONE
        REAL, INTENT(IN) :: V(3)
        REAL :: RHO

        ! Projecting V in the xy-plane, we compute rho using Pythagoras.
        RHO = SQRT(V(1)**2 + V(2)**2)
    END FUNCTION RHO

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       RHO_SQUARED:                                                       !
    !   Purpose:                                                               !
    !       Computes the square of the magnitude of the cylindrical part, also !
    !       called the azimuthal part, of a 3D vector.                         !
    !   Arguments:                                                             !
    !       V (REAL(3)):                                                       !
    !           A three-dimensional vector.                                    !
    !   Outputs:                                                               !
    !       RHO_SQUARED (REAL):                                                !
    !           The square of the magnitude of the cylindrical part of V.      !
    !   Method:                                                                !
    !       Use the Pythagorean formula for the XY component of V.             !
    !--------------------------------------------------------------------------!
    FUNCTION RHO_SQUARED(V)
        IMPLICIT NONE
        REAL, INTENT(IN) :: V(3)
        REAL :: RHO_SQUARED

        ! Appeal to the Pythagorean formula for the xy-component of V.
        RHO_SQUARED = V(1)**2 + V(2)**2
    END FUNCTION RHO_SQUARED

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       DOT:                                                               !
    !   Purpose:                                                               !
    !       Computes the dot product of two vectors in R^3.                    !
    !   Arguments:                                                             !
    !       V (REAL(3)):                                                       !
    !           A three-dimensional vector.                                    !
    !       W (REAL(3)):                                                       !
    !           Another three-dimensional vector.                              !
    !   Outputs:                                                               !
    !       V_DOT_W (REAL):                                                    !
    !           The Euclidean dot product of V with W.                         !
    !   Method:                                                                !
    !       Sum the product of the components of V and W and return.           !
    !--------------------------------------------------------------------------!
    FUNCTION DOT(V, W)
        IMPLICIT NONE
        REAL, INTENT(IN) :: V(3)
        REAL, INTENT(IN) :: W(3)
        REAL :: DOT

        ! Sum the product of the individual components to conclude.
        DOT = SUM(V*W)
    END FUNCTION DOT

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       CROSS:                                                             !
    !   Purpose:                                                               !
    !       Computes the cross product of two vectors in R^3.                  !
    !   Arguments:                                                             !
    !       V (REAL(3)):                                                       !
    !           A three-dimensional vector.                                    !
    !       W (REAL(3)):                                                       !
    !           Another three-dimensional vector.                              !
    !   Outputs:                                                               !
    !       V_CROSS_W (REAL(3):                                                !
    !           The Euclidean cross product of V with W.                       !
    !--------------------------------------------------------------------------!
    FUNCTION CROSS(V, W)
        IMPLICIT NONE
        REAL, INTENT(IN) :: V(3)
        REAL, INTENT(IN) :: W(3)
        REAL :: CROSS(3)

        ! Use the determinant of the cross-product matrix to compute.
        CROSS(1) = V(2)*W(3) - V(3)*W(2)
        CROSS(2) = V(3)*W(1) - V(1)*W(3)
        CROSS(3) = V(1)*W(2) - V(2)*W(1)
    END FUNCTION CROSS

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       NORMALIZE:                                                         !
    !   Purpose:                                                               !
    !       Normalizes a vector to unit magnitude.                             !
    !   Arguments:                                                             !
    !       V (REAL(3)):                                                       !
    !           A three-dimensional vector.                                    !
    !   Outputs:                                                               !
    !       V_HAT (REAL(3):                                                    !
    !           The unit vector pointing in the direction of V.                !
    !--------------------------------------------------------------------------!
    FUNCTION NORMALIZE(V)
        IMPLICIT NONE
        REAL, INTENT(IN) :: V(3)
        REAL :: NORM_V
        REAL :: NORM_V_SQ
        REAL :: FACTOR
        REAL :: NORMALIZE(3)

        ! The normalization is V / ||V||. We should check that ||V|| is
        ! non-zero first. Avoid a wasteful sqrt call, compute ||V||^2.
        NORM_V_SQ = NORM_SQUARED(V)

        ! If the input is the zero vector, set the output to be zero as well.
        IF (NORM_V_SQ .EQ. 0.0) THEN
            NORMALIZE = V

        ! Otherwise compute V / ||V|| to normalize the input.
        ELSE
            ! We've already computed ||V||^2. Use this to compute ||V||.
            NORM_V = SQRT(NORM_V_SQ)

            ! The scale factor is the reciprocal of the norm.
            FACTOR = 1.0 / NORM_V
            NORMALIZE = FACTOR * V
        END IF
    END FUNCTION NORMALIZE

    !--------------------------------------------------------------------------!
    !   Subroutine:                                                            !
    !       NORMALIZE_SELF:                                                    !
    !   Purpose:                                                               !
    !       Normalizes a vector to unit magnitude.                             !
    !   Arguments:                                                             !
    !       V (REAL(3)):                                                       !
    !           A three-dimensional vector.                                    !
    !--------------------------------------------------------------------------!
    SUBROUTINE NORMALIZE_SELF(V)
        IMPLICIT NONE
        REAL, INTENT(INOUT) :: V(3)
        REAL :: NORM_V
        REAL :: NORM_V_SQ
        REAL :: FACTOR

        ! The normalization is V / ||V||. We should check that ||V|| is
        ! non-zero first. Avoid a wasteful sqrt call, compute ||V||^2.
        NORM_V_SQ = NORM_SQUARED(V)

        ! If the input is the zero vector, do nothing.
        IF (NORM_V_SQ .EQ. 0.0) THEN
            RETURN

        ! Otherwise compute V / ||V|| to normalize the input.
        ELSE
            ! We've already computed ||V||^2. Use this to compute ||V||.
            NORM_V = SQRT(NORM_V_SQ)

            ! The scale factor is the reciprocal of the norm.
            FACTOR = 1.0 / NORM_V
            V = FACTOR * V
        END IF
    END SUBROUTINE NORMALIZE_SELF

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       COMPONENT:                                                         !
    !   Purpose:                                                               !
    !       Computes the projection of one vector in R^3 along another.        !
    !   Arguments:                                                             !
    !       V (REAL(3)):                                                       !
    !           A three-dimensional vector.                                    !
    !       W (REAL(3)):                                                       !
    !           Another 3D vector, the vector V is projected on to.            !
    !   Output:                                                                !
    !       COMPONENT (REAL(3)):                                               !
    !           The projection of V along the vector W.                        !
    !   Method:                                                                !
    !       The projection formula is proj_{W}(V) = (V . W_hat) * W_hat, where !
    !       W_hat is the unit vector in the W direction: W_hat = W / ||W||.    !
    !       We can rearrange this formula to get ((V . W) / ||W||^2) * W. This !
    !       final formula is used to compute the component of V along W.       !
    !--------------------------------------------------------------------------!
    FUNCTION COMPONENT(V, W)
        REAL, INTENT(IN) :: V(3)
        REAL, INTENT(IN) :: W(3)
        REAL :: FACTOR
        REAL :: COMPONENT(3)

        ! The projection factor is given by the "normalized" dot product.
        FACTOR = DOT(V, W) / NORM_SQUARED(W)
        COMPONENT = FACTOR * W
    END FUNCTION COMPONENT

    !--------------------------------------------------------------------------!
    !   Function:                                                              !
    !       ORTHOGONAL_PART:                                                   !
    !   Purpose:                                                               !
    !       Computes the orthogonal projection of a 3D vector along another.   !
    !   Arguments:                                                             !
    !       V (REAL(3)):                                                       !
    !           A three-dimensional vector.                                    !
    !       W (REAL(3)):                                                       !
    !           Another 3D vector.                                             !
    !   Output:                                                                !
    !       ORTHOGONAL_PART (REAL(3)):                                         !
    !           The orthogonal projection of V along the vector W.             !
    !   Method:                                                                !
    !       Subtract the component proj_{W}(V) from V.                         !
    !--------------------------------------------------------------------------!
    FUNCTION ORTHOGONAL_PART(V, W)
        REAL, INTENT(IN) :: V(3)
        REAL, INTENT(IN) :: W(3)
        REAL :: PROJ_W_OF_V(3)
        REAL :: ORTHOGONAL_PART(3)

        ! Compute the part of V that is parallel to W.
        PROJ_W_OF_V = COMPONENT(V, W)

        ! The orthogonal component is V minus the parallel component.
        ORTHOGONAL_PART = V - PROJ_W_OF_V
    END FUNCTION ORTHOGONAL_PART

    !--------------------------------------------------------------------------!
    !   Subroutine:                                                            !
    !       PATH:                                                              !
    !   Purpose:                                                               !
    !       Given a vector-valued differential equation P'' = acc(P), a        !
    !       stopping condition, and initial values P and V, numerically trace  !
    !       the path P follows using Euler's method.                           !
    !   Arguments:                                                             !
    !       P (REAL(3)):                                                       !
    !           The initial position vector.                                   !
    !       V (REAL(3)):                                                       !
    !           The initial velocity vector.                                   !
    !       ACCELERATION (Function REAL(3) -> REAL(3)):                        !
    !           The vector-valued differential equation, P'' = ACCELERATION(P).!
    !       HALT (Function REAL(3) -> LOGICAL):                                !
    !           The stopping condition for when to halt Euler's method.        !
    !--------------------------------------------------------------------------!
    SUBROUTINE PATH(P, V, ACCELERATION, HALT)
        IMPLICIT NONE
        REAL, INTENT(INOUT) :: P(3)
        REAL, INTENT(INOUT) :: V(3)

        ! Integer for keeping track of the number iterations performed.
        INTEGER :: ITERS

        ! Variable for the acceleration at a given point.
        REAL :: A(3)

        ! Interface for the acceleration function. 3D vector in, 3D vector out.
        INTERFACE
            FUNCTION ACCELERATION(P)
                REAL, INTENT(IN) :: P(3)
                REAL :: ACCELERATION(3)
            END FUNCTION ACCELERATION
        END INTERFACE

        ! Interface for the stopping condition. Outputs a Boolean.
        INTERFACE
            FUNCTION HALT(V)
                REAL, INTENT(IN) :: V(3)
                LOGICAL :: HALT
            END FUNCTION HALT
        END INTERFACE

        ITERS = 0

        ! Iteratively performed Euler's method.
        DO WHILE (ITERS < EULER_MAX_ITERS)

            ! If we reach the detector, or are swallowed by a black hole, abort.
            IF (HALT(P)) THEN
                RETURN
            END IF

            ! Compute the acceleration due to gravity at the current point.
            A = ACCELERATION(P)

            ! Update the position and velocity using Euler's method.
            P = V*EULER_TIME_INCREMENT + P
            V = A*EULER_TIME_INCREMENT + V

            ! Avoid an infinite loop. If ITERS gets too large, abort.
            ITERS = ITERS + 1
        END DO
    END SUBROUTINE PATH

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
        IMPLICIT NONE
        REAL, INTENT(IN) :: P(3)
        REAL :: COLOR_FACTOR
        INTEGER :: CHECKER_BOARD_FOUR(3)

        ! Scale factor for darkening the detector for points far away.
        COLOR_FACTOR = Z_DETECTOR_SQUARED / NORM_SQUARED(P)

        ! Use the general checker board function to get the color.
        CHECKER_BOARD_FOUR = SCALED_CHECKER_BOARD_FOUR(COLOR_FACTOR, P)
    END FUNCTION CHECKER_BOARD_FOUR

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
    SUBROUTINE RUN(ACCELERATION, STOPPER, RAYTRACER)
        IMPLICIT NONE

        INTERFACE
            FUNCTION ACCELERATION(P)
                REAL, INTENT(IN) :: P(3)
                REAL :: ACCELERATION(3)
            END FUNCTION ACCELERATION
        END INTERFACE

        INTERFACE
            FUNCTION STOPPER(P)
                REAL, INTENT(IN) :: P(3)
                LOGICAL :: STOPPER
            END FUNCTION STOPPER
        END INTERFACE

        REAL :: P(3)
        REAL :: V(3)

        REAL :: PROG_FACTOR
        INTEGER :: X, Y
        INTEGER :: C(3)
        INTEGER :: PPM

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
                C = CHECKER_BOARD(P)

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

PROGRAM MAIN
    USE NBH
    IMPLICIT NONE
    CALL RUN(GRAVITY, HALT, PATH)
END PROGRAM MAIN
