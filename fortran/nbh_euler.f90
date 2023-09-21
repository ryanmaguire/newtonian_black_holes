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
!       Provides Euler's method for numerically solving ODE's.                 !
!------------------------------------------------------------------------------!
!   Author: Ryan Maguire                                                       !
!   Date:   2023/09/20                                                         !
!------------------------------------------------------------------------------!
MODULE EULER
    IMPLICIT NONE

    ! Maximum number of iterations allowed in Euler's method.
    INTEGER :: EULER_MAX_ITERS = 65535

    ! Step-size for Euler's method.
    REAL :: EULER_TIME_INCREMENT = 0.01

    CONTAINS

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
END MODULE EULER
