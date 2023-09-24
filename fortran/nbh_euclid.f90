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
!       Provides basic Euclidean geometry routines.                            !
!------------------------------------------------------------------------------!
!   Author: Ryan Maguire                                                       !
!   Date:   2023/09/20                                                         !
!------------------------------------------------------------------------------!
MODULE NBH_EUCLID
    IMPLICIT NONE
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
END MODULE NBH_EUCLID
