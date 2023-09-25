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
END MODULE NBH_EUCLID
