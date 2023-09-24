(******************************************************************************
 *                                  LICENSE                                   *
 ******************************************************************************
 *  This file is part of newtonian_black_holes.                               *
 *                                                                            *
 *  newtonian_black_holes is free software: you can redistribute it and/or    *
 *  modify it under the terms of the GNU General Public License as published  *
 *  by the Free Software Foundation, either version 3 of the License, or      *
 *  (at your option) any later version.                                       *
 *                                                                            *
 *  newtonian_black_holes is distributed in the hope that it will be useful   *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *
 *  GNU General Public License for more details.                              *
 *                                                                            *
 *  You should have received a copy of the GNU General Public License         *
 *  along with newtonian_black_holes.  If not, see                            *
 *  <https://www.gnu.org/licenses/>.                                          *
 ******************************************************************************)
UNIT nbh_Vec3;

INTERFACE

TYPE
    Vec3EnumType = 0..2;
    Vec3 = Array[Vec3EnumType] of Real;

(*  Creates a 3D vector from rectangular / Cartesian coordinates.             *)
Function Vec3Rect(const x, y, z : Real) : Vec3;

(*  Performs vector addition for vectors in R^3.                              *)
Function Vec3Add(const P, Q: Vec3) : Vec3;

(*  Computes vector addition and stores the result in P.                      *)
Procedure Vec3AddTo(var P: Vec3; const Q: Vec3);

(*  Subtracts a 3D vector from another, P - Q.                                *)
Function Vec3Subtract(const P, Q: Vec3) : Vec3;

(*  Computes vector subtraction and stores the result in P.                   *)
Procedure Vec3SubtractFrom(var P: Vec3; const Q: Vec3);

(*  Performs scalar multiplication for vectors in R^3.                        *)
Function Vec3Scale(const a: Real; const P: Vec3) : Vec3;

(*  Performs scalar multiplication and stores the result in P.                *)
Procedure Vec3ScaleBy(const a: Real; var P: Vec3);

(*  Performs the affine transformation out = P + a*Q.                         *)
Function Vec3ScaledAdd(const P: Vec3; const a: Real; const Q: Vec3) : Vec3;

(*  Performs the affine transformation P += a*Q.                              *)
Procedure Vec3ScaledAddTo(var P: Vec3; const a: Real; const Q: Vec3);

(*  Computes the three dimensional cross product.                             *)
Function Vec3CrossProduct(const P, Q: Vec3) : Vec3;

(*  Computes the cross product of two vectors and stores the result in P.     *)
Procedure Vec3CrossWith(var P: Vec3; const Q: Vec3);

(*  Computes the Euclidean dot product of two vectors.                        *)
Function Vec3DotProduct(const P, Q: Vec3) : Real;

(*  Computes the square of the Euclidean norm of a vector.                    *)
Function Vec3NormSq(const P: Vec3) : Real;

(*  Computes the Euclidean, or L2, norm of a vector in R^3.                   *)
Function Vec3Norm(const P: Vec3) : Real;

(*  Computes the square of the magnitude of the azimuthal part of a vector.   *)
Function Vec3RhoSq(const P: Vec3) : Real;

(*  Computes the magnitude of the azimuthal part of a vector.                 *)
Function Vec3Rho(const P: Vec3) : Real;

(*  Prints a vector to the screen.                                            *)
Procedure Vec3Print(const P: Vec3);

IMPLEMENTATION

(******************************************************************************
 *  Function:                                                                 *
 *      Vec3Rect                                                              *
 *  Purpose:                                                                  *
 *      Returns a 3D vector from the specified Cartesian coordinates.         *
 *  Arguments:                                                                *
 *      x (const Real):                                                       *
 *          The x-coordinate of the vector.                                   *
 *      y (const Real):                                                       *
 *          The y-coordinate of the vector.                                   *
 *      z (const Real):                                                       *
 *          The z-coordinate of the vector.                                   *
 *  Output:                                                                   *
 *      Vec3Rect (Vec3):                                                      *
 *          The vector (x, y, z).                                             *
 ******************************************************************************)
Function Vec3Rect(const x, y, z: Real) : Vec3;
BEGIN
   Vec3Rect[0] := x;
   Vec3Rect[1] := y;
   Vec3Rect[2] := z;
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec3Add                                                               *
 *  Purpose:                                                                  *
 *      Performs vector addition in R^3.                                      *
 *  Arguments:                                                                *
 *      P (const Vec3):                                                       *
 *          A vector in R^3.                                                  *
 *      Q (const Vec3):                                                       *
 *          Another vector in R^3.                                            *
 *  Output:                                                                   *
 *      Vec3Add (Vec3):                                                       *
 *          The vector sum P + Q.                                             *
 ******************************************************************************)
Function Vec3Add(const P, Q: Vec3) : Vec3;
BEGIN
    Vec3Add[0] := P[0] + Q[0];
    Vec3Add[1] := P[1] + Q[1];
    Vec3Add[2] := P[2] + Q[2];
END;

(******************************************************************************
 *  Procedure:                                                                *
 *      Vec3AddTo                                                             *
 *  Purpose:                                                                  *
 *      Performs vector addition in R^3.                                      *
 *  Arguments:                                                                *
 *      P (var Vec3):                                                         *
 *          A vector in R^3. The sum is stored here.                          *
 *      Q (const Vec3):                                                       *
 *          Another vector in R^3.                                            *
 ******************************************************************************)
Procedure Vec3AddTo(var P: Vec3; const Q: Vec3);
BEGIN
    P[0] += Q[0];
    P[1] += Q[1];
    P[2] += Q[2];
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec3Subtract                                                          *
 *  Purpose:                                                                  *
 *      Performs vector subtraction in R^3.                                   *
 *  Arguments:                                                                *
 *      P (const Vec3):                                                       *
 *          A vector in R^3.                                                  *
 *      Q (const Vec3):                                                       *
 *          Another vector in R^3.                                            *
 *  Output:                                                                   *
 *      Vec3Subtract (Vec3):                                                  *
 *          The vector difference P - Q.                                      *
 ******************************************************************************)
Function Vec3Subtract(const P, Q: Vec3) : Vec3;
BEGIN
    Vec3Subtract[0] := P[0] - Q[0];
    Vec3Subtract[1] := P[1] - Q[1];
    Vec3Subtract[2] := P[2] - Q[2];
END;

(******************************************************************************
 *  Procedure:                                                                *
 *      Vec3SubtractFrom                                                      *
 *  Purpose:                                                                  *
 *      Performs vector subtraction in R^3.                                   *
 *  Arguments:                                                                *
 *      P (var Vec3):                                                         *
 *          A vector in R^3. The difference is stored here.                   *
 *      Q (const Vec3):                                                       *
 *          Another vector in R^3.                                            *
 ******************************************************************************)
Procedure Vec3SubtractFrom(var P: Vec3; const Q: Vec3);
BEGIN
    P[0] -= Q[0];
    P[1] -= Q[1];
    P[2] -= Q[2];
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec3Scale                                                             *
 *  Purpose:                                                                  *
 *      Performs scalar multiplication for vectors in R^3.                    *
 *  Arguments:                                                                *
 *      a (const Real):                                                       *
 *          A real number, the scalar multiplier.                             *
 *      P (const Vec3):                                                       *
 *          A vector in R^3.                                                  *
 *  Output:                                                                   *
 *      Vec3Scale (Vec3):                                                     *
 *          The scalar product a*P.                                           *
 ******************************************************************************)
Function Vec3Scale(const a: Real; const P: Vec3) : Vec3;
BEGIN
    Vec3Scale[0] := a * P[0];
    Vec3Scale[1] := a * P[1];
    Vec3Scale[2] := a * P[2];
END;

(******************************************************************************
 *  Procedure:                                                                *
 *      Vec3ScaleBy                                                           *
 *  Purpose:                                                                  *
 *      Performs scalar multiplication and stores the result in P.            *
 *  Arguments:                                                                *
 *      a (const Real):                                                       *
 *          A real number, the scalar multiplier.                             *
 *      P (var Vec3):                                                         *
 *          A vector in R^3. The product is stored here.                      *
 ******************************************************************************)
Procedure Vec3ScaleBy(const a: Real; var P: Vec3);
BEGIN
    P[0] *= a;
    P[1] *= a;
    P[2] *= a;
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec3ScaledAdd                                                         *
 *  Purpose:                                                                  *
 *      Performs the affine transformation out = P + a*Q.                     *
 *  Arguments:                                                                *
 *      P (const Vec3):                                                       *
 *          A vector in R^3.                                                  *
 *      a (const Real):                                                       *
 *          A real number, the scalar multiplier for Q.                       *
 *      Q (const Vec3):                                                       *
 *          Another vector in R^3.                                            *
 *  Output:                                                                   *
 *      Vec3ScaledAdd (Vec3):                                                 *
 *          The affine transformation P + a*Q.                                *
 ******************************************************************************)
Function Vec3ScaledAdd(const P: Vec3; const a: Real; const Q: Vec3) : Vec3;
BEGIN
    Vec3ScaledAdd[0] := P[0] + a*Q[0];
    Vec3ScaledAdd[1] := P[1] + a*Q[1];
    Vec3ScaledAdd[2] := P[2] + a*Q[2];
END;

(******************************************************************************
 *  Procedure:                                                                *
 *      Vec3ScaledAddTo                                                       *
 *  Purpose:                                                                  *
 *      Performs the affine transformation P += a*Q.                          *
 *  Arguments:                                                                *
 *      P (var Vec3):                                                         *
 *          A vector in R^3. The result is stored here.                       *
 *      a (const Real):                                                       *
 *          A real number, the scalar multiplier for Q.                       *
 *      Q (const Vec3):                                                       *
 *          Another vector in R^3.                                            *
 ******************************************************************************)
Procedure Vec3ScaledAddTo(var P: Vec3; const a: Real; const Q: Vec3);
BEGIN
    P[0] += a*Q[0];
    P[1] += a*Q[1];
    P[2] += a*Q[2];
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec3CrossProduct                                                      *
 *  Purpose:                                                                  *
 *      Computes the Euclidean cross product of two vectors in R^3.           *
 *  Arguments:                                                                *
 *      P (const Vec3):                                                       *
 *          A vector in R^3.                                                  *
 *      Q (const Vec3):                                                       *
 *          Another vector in R^3.                                            *
 *  Output:                                                                   *
 *      Vec3CrossProduct (Vec3):                                              *
 *          The vector cross product P x Q.                                   *
 ******************************************************************************)
Function Vec3CrossProduct(const P, Q: Vec3) : Vec3;
BEGIN
    Vec3CrossProduct[0] := P[1]*Q[2] - P[2]*Q[1];
    Vec3CrossProduct[1] := P[2]*Q[0] - P[0]*Q[2];
    Vec3CrossProduct[2] := P[0]*Q[1] - P[1]*Q[0];
END;

(******************************************************************************
 *  Procedure:                                                                *
 *      Vec3CrossWith                                                         *
 *  Purpose:                                                                  *
 *      Performs vector subtraction in R^3 and stores the result in P.        *
 *  Arguments:                                                                *
 *      P (var Vec3):                                                         *
 *          A vector in R^3. The product is stored here.                      *
 *      Q (const Vec3):                                                       *
 *          Another vector in R^3.                                            *
 ******************************************************************************)
Procedure Vec3CrossWith(var P: Vec3; const Q: Vec3);
VAR
    X, Y: Real;

BEGIN
    (*  We need to avoid overwriting data, so save the x and y parts of P.    *)
    X := P[0];
    Y := P[1];

    P[0] := Y*Q[2] - P[2]*Q[1];
    P[1] := P[2]*Q[0] - X*Q[2];
    P[2] := X*Q[1] - Y*Q[0];
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec3DotProduct                                                        *
 *  Purpose:                                                                  *
 *      Computes the Euclidean dot product of two vectors in R^3.             *
 *  Arguments:                                                                *
 *      P (const Vec3):                                                       *
 *          A vector in R^3.                                                  *
 *      Q (const Vec3):                                                       *
 *          Another vector in R^3.                                            *
 *  Output:                                                                   *
 *      Vec3DotProduct (Real):                                                *
 *          The Euclidean dot product P . Q.                                  *
 ******************************************************************************)
Function Vec3DotProduct(const P, Q: Vec3) : Real;
BEGIN
    Vec3DotProduct := P[0]*Q[0] + P[1]*Q[1] + P[2]*Q[2];
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec3NormSq                                                            *
 *  Purpose:                                                                  *
 *      Computes the square of the Euclidean norm of a vector in R^3.         *
 *  Arguments:                                                                *
 *      P (const Vec3):                                                       *
 *          A vector in R^3.                                                  *
 *  Output:                                                                   *
 *      Vec3NormSq (Real):                                                    *
 *          The square of the Euclidean norm of P, ||P||^2.                   *
 ******************************************************************************)
Function Vec3NormSq(const P: Vec3) : Real;
BEGIN
    Vec3NormSq := P[0]*P[0] + P[1]*P[1] + P[2]*P[2]
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec3Norm                                                              *
 *  Purpose:                                                                  *
 *      Computes the Euclidean norm of a vector in R^3.                       *
 *  Arguments:                                                                *
 *      P (const Vec3):                                                       *
 *          A vector in R^3.                                                  *
 *  Output:                                                                   *
 *      Vec3Norm (Real):                                                      *
 *          The Euclidean norm of P, ||P||.                                   *
 ******************************************************************************)
Function Vec3Norm(const P: Vec3) : Real;
BEGIN
    Vec3Norm := Sqrt(Vec3NormSq(P))
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec3RhoSq                                                             *
 *  Purpose:                                                                  *
 *      Computes the square of the azimuthal part of a vector in R^2.         *
 *  Arguments:                                                                *
 *      P (const Vec3):                                                       *
 *          A vector in R^3.                                                  *
 *  Output:                                                                   *
 *      Vec3RhoSq (Real):                                                     *
 *          The square of the magnitude of the azimuthal part of P.           *
 ******************************************************************************)
Function Vec3RhoSq(const P: Vec3) : Real;
BEGIN
    Vec3RhoSq := P[0]*P[0] + P[1]*P[1]
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec3Rho                                                               *
 *  Purpose:                                                                  *
 *      Computes the magnitude of the azimuthal part of a vector in R^2.      *
 *  Arguments:                                                                *
 *      P (const Vec3):                                                       *
 *          A vector in R^3.                                                  *
 *  Output:                                                                   *
 *      Vec3RhoSq (Real):                                                     *
 *          The magnitude of the azimuthal part of P.                         *
 ******************************************************************************)
Function Vec3Rho(const P: Vec3) : Real;
BEGIN
    Vec3Rho := Sqrt(Vec3RhoSq(P))
END;

Procedure Vec3Print(const P : Vec3);
BEGIN
    WriteLn('(', P[0], ',', P[1], ',', P[2], ')');
END;

END.
