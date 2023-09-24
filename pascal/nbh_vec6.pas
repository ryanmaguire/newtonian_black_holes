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
UNIT nbh_Vec6;

INTERFACE

(*  3D vectors are used to represent 6D vectors using "phase-space."          *)
USES nbh_Vec3;

TYPE
    (*  A 6D vector consists of two 3D vectors: Position and velocity.        *)
    Vec6EnumType = 0..1;
    Vec6 = Array[Vec6EnumType] of Vec3;

(*  Computes a 6D vector from two 3D vectors, the position and velocity.      *)
Function Vec6FromVectors(const P, V: Vec3) : Vec6;

(*  Computes a 6D vector from 6 real numbers, the individual components.      *)
Function Vec6FromReals(const x, y, z, vx, vy, vz: Real) : Vec6;

(*  Performs vector addition for two vectors in R^6.                          *)
Function Vec6Add(const U0, U1: Vec6) : Vec6;

(*  Performs vector addition in R^6 and stores the result in U0.              *)
Procedure Vec6AddTo(var U0: Vec6; const U1: Vec6);

(*  Performs vector subtraction for two vectors in R^6.                       *)
Function Vec6Subtract(const U0, U1: Vec6) : Vec6;

(*  Performs vector subtraction and stores the result in U0.                  *)
Procedure Vec6SubtractFrom(var U0: Vec6; const U1: Vec6);

(*  Performs scalar multiplication for 6D vectors.                            *)
Function Vec6Scale(const a: Real; const U: Vec6) : Vec6;

(*  Performs scalar multiplication and stores the result in U.                *)
Procedure Vec6ScaleBy(const a: Real; var U: Vec6);

(*  Performs the Euclidean dot product in R^6.                                *)
Function Vec6DotProduct(const U0, U1: Vec6) : Real;

(*  Computes the square of the Euclidean norm for vectors in R^6.             *)
Function Vec6NormSq(const U: Vec6) : Real;

(*  Computes the magnitude, or length, of vectors in R^6.                     *)
Function Vec6Norm(const U: Vec6) : Real;

IMPLEMENTATION

(******************************************************************************
 *  Function:                                                                 *
 *      Vec6FromVectors                                                       *
 *  Purpose:                                                                  *
 *      Returns a 6D vector from the specified position and velocity vectors. *
 *  Arguments:                                                                *
 *      P (const Vec3):                                                       *
 *          The position vector.                                              *
 *      V (const Vec3):                                                       *
 *          The velocity vector.                                              *
 *  Output:                                                                   *
 *      Vec6FromVectors (Vec6):                                               *
 *          The vector (P, V).                                                *
 ******************************************************************************)
Function Vec6FromVectors(const P, V: Vec3) : Vec6;
BEGIN
    Vec6FromVectors[0] := P;
    Vec6FromVectors[1] := V;
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec6FromReals                                                         *
 *  Purpose:                                                                  *
 *      Returns a 6D vector from the specified Cartesian coordinates.         *
 *  Arguments:                                                                *
 *      x (const Real):                                                       *
 *          The x-coordinate of the position vector.                          *
 *      y (const Real):                                                       *
 *          The y-coordinate of the position vector.                          *
 *      z (const Real):                                                       *
 *          The z-coordinate of the position vector.                          *
 *      vx (const Real):                                                      *
 *          The x-coordinate of the velocity vector.                          *
 *      vy (const Real):                                                      *
 *          The y-coordinate of the velocity vector.                          *
 *      vz (const Real):                                                      *
 *          The z-coordinate of the velocity vector.                          *
 *  Output:                                                                   *
 *      Vec6FromReals (Vec6):                                                 *
 *          The vector (x, y, z, vx, vy, vz).                                 *
 ******************************************************************************)
Function Vec6FromReals(const x, y, z, vx, vy, vz: Real) : Vec6;
VAR
    (*  Two variables representing the position and velocity components.      *)
    P, V: Vec3;

BEGIN
    (*  Compute the individual position and velocity components and return.   *)
    P := Vec3Rect(x, y, z);
    V := Vec3Rect(vx, vy, vz);
    Vec6FromReals := Vec6FromVectors(P, V);
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec6Add                                                               *
 *  Purpose:                                                                  *
 *      Performs vector addition in R^6.                                      *
 *  Arguments:                                                                *
 *      U0 (const Vec6):                                                      *
 *          A vector in R^6.                                                  *
 *      U1 (const Vec6):                                                      *
 *          Another vector in R^6.                                            *
 *  Output:                                                                   *
 *      Vec6Add (Vec6):                                                       *
 *          The vector sum U0 + U1.                                           *
 ******************************************************************************)
Function Vec6Add(const U0, U1: Vec6) : Vec6;
BEGIN
    Vec6Add[0] := Vec3Add(U0[0], U1[0]);
    Vec6Add[1] := Vec3Add(U0[1], U1[1]);
END;

(******************************************************************************
 *  Procedure:                                                                *
 *      Vec6AddTo                                                             *
 *  Purpose:                                                                  *
 *      Performs vector addition in R^6.                                      *
 *  Arguments:                                                                *
 *      U0 (var Vec6):                                                        *
 *          A vector in R^6. The sum is stored here.                          *
 *      U1 (const Vec6):                                                      *
 *          Another vector in R^6.                                            *
 ******************************************************************************)
Procedure Vec6AddTo(var U0: Vec6; const U1: Vec6);
BEGIN
    Vec3AddTo(U0[0], U1[0]);
    Vec3AddTo(U0[1], U1[1]);
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec36ubtract                                                          *
 *  Purpose:                                                                  *
 *      Performs vector subtraction in R^6.                                   *
 *  Arguments:                                                                *
 *      U0 (const Vec6):                                                      *
 *          A vector in R^6.                                                  *
 *      U1 (const Vec6):                                                      *
 *          Another vector in R^6.                                            *
 *  Output:                                                                   *
 *      Vec6Subtract (Vec6):                                                  *
 *          The vector difference U0 - U1.                                    *
 ******************************************************************************)
Function Vec6Subtract(const U0, U1: Vec6) : Vec6;
BEGIN
    Vec6Subtract[0] := Vec3Subtract(U0[0], U1[0]);
    Vec6Subtract[1] := Vec3Subtract(U0[1], U1[1]);
END;

(******************************************************************************
 *  Procedure:                                                                *
 *      Vec6SubtractFrom                                                      *
 *  Purpose:                                                                  *
 *      Performs vector subtraction in R^6.                                   *
 *  Arguments:                                                                *
 *      U0 (var Vec6):                                                        *
 *          A vector in R^6. The difference is stored here.                   *
 *      U1 (const Vec6):                                                      *
 *          Another vector in R^6.                                            *
 ******************************************************************************)
Procedure Vec6SubtractFrom(var U0: Vec6; const U1: Vec6);
BEGIN
    Vec3SubtractFrom(U0[0], U1[0]);
    Vec3SubtractFrom(U0[1], U1[1]);
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec6Scale                                                             *
 *  Purpose:                                                                  *
 *      Performs scalar multiplication for vectors in R^6.                    *
 *  Arguments:                                                                *
 *      a (const Real):                                                       *
 *          A real number, the scalar multiplier.                             *
 *      U (const Vec6):                                                       *
 *          A vector in R^6.                                                  *
 *  Output:                                                                   *
 *      Vec6Scale (Vec6):                                                     *
 *          The scalar product a*U.                                           *
 ******************************************************************************)
Function Vec6Scale(const a: Real; const U: Vec6) : Vec6;
BEGIN
    Vec6Scale[0] := Vec3Scale(a, U[0]);
    Vec6Scale[1] := Vec3Scale(a, U[1]);
END;

(******************************************************************************
 *  Procedure:                                                                *
 *      Vec6ScaleBy                                                           *
 *  Purpose:                                                                  *
 *      Performs scalar multiplication and stores the result in U.            *
 *  Arguments:                                                                *
 *      a (const Real):                                                       *
 *          A real number, the scalar multiplier.                             *
 *      U (var Vec6):                                                         *
 *          A vector in R^6. The product is stored here.                      *
 ******************************************************************************)
Procedure Vec6ScaleBy(const a: Real; var U: Vec6);
BEGIN
    Vec3ScaleBy(a, U[0]);
    Vec3ScaleBy(a, U[1]);
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec6DotProduct                                                        *
 *  Purpose:                                                                  *
 *      Computes the Euclidean dot product of two vectors in R^6.             *
 *  Arguments:                                                                *
 *      U0 (const Vec6):                                                      *
 *          A vector in R^6.                                                  *
 *      U1 (const Vec6):                                                      *
 *          Another vector in R^6.                                            *
 *  Output:                                                                   *
 *      Vec6DotProduct (Real):                                                *
 *          The Euclidean dot product U0 . U1.                                *
 ******************************************************************************)
Function Vec6DotProduct(const U0, U1: Vec6) : Real;
BEGIN
    Vec6DotProduct := Vec3DotProduct(U0[0], U1[0]) +
                      Vec3DotProduct(U0[1], U1[1]);
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec6NormSq                                                            *
 *  Purpose:                                                                  *
 *      Computes the square of the Euclidean norm of a vector in R^6.         *
 *  Arguments:                                                                *
 *      U (const Vec6):                                                       *
 *          A vector in R^6.                                                  *
 *  Output:                                                                   *
 *      Vec6NormSq (Real):                                                    *
 *          The square of the Euclidean norm of U, ||U||^2.                   *
 ******************************************************************************)
Function Vec6NormSq(const U: Vec6) : Real;
BEGIN
    Vec6NormSq := Vec3NormSq(U[0]) + Vec3NormSq(U[1]);
END;

(******************************************************************************
 *  Function:                                                                 *
 *      Vec6Norm                                                              *
 *  Purpose:                                                                  *
 *      Computes the Euclidean norm of a vector in R^6.                       *
 *  Arguments:                                                                *
 *      U (const Vec6):                                                       *
 *          A vector in R^6.                                                  *
 *  Output:                                                                   *
 *      Vec3Norm (Real):                                                      *
 *          The Euclidean norm of U, ||U||.                                   *
 ******************************************************************************)
Function Vec6Norm(const U: Vec6) : Real;
BEGIN
    Vec6Norm := Sqrt(Vec6NormSq(U));
END;

END.
