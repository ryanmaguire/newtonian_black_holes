/******************************************************************************
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
 ******************************************************************************
 *  Purpose:                                                                  *
 *      Provides a basic 6D vector struct to represent "phase-space."         *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/23                                                        *
 ******************************************************************************/
package main

/*  Sqrt function provided here, used for calculating norms.                  */
import "math"

/*  Struct for working in phase space. Points in R^6 are two points in R^3.   */
type Vec6 struct {

    /*  A point in phase-space is defined by its position and velocity.       */
    P, V Vec3
}

/******************************************************************************
 *                         Vec6 Functions and Methods                         *
 ******************************************************************************/

/******************************************************************************
 *  Function:                                                                 *
 *      Vec6FromReals                                                         *
 *  Purpose:                                                                  *
 *      Creates a 6D vector from "rectangular", or Cartesian, coordinates.    *
 *  Arguments:                                                                *
 *      x (float64):                                                          *
 *          The x-component of the position vector.                           *
 *      y (float64):                                                          *
 *          The y-component of the position vector.                           *
 *      z (float64):                                                          *
 *          The z-component of the position vector.                           *
 *      vx (float64):                                                         *
 *          The x-component of the velocity vector.                           *
 *      vy (float64):                                                         *
 *          The y-component of the velocity vector.                           *
 *      vz (float64):                                                         *
 *          The z-component of the velocity vector.                           *
 *  Outputs:                                                                  *
 *      u (Vec6):                                                             *
 *          The vector (x, y, z, vx, vy, vz).                                 *
 *  Method:                                                                   *
 *      Set the individual components for the struct and return.              *
 ******************************************************************************/
func Vec6FromReals(x, y, z, vx, vy, vz float64) {
    var u Vec6
    u.P.X = x
    u.P.Y = y
    u.P.Z = z
    u.V.X = vx
    u.V.Y = vy
    u.V.Z = vz
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec6Add                                                               *
 *  Purpose:                                                                  *
 *      Adds two vectors in R^6.                                              *
 *  Arguments:                                                                *
 *      u0 (*Vec6):                                                           *
 *          A pointer to a vector.                                            *
 *      u1 (*Vec6):                                                           *
 *          Another pointer to a vector.                                      *
 *  Outputs:                                                                  *
 *      sum (Vec6):                                                           *
 *          The vector sum of u0 and u1.                                      *
 *  Method:                                                                   *
 *      Vector addition is performed component-wise. That is, given two       *
 *      vectors u0 = (p0, v0) and u1 = (p1, v1), the sum is:                  *
 *                                                                            *
 *          sum = u0 + u1                                                     *
 *              = (p0, v0) + (p1, v1)                                         *
 *              = (p0 + p1, v0 + v1)                                          *
 *                                                                            *
 *      This is computed and the sum is returned.                             *
 ******************************************************************************/
func Vec6Add(u0, u1 *Vec6) Vec6 {
    return Vec6{Vec3Add(&u0.P, &u1.P), Vec3Add(&u0.V, &u1.V)}
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec6AddTo                                                             *
 *  Purpose:                                                                  *
 *      Adds two vectors in R^6 and stores the result in the first variable.  *
 *  Arguments:                                                                *
 *      u0 (*Vec6):                                                           *
 *          A pointer to a vector. The sum is stored here.                    *
 *      u1 (*Vec6):                                                           *
 *          Another pointer to a vector.                                      *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 *  Method:                                                                   *
 *      Perform vector addition component-wise and store the result in u0.    *
 ******************************************************************************/
func Vec6AddTo(u0, u1 *Vec6) {
    Vec3AddTo(&u0.P, &u1.P)
    Vec3AddTo(&u0.V, &u1.V)
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec6Subtract                                                          *
 *  Purpose:                                                                  *
 *      Subtracts two vectors in R^6.                                         *
 *  Arguments:                                                                *
 *      u0 (*Vec6):                                                           *
 *          A pointer to a vector.                                            *
 *      u1 (*Vec6):                                                           *
 *          Another pointer to a vector.                                      *
 *  Outputs:                                                                  *
 *      diff (Vec6):                                                          *
 *          The vector difference of u0 and u1, u0 - u1.                      *
 *  Method:                                                                   *
 *      Vector subtraction is performed component-wise. That is, given two    *
 *      vectors u0 = (p0, v0) and u1 = (p1, v1), the difference is:           *
 *                                                                            *
 *          diff = u0 - u1                                                    *
 *               = (p0, v0) - (p1, v1)                                        *
 *               = (p0 - p1, v0 - v1)                                         *
 *                                                                            *
 *      This is computed and the difference is returned.                      *
 *  Notes:                                                                    *
 *      Vector subtraction is not commutative. Vec6Subtract(&u0, &u1)         *
 *      computes the difference u0 - u1 (and not u1 - u0).                    *
 ******************************************************************************/
func Vec6Subtract(u0, u1 *Vec6) Vec6 {
    return Vec6{Vec3Subtract(&u0.P, &u1.P), Vec3Subtract(&u0.V, &u1.V)}
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec6SubtractFrom                                                      *
 *  Purpose:                                                                  *
 *      Subtracts two vectors and stores the result in the first variable.    *
 *  Arguments:                                                                *
 *      u0 (*Vec6):                                                           *
 *          A pointer to a vector. The difference is stored here.             *
 *      u1 (*Vec6):                                                           *
 *          Another pointer to a vector.                                      *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 *  Method:                                                                   *
 *      Perform vector subtraction component-wise and store the result in u0. *
 *  Notes:                                                                    *
 *      Vector subtraction is not commutative. Vec6SubtractFrom(&u0, &u1)     *
 *      computes the difference u0 - u1 (and not u1 - u0).                    *
 ******************************************************************************/
func Vec6SubtractFrom(u0, u1 *Vec6) {
    Vec3SubtractFrom(&u0.P, &u1.P)
    Vec3SubtractFrom(&u0.V, &u1.V)
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec6Scale                                                             *
 *  Purpose:                                                                  *
 *      Performs scalar multiplication.                                       *
 *  Arguments:                                                                *
 *      a (float64):                                                          *
 *          A real number.                                                    *
 *      u (*Vec6):                                                            *
 *          A pointer to a vector.                                            *
 *  Outputs:                                                                  *
 *      prod (Vec6):                                                          *
 *          The scalar product a*u.                                           *
 *  Method:                                                                   *
 *      Scalar multiplication is performed component-wise. That is, if u is   *
 *      the vector u = (p, v), then the product is:                           *
 *                                                                            *
 *          prod = a*u                                                        *
 *               = a*(p, v)                                                   *
 *               = (a*p, a*v)                                                 *
 *                                                                            *
 *      This is computed and the product is returned.                         *
 ******************************************************************************/
func Vec6Scale(a float64, u *Vec6) Vec6 {
    return Vec6{Vec3Scale(a, &u.P), Vec3Scale(a, &u.V)}
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec6ScaleBy                                                           *
 *  Purpose:                                                                  *
 *      Performs scalar multiplication and stores the result in place.        *
 *  Arguments:                                                                *
 *      a (float64):                                                          *
 *          A real number.                                                    *
 *      u (*Vec6):                                                            *
 *          A pointer to a vector. The product is stored here.                *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 *  Method:                                                                   *
 *      Perform scalar multiplication component-wise, storing the result in u.*
 ******************************************************************************/
func Vec6ScaleBy(a float64, u *Vec6) {
    Vec3ScaleBy(a, &u.P)
    Vec3ScaleBy(a, &u.V)
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec6.Dot                                                              *
 *  Purpose:                                                                  *
 *      Performs the Euclidean dot product in R^6.                            *
 *  Arguments:                                                                *
 *      u1 (*Vec6):                                                           *
 *          Another pointer to a vector.                                      *
 *  Outputs:                                                                  *
 *      dot (float64):                                                        *
 *          The dot product u0 . u1.                                          *
 *  Method:                                                                   *
 *      The Euclidean dot product sums the products of the components. Given  *
 *      two vectors u0 = (p0, v0) and u1 = (p1, v1), the dot product is:      *
 *                                                                            *
 *          u0 . u1 = (p0, v0) . (p1, v1)                                     *
 *                  = (p0 . p1) + (v0 . v1)                                   *
 *                                                                            *
 *      This is computed and the sum is returned.                             *
 ******************************************************************************/
func (u0 *Vec6) Dot(u1 *Vec6) float64 {
    return u0.P.Dot(&u1.P) + u0.V.Dot(&u1.V)
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec6.NormSq                                                           *
 *  Purpose:                                                                  *
 *      Computes the square of the Euclidean, or L2, norm for vectors in R^6. *
 *  Arguments:                                                                *
 *      None.                                                                 *
 *  Outputs:                                                                  *
 *      normsq_u (float64):                                                   *
 *          The square of the norm, or magnitude, or length of the vector u.  *
 *  Method:                                                                   *
 *      Use the Pythagorean theorem to compute. Given u = (p, v) we have:     *
 *                                                                            *
 *          ||u||^2 = ||(p, v)||^2                                            *
 *                  = ||p||^2 + ||v||^2                                       *
 *                                                                            *
 *      Compute and return.                                                   *
 ******************************************************************************/
func (u *Vec6) NormSq() float64 {
    return u.P.NormSq() + u.V.NormSq()
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec6.Norm                                                             *
 *  Purpose:                                                                  *
 *      Computes the Euclidean, or L2, norm for vectors in R^6.               *
 *  Arguments:                                                                *
 *      None.                                                                 *
 *  Outputs:                                                                  *
 *      norm_u (float64):                                                     *
 *          The norm, or magnitude, or length of the vector u.                *
 *  Method:                                                                   *
 *      Return the square root of the Vec6.NormSq function.                   *
 ******************************************************************************/
func (u *Vec6) Norm() float64 {
    return math.Sqrt(u.NormSq())
}
