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
 *      Provides a basic 3D vector struct and Euclidean geometry tools.       *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/23                                                        *
 ******************************************************************************/
package main

/*  Sqrt function provided here, along with other trig functions.             */
import "math"

/*  Basic struct for vectors in R^3.                                          */
type Vec3 struct {

    /*  A vector is represented by its Cartesian coordinates.                 */
    X, Y, Z float64
}

/******************************************************************************
 *                         Vec3 Functions and Methods                         *
 ******************************************************************************/

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3Rect                                                              *
 *  Purpose:                                                                  *
 *      Creates a 3D vector from "rectangular", or Cartesian, coordinates.    *
 *  Arguments:                                                                *
 *      x (float64):                                                          *
 *          The x-component of the vector.                                    *
 *      y (float64):                                                          *
 *          The y-component of the vector.                                    *
 *      z (float64):                                                          *
 *          The z-component of the vector.                                    *
 *  Outputs:                                                                  *
 *      v (Vec3):                                                             *
 *          The vector (x, y, z).                                             *
 *  Method:                                                                   *
 *      Use the constructor syntax for structs and return.                    *
 ******************************************************************************/
func Vec3Rect(x, y, z float64) Vec3 {
    return Vec3{x, y, z}
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3Add                                                               *
 *  Purpose:                                                                  *
 *      Adds two 3D vectors in R^3.                                           *
 *  Arguments:                                                                *
 *      v (*Vec3):                                                            *
 *          A pointer to a 3D vector.                                         *
 *      w (*Vec3):                                                            *
 *          Another pointer to a 3D vector.                                   *
 *  Outputs:                                                                  *
 *      sum (Vec3):                                                           *
 *          The sum of v and w.                                               *
 *  Method:                                                                   *
 *      Vector addition is performed component-wise. That is, given two       *
 *      vectors v = (vx, vy, vz) and w = (wx, wy, wz), the sum is:            *
 *                                                                            *
 *          sum = v + w                                                       *
 *              = (vx, vy, vz) + (wx, wy, wz)                                 *
 *              = (vx + wx, vy + wy, vz + wz)                                 *
 *                                                                            *
 *      This is computed and the sum is returned.                             *
 ******************************************************************************/
func Vec3Add(v, w *Vec3) Vec3 {
    return Vec3{v.X + w.X, v.Y + w.Y, v.Z + w.Z}
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3AddTo                                                             *
 *  Purpose:                                                                  *
 *      Adds two vectors in R^3 and stores the result in the first variable.  *
 *  Arguments:                                                                *
 *      v (*Vec3):                                                            *
 *          A pointer to a vector. The sum is stored here.                    *
 *      w (*Vec3):                                                            *
 *          Another pointer to a vector.                                      *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 *  Method:                                                                   *
 *      Perform vector addition component-wise and store the result in v.     *
 ******************************************************************************/
func Vec3AddTo(v, w *Vec3) {
    v.X += w.X
    v.Y += w.Y
    v.Z += w.Z
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3Subtract                                                          *
 *  Purpose:                                                                  *
 *      Subtracts two vectors in R^3.                                         *
 *  Arguments:                                                                *
 *      v (*Vec3):                                                            *
 *          A pointer to a vector.                                            *
 *      w (*Vec3):                                                            *
 *          Another pointer to a vector.                                      *
 *  Outputs:                                                                  *
 *      diff (Vec3):                                                          *
 *          The vector difference of v and w, v - w.                          *
 *  Method:                                                                   *
 *      Vector subtraction is performed component-wise. That is, given two    *
 *      vectors v = (vx, vy, vz) and w = (wx, wy, wz), the difference is:     *
 *                                                                            *
 *          diff = v - w                                                      *
 *               = (vx, vy, vz) - (wx, wy, wz)                                *
 *               = (vx - wx, vy - wy, vz - wz)                                *
 *                                                                            *
 *      This is computed and the difference is returned.                      *
 *  Notes:                                                                    *
 *      Vector subtraction is not commutative. Vec3Subtract(&v, &w)           *
 *      computes the difference v - w (and not w - v).                        *
 ******************************************************************************/
func Vec3Subtract(v, w *Vec3) Vec3 {
    return Vec3{v.X - w.X, v.Y - w.Y, v.Z - w.Z}
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3SubtractFrom                                                      *
 *  Purpose:                                                                  *
 *      Subtracts two vectors and stores the result in the first variable.    *
 *  Arguments:                                                                *
 *      v (*Vec3):                                                            *
 *          A pointer to a vector. The difference is stored here.             *
 *      w (*Vec3):                                                            *
 *          Another pointer to a vector.                                      *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 *  Method:                                                                   *
 *      Perform vector subtraction component-wise and store the result in v.  *
 *  Notes:                                                                    *
 *      Vector subtraction is not commutative. Vec3SubtractFrom(&v, &w)       *
 *      computes the difference v - w (and not w - v).                        *
 ******************************************************************************/
func Vec3SubtractFrom(v, w *Vec3) {
    v.X -= w.X
    v.Y -= w.Y
    v.Z -= w.Z
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3Scale                                                             *
 *  Purpose:                                                                  *
 *      Performs scalar multiplication.                                       *
 *  Arguments:                                                                *
 *      a (float64):                                                          *
 *          A real number.                                                    *
 *      v (*Vec3):                                                            *
 *          A pointer to a vector.                                            *
 *  Outputs:                                                                  *
 *      prod (Vec3):                                                          *
 *          The scalar product a*v.                                           *
 *  Method:                                                                   *
 *      Scalar multiplication is performed component-wise. That is, if v is   *
 *      the vector v = (x, y, z), then the product is:                        *
 *                                                                            *
 *          prod = a*v                                                        *
 *               = a*(x, y, z)                                                *
 *               = (a*x, a*y, a*z)                                            *
 *                                                                            *
 *      This is computed and the product is returned.                         *
 ******************************************************************************/
func Vec3Scale(a float64, v *Vec3) Vec3 {
    return Vec3{a*v.X, a*v.Y, a*v.Z}
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3ScaleBy                                                           *
 *  Purpose:                                                                  *
 *      Performs scalar multiplication and stores the result in place.        *
 *  Arguments:                                                                *
 *      a (float64):                                                          *
 *          A real number.                                                    *
 *      v (*Vec3):                                                            *
 *          A pointer to a vector. The product is stored here.                *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 *  Method:                                                                   *
 *      Perform scalar multiplication component-wise, storing the result in v.*
 ******************************************************************************/
func Vec3ScaleBy(a float64, v *Vec3) {
    v.X *= a
    v.Y *= a
    v.Z *= a
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3ScaledAdd                                                         *
 *  Purpose:                                                                  *
 *      Performs scalar multiplication and vector addition, v + a*w           *
 *  Arguments:                                                                *
 *      v (*Vec3):                                                            *
 *          A pointer to a vector.                                            *
 *      a (float64):                                                          *
 *          A real number.                                                    *
 *      w (*Vec3):                                                            *
 *          A pointer to a vector.                                            *
 *  Outputs:                                                                  *
 *      out (Vec3):                                                           *
 *          The vector v + a*w.                                               *
 *  Method:                                                                   *
 *      Perform the operation component-wise.                                 *
 ******************************************************************************/
func Vec3ScaledAdd(v *Vec3, a float64, w *Vec3) Vec3 {
    return Vec3{v.X + a*w.X, v.Y + a*w.Y, v.Z + a*w.Z}
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3ScaledAddTo                                                       *
 *  Purpose:                                                                  *
 *      Performs scalar multiplication and vector addition, v += a*w          *
 *  Arguments:                                                                *
 *      v (*Vec3):                                                            *
 *          A pointer to a vector.                                            *
 *      a (float64):                                                          *
 *          A real number.                                                    *
 *      w (*Vec3):                                                            *
 *          A pointer to a vector.                                            *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 *  Method:                                                                   *
 *      Perform the operation component-wise.                                 *
 ******************************************************************************/
func Vec3ScaledAddTo(v *Vec3, a float64, w *Vec3) {
    v.X += a*w.X
    v.Y += a*w.Y
    v.Z += a*w.Z
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3.Dot                                                              *
 *  Purpose:                                                                  *
 *      Performs the Euclidean dot product in R^3.                            *
 *  Arguments:                                                                *
 *      w (*Vec3):                                                            *
 *          A pointer to a vector.                                            *
 *  Outputs:                                                                  *
 *      dot (float64):                                                        *
 *          The dot product v . w.                                            *
 *  Method:                                                                   *
 *      The Euclidean dot product sums the products of the components. Given  *
 *      two vectors v = (vx, vy, vz) and w = (wx, wy, wz), the dot product is:*
 *                                                                            *
 *          v.w = (vx, vy, vz) . (wx, wy, wz)                                 *
 *              = vx*wx + vy*wy + vz*wz                                       *
 *                                                                            *
 *      This is computed and the sum of products is returned.                 *
 ******************************************************************************/
func (v *Vec3) Dot(w *Vec3) float64 {
    return v.X*w.X + v.Y*w.Y + v.Z*w.Z
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3.Cross                                                            *
 *  Purpose:                                                                  *
 *      Performs the Euclidean cross product in R^3.                          *
 *  Arguments:                                                                *
 *      w (*Vec3):                                                            *
 *          A pointer to a vector.                                            *
 *  Outputs:                                                                  *
 *      cross (Vec3):                                                         *
 *          The cross product v x w.                                          *
 *  Method:                                                                   *
 *      The Euclidean cross is given by the right-hand rule. v x w is the     *
 *      the unique vector orthogonal to v and w with right-handed orientation *
 *      and magnitude given by ||v|| ||w|| sin(theta), theta being the angle  *
 *      between v and w. This can be computed explicitly as follows. Given    *
 *      v = (vx, vy, vz) and w = (wx, wy, wz), the cross product is:          *
 *                                                                            *
 *          cross = v x w                                                     *
 *                = (vx, vy, vz) x (wx, wy, wz)                               *
 *                = (vy*wz - vz*wy, vz*wx - vx*wz, vx*wy - vy*wx)             *
 *                                                                            *
 *      These components are computed and the cross product is returned.      *
 ******************************************************************************/
func (v *Vec3) Cross(w *Vec3) Vec3 {

    /*  Compute each component of the cross product and return.               */
    var cross Vec3
    cross.X = v.Y*w.Z - v.Z*w.Y
    cross.Y = v.Z*w.X - v.X*w.Z
    cross.Z = v.X*w.Y - v.Y*w.X
    return cross
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3.CrossWith                                                        *
 *  Purpose:                                                                  *
 *      Computes the cross product of two vector in-place.                    *
 *  Arguments:                                                                *
 *      w (*Vec3):                                                            *
 *          A pointer to a vector.                                            *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 *  Method:                                                                   *
 *      Compute the cross product component-wise and store the resut in v.    *
 ******************************************************************************/
func (v *Vec3) CrossWith(w *Vec3) {

    /*  Naively computing the cross product and storing the result in v will  *
     *  cause overwrite problems and we will not get the correct result. To   *
     *  avoid this first save the x and y components of v before computing.   */
    var X float64 = v.X
    var Y float64 = v.Y

    /*  Perform the cross product component-wise.                             */
    v.X = Y*w.Z - v.Z*w.Y
    v.Y = v.Z*w.X - X*w.Z
    v.Z = X*w.Y - Y*w.X
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3.Norm                                                             *
 *  Purpose:                                                                  *
 *      Computes the Euclidean, or L2, norm of a vector in R^3.               *
 *  Arguments:                                                                *
 *      v (*Vec3):                                                            *
 *          A pointer to a vector in R^3.                                     *
 *  Outputs:                                                                  *
 *      norm_v (float64):                                                     *
 *          The norm, or magnitude, or length of the vector v.                *
 *  Method:                                                                   *
 *      Use the Pythagorean theorem to compute. Given v = (x, y, z) we have:  *
 *                                                                            *
 *          ||v|| = ||(x, y, z)||                                             *
 *                = sqrt(x^2 + y^2 + z^2)                                     *
 *                                                                            *
 *      Compute and return the norm.                                          *
 ******************************************************************************/
func (v *Vec3) Norm() float64 {
    return math.Sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3.NormSq                                                           *
 *  Purpose:                                                                  *
 *      Computes the square of the Euclidean, or L2, norm for vectors in R^3. *
 *  Arguments:                                                                *
 *      None.                                                                 *
 *  Outputs:                                                                  *
 *      normsq_v (float64):                                                   *
 *          The square of the norm, or magnitude, or length of the vector v.  *
 *  Method:                                                                   *
 *      Use the Pythagorean theorem to compute. Given v = (x, y, z) we have:  *
 *                                                                            *
 *          ||v||^2 = ||(x, y, z)||^2                                         *
 *                  = x^2 + y^2 + z^2                                         *
 *                                                                            *
 *      Compute and return.                                                   *
 ******************************************************************************/
func (v *Vec3) NormSq() float64 {
    return v.X*v.X + v.Y*v.Y + v.Z*v.Z
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3.Rho                                                              *
 *  Purpose:                                                                  *
 *      Computes the magnitude of the azimuthal part of a vector.             *
 *  Arguments:                                                                *
 *      None.                                                                 *
 *  Outputs:                                                                  *
 *      rho (float64):                                                        *
 *          The magnitude of the azimuthal part of v. This is the cylindrical *
 *          radius of v in cylindrical coordinates.                           *
 *  Method:                                                                   *
 *      Use the Pythagorean theorem to compute. Given v = (x, y, z) we have:  *
 *                                                                            *
 *          rho = ||(x, y)||                                                  *
 *              = sqrt(x^2 + y^2)                                             *
 *                                                                            *
 *      Compute and return.                                                   *
 ******************************************************************************/
func (v *Vec3) Rho() float64 {

    /*  Use Pythagoras and compute the square root of the sum of the squares. */
    return math.Sqrt(v.X*v.X + v.Y*v.Y)
}

/******************************************************************************
 *  Function:                                                                 *
 *      Vec3.RhoSq                                                            *
 *  Purpose:                                                                  *
 *      Computes the square of the azimuthal part of a vector.                *
 *  Arguments:                                                                *
 *      None.                                                                 *
 *  Outputs:                                                                  *
 *      rho (float64):                                                        *
 *          The square of the azimuthal part of v.                            *
 *  Method:                                                                   *
 *      Use the Pythagorean theorem to compute. Given v = (x, y, z) we have:  *
 *                                                                            *
 *          rho^2 = ||(x, y)||^2                                              *
 *                = x^2 + y^2                                                 *
 *                                                                            *
 *      Compute and return.                                                   *
 ******************************************************************************/
func (v *Vec3) RhoSq() float64 {

    /*  Use Pythagoras and compute.                                           */
    return v.X*v.X + v.Y*v.Y
}
