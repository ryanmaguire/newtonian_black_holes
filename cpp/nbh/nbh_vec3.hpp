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
 *      Provides a basic double-precision 3D vector struct (not a class).     *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/02/28                                                        *
 ******************************************************************************/

/*  Include guard to prevent including this file twice.                       */
#ifndef NBH_VEC3_HPP
#define NBH_VEC3_HPP

/*  sqrt found here.                                                          */
#include <cmath>

/*  Namespace for the mini-project. "Newtonian Black Holes."                  */
namespace nbh {

    /*  A structure for dealing with vectors. Vectors are treated as rays of  *
     *  light moving under the influence of the gravity of a black hole.      */
    struct vec3 {

        /*  A vector is defined by it's Euclidean components, x, y, z.        */
        double x, y, z;

        /*  Empty constructor.                                                */
        vec3(void);

        /*  Simple method for creating a vector from the components.          */
        vec3(double a, double b, double c);

        /*  Operator for vector addition.                                     */
        inline void operator += (const vec3 &v);

        /*  Operator for vector subtraction.                                  */
        inline void operator -= (const vec3 &v);

        /*  Operator for scalar multiplication.                               */
        inline void operator *= (double r);

        /*  Operator for the cross product.                                   */
        inline void operator ^= (const vec3 &u);

        /*  Method for computing the dot product with another vector.         */
        inline double dot(const vec3 &v) const;

        /*  Three-Dimensional Euclidean cross-product.                        */
        inline vec3 cross(const vec3 &v) const;

        /*  Cross-product where the result is stored in this struct.          */
        inline void crosswith(const vec3 &v);

        /*  Method for computing the Euclidean, or L2, norm of the vector.    */
        inline double norm(void) const;

        /*  Method for the square of the norm. Removes redundant square root. */
        inline double normsq(void) const;

        /*  Computes the azimuthal radius, sqrt(x^2 + y^2).                   */
        inline double rho(void) const;

        /*  Computes the square of the azimuthal radius, x^2 + y^2.           */
        inline double rhosq(void) const;
    };

    /**************************************************************************
     *  Constructor:                                                          *
     *      nbh::vec3                                                         *
     *  Purpose:                                                              *
     *      Creates an "empty" vec3 struct. Nothing is initialized.           *
     *  Arguments:                                                            *
     *      None (void).                                                      *
     *  Outputs:                                                              *
     *      v (nbh::vec3):                                                    *
     *          An uninitialized vector.                                      *
     **************************************************************************/
    vec3::vec3(void)
    {
        return;
    }

    /**************************************************************************
     *  Constructor:                                                          *
     *      nbh::vec3                                                         *
     *  Purpose:                                                              *
     *      Creates a 3D vector from the specified Cartesian coordinates.     *
     *  Arguments:                                                            *
     *      a (double):                                                       *
     *          The x component of the vector.                                *
     *      b (double):                                                       *
     *          The y component of the vector.                                *
     *      c (double):                                                       *
     *          The z component of the vector.                                *
     *  Outputs:                                                              *
     *      v (nbh::vec3):                                                    *
     *          The 3D vector (a, b, c).                                      *
     **************************************************************************/
    vec3::vec3(double a, double b, double c)
    {
        /*  Set each of the components and return.                            */
        x = a;
        y = b;
        z = c;
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      +                                                                 *
     *  Purpose:                                                              *
     *      Adds two vectors in R^3.                                          *
     *  Arguments:                                                            *
     *      v (const nbh::vec3 &):                                            *
     *          A 3D vector.                                                  *
     *      w (const nbh::vec3 &):                                            *
     *          Another 3D vector.                                            *
     *  Outputs:                                                              *
     *      sum (nbh::vec3):                                                  *
     *          The vector sum of v and w.                                    *
     *  Method:                                                               *
     *      Vector addition is performed component-wise. That is, given two   *
     *      vectors v = (vx, vy, vz) and w = (wx, wy, wz), the sum is:        *
     *                                                                        *
     *          sum = v + w                                                   *
     *              = (vx, vy, vz) + (wx, wy, wz)                             *
     *              = (vx + wx, vy + wy, vz + wz)                             *
     *                                                                        *
     *      This is computed and the sum is returned.                         *
     **************************************************************************/
    inline vec3 operator + (const vec3 &v, const vec3 &u)
    {
        return vec3(v.x + u.x, v.y + u.y, v.z + u.z);
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      +=                                                                *
     *  Purpose:                                                              *
     *      Adds two vectors in R^3 and stores the result in *this*.          *
     *  Arguments:                                                            *
     *      v (const nbh::vec3 &):                                            *
     *          A 3D vector.                                                  *
     *  Outputs:                                                              *
     *      None (void).                                                      *
     *  Method:                                                               *
     *      Add the components of v to *this*.                                *
     **************************************************************************/
    inline void vec3::operator += (const vec3 &v)
    {
        x += v.x;
        y += v.y;
        z += v.z;
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      -                                                                 *
     *  Purpose:                                                              *
     *      Subtracts two vectors in R^3.                                     *
     *  Arguments:                                                            *
     *      v (const nbh::vec3 &):                                            *
     *          A 3D vector.                                                  *
     *      w (const nbh::vec3 &):                                            *
     *          Another 3D vector.                                            *
     *  Outputs:                                                              *
     *      diff (nbh::vec3):                                                 *
     *          The vector difference of v and w.                             *
     *  Method:                                                               *
     *      Vector subtraction is performed component-wise. Given two vectors *
     *      v = (vx, vy, vz) and w = (wx, wy, wz), the difference is:         *
     *                                                                        *
     *          diff = v - w                                                  *
     *               = (vx, vy, vz) - (wx, wy, wz)                            *
     *               = (vx - wx, vy - wy, vz - wz)                            *
     *                                                                        *
     *      This is computed and the difference is returned.                  *
     **************************************************************************/
    inline vec3 operator - (const vec3 &v, const vec3 &u)
    {
        return vec3(v.x - u.x, v.y - u.y, v.z - u.z);
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      -=                                                                *
     *  Purpose:                                                              *
     *      Subtracts two vectors in R^3 and stores the result in *this*.     *
     *  Arguments:                                                            *
     *      v (const nbh::vec3 &):                                            *
     *          A 3D vector.                                                  *
     *  Outputs:                                                              *
     *      None (void).                                                      *
     *  Method:                                                               *
     *      Subtract the components of v from *this*.                         *
     **************************************************************************/
    inline void vec3::operator -= (const vec3 &v)
    {
        x -= v.x;
        y -= v.y;
        z -= v.z;
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      *                                                                 *
     *  Purpose:                                                              *
     *      Performs scalar multiplication on the left.                       *
     *  Arguments:                                                            *
     *      a (double):                                                       *
     *          A real number.                                                *
     *      v (const nbh::vec3 &):                                            *
     *          A reference to a vector.                                      *
     *  Outputs:                                                              *
     *      prod (nbh::vec3):                                                 *
     *          The scalar product a*v.                                       *
     *  Method:                                                               *
     *      Scalar multiplication is performed component-wise. That is, if v  *
     *      is the vector v = (x, y, z), then the product is:                 *
     *                                                                        *
     *          prod = a*v                                                    *
     *               = a*(x, y, z)                                            *
     *               = (a*x, a*y, a*z)                                        *
     *                                                                        *
     *      This is computed and the product is returned.                     *
     **************************************************************************/
    inline vec3 operator * (double a, const vec3 &v)
    {
        return vec3(a*v.x, a*v.y, a*v.z);
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      *                                                                 *
     *  Purpose:                                                              *
     *      Performs scalar multiplication on the right.                      *
     *  Arguments:                                                            *
     *      v (const nbh::vec3 &):                                            *
     *          A reference to a vector.                                      *
     *      a (double):                                                       *
     *          A real number.                                                *
     *  Outputs:                                                              *
     *      prod (nbh::vec3):                                                 *
     *          The scalar product v*a.                                       *
     *  Method:                                                               *
     *      Mimicry of scalar multiplication on the left.                     *
     **************************************************************************/
    inline vec3 operator * (const vec3 &v, double a)
    {
        return vec3(v.x*a, v.y*a, v.z*a);
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      *=                                                                *
     *  Purpose:                                                              *
     *      Performs scalar multiplication and stores the result in *this*.   *
     *  Arguments:                                                            *
     *      a (double):                                                       *
     *          A real number.                                                *
     *  Outputs:                                                              *
     *      None.                                                             *
     *  Method:                                                               *
     *      Multiply the components of *this* by a and return.                *
     **************************************************************************/
    inline void vec3::operator *= (double a)
    {
        x *= a;
        y *= a;
        z *= a;
    }

    /**************************************************************************
     *  Method:                                                               *
     *      dot                                                               *
     *  Purpose:                                                              *
     *      Performs the Euclidean dot product in R^3.                        *
     *  Arguments:                                                            *
     *      v (const nbh::vec3 &):                                            *
     *          A reference to a vector.                                      *
     *  Outputs:                                                              *
     *      dot (double):                                                     *
     *          The dot product *this* . v.                                   *
     *  Method:                                                               *
     *      The Euclidean dot product sums the products of the components.    *
     *      Given two vectors v = (vx, vy, vz) and w = (wx, wy, wz),          *
     *      the dot product is:                                               *
     *                                                                        *
     *          v.w = (vx, vy, vz) . (wx, wy, wz)                             *
     *              = vx*wx + vy*wy + vz*wz                                   *
     *                                                                        *
     *      This is computed and the sum of products is returned.             *
     **************************************************************************/
    inline double vec3::dot(const vec3 &v) const
    {
        return v.x*x + v.y*y + v.z*z;
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      %                                                                 *
     *  Purpose:                                                              *
     *      Provides an operator for the dot product. Shorthand use.          *
     *  Arguments:                                                            *
     *      v (const nbh::vec3 &):                                            *
     *          A reference to a vector.                                      *
     *      w (const nbh::vec3 &):                                            *
     *          Another reference to a vector.                                *
     *  Outputs:                                                              *
     *      dot (double):                                                     *
     *          The dot product v . w.                                        *
     *  Method:                                                               *
     *      Use the "dot" method and return.                                  *
     **************************************************************************/
    inline double operator % (const vec3 &v, const vec3 &u)
    {
        return v.dot(u);
    }

    /**************************************************************************
     *  Method:                                                               *
     *      cross                                                             *
     *  Purpose:                                                              *
     *      Performs the Euclidean cross product in R^3.                      *
     *  Arguments:                                                            *
     *      v (const nbh::vec3 &):                                            *
     *          A reference to a vector.                                      *
     *  Outputs:                                                              *
     *      cross (nbh::vec3):                                                *
     *          The cross product *this* x v.                                 *
     *  Method:                                                               *
     *      The Euclidean cross is given by the right-hand rule. v x w is the *
     *      the unique vector orthogonal to v and w with right-handed         *
     *      orientation and magnitude given by ||v|| ||w|| sin(theta), theta  *
     *      being the angle between v and w. This can be computed explicitly  *
     *      as follows. Given  v = (vx, vy, vz) and w = (wx, wy, wz),         *
     *      the cross product is:                                             *
     *                                                                        *
     *          cross = v x w                                                 *
     *                = (vx, vy, vz) x (wx, wy, wz)                           *
     *                = (vy*wz - vz*wy, vz*wx - vx*wz, vx*wy - vy*wx)         *
     *                                                                        *
     *      These components are computed and the cross product is returned.  *
     **************************************************************************/
    inline vec3 vec3::cross(const vec3 &v) const
    {
        return vec3(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x);
    }

    /**************************************************************************
     *  Method:                                                               *
     *      crosswith                                                         *
     *  Purpose:                                                              *
     *      Performs the cross product and stores the result in *this*.       *
     *  Arguments:                                                            *
     *      v (const nbh::vec3 &):                                            *
     *          A reference to a vector.                                      *
     *  Outputs:                                                              *
     *      None.                                                             *
     *  Method:                                                               *
     *      Mimic the "cross" method, but be careful of overwriting variables.*
     **************************************************************************/
    inline void vec3::crosswith(const vec3 &v)
    {
        /*  Save these variables for later.                                   */
        const double a = x;
        const double b = y;

        /*  Compute the three components of the cross product.                */
        x = b*v.z - z*v.y;
        y = z*v.x - a*v.z;
        z = a*v.y - b*v.x;
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      ^                                                                 *
     *  Purpose:                                                              *
     *      Provides an operator for the cross product. Shorthand use.        *
     *  Arguments:                                                            *
     *      v (const nbh::vec3 &):                                            *
     *          A reference to a vector.                                      *
     *      w (const nbh::vec3 &):                                            *
     *          Another reference to a vector.                                *
     *  Outputs:                                                              *
     *      cross (nbh::vec3):                                                *
     *          The cross product v x w.                                      *
     *  Method:                                                               *
     *      Use the "cross" method and return.                                *
     *  Notes:                                                                *
     *      The "wedge product" is a generalization of the cross product, so  *
     *      it is fitting to use the "^" symbol.                              *
     **************************************************************************/
    inline vec3 operator ^ (const vec3 &v, const vec3 &u)
    {
        return v.cross(u);
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      ^=                                                                *
     *  Purpose:                                                              *
     *      Provides an operator for the cross product. Shorthand use.        *
     *  Arguments:                                                            *
     *      v (const nbh::vec3 &):                                            *
     *          A reference to a vector.                                      *
     *  Outputs:                                                              *
     *      None (void).                                                      *
     *  Method:                                                               *
     *      Use the "crosswith" method and return.                            *
     **************************************************************************/
    inline void vec3::operator ^= (const vec3 &u)
    {
        this->crosswith(u);
    }

    /**************************************************************************
     *  Method:                                                               *
     *      norm                                                              *
     *  Purpose:                                                              *
     *      Computes the Euclidean, or L2, norm of a vector in R^3.           *
     *  Arguments:                                                            *
     *      None (void).                                                      *
     *  Outputs:                                                              *
     *      norm_v (double):                                                  *
     *          The norm, or magnitude, or length of the vector v.            *
     *  Method:                                                               *
     *      Use the Pythagorean theorem. Given v = (x, y, z) we have:         *
     *                                                                        *
     *          ||v|| = ||(x, y, z)||                                         *
     *                = sqrt(x^2 + y^2 + z^2)                                 *
     *                                                                        *
     *      Compute and return the norm.                                      *
     **************************************************************************/
    inline double vec3::norm(void) const
    {
        return std::sqrt(x*x + y*y + z*z);
    }

    /**************************************************************************
     *  Method:                                                               *
     *      normsq                                                            *
     *  Purpose:                                                              *
     *      Computes the square of the Euclidean norm of a vector in R^3.     *
     *  Arguments:                                                            *
     *      None (void).                                                      *
     *  Outputs:                                                              *
     *      norm_v_sq (double):                                               *
     *          The square of the magnitude of *this*.                        *
     *  Method:                                                               *
     *      Use the Pythagorean theorem. Given v = (x, y, z) we have:         *
     *                                                                        *
     *          ||v||^2 = ||(x, y, z)||^2                                     *
     *                  = x^2 + y^2 + z^2                                     *
     *                                                                        *
     *      Compute and return.                                               *
     **************************************************************************/
    inline double vec3::normsq(void) const
    {
        return x*x + y*y + z*z;
    }

    /**************************************************************************
     *  Method:                                                               *
     *      rho                                                               *
     *  Purpose:                                                              *
     *      Computes the magnitude of the azimuthal part of a vector.         *
     *  Arguments:                                                            *
     *      None (void).                                                      *
     *  Outputs:                                                              *
     *      rho_v (double):                                                   *
     *          The magnitude of the azimuthal part of *this*.                *
     *  Method:                                                               *
     *      Use the Pythagorean theorem. Given v = (x, y, z) we have:         *
     *                                                                        *
     *          rho = ||(x, y)||                                              *
     *              = sqrt(x^2 + y^2)                                         *
     *                                                                        *
     *      Compute and return.                                               *
     **************************************************************************/
    inline double vec3::rho(void) const
    {
        return std::sqrt(x*x + y*y);
    }

    /**************************************************************************
     *  Method:                                                               *
     *      rhosq                                                             *
     *  Purpose:                                                              *
     *      Computes the square of magnitude of the azimuthal part of *this*. *
     *  Arguments:                                                            *
     *      None (void).                                                      *
     *  Outputs:                                                              *
     *      rho_v_sq (double):                                                *
     *          The square of the magnitude of the azimuthal part of *this*.  *
     *  Method:                                                               *
     *      Use the Pythagorean theorem. Given v = (x, y, z) we have:         *
     *                                                                        *
     *          rho^2 = ||(x, y)||^2                                          *
     *                = x^2 + y^2                                             *
     *                                                                        *
     *      Compute and return.                                               *
     **************************************************************************/
    inline double vec3::rhosq(void) const
    {
        return x*x + y*y;
    }
}
/*  End of namespace nbh.                                                     */

#endif
/*  End of include guard.                                                     */
