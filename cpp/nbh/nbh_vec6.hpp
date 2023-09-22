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
 *      Provides a basic double-precision 6D vector struct (not a class).     *
 *      Used for working in phase space (position, velocity).                 *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/04/04                                                        *
 ******************************************************************************/

/*  Include guard to prevent including this file twice.                       */
#ifndef NBH_VEC6_HPP
#define NBH_VEC6_HPP

/*  3D vectors found here. A 6D vector is two 3D vectors.                     */
#include "nbh_vec3.hpp"

/*  Namespace for the mini-project. "Newtonian Black Holes."                  */
namespace nbh {

    /*  A structure for dealing with vectors in R^6. These are thought of as  *
     *  two vectors in R^3, a position vector and a velocity vector.          */
    struct vec6 {

        /*  A vector is defined by its 3D position and 3D velocity.           */
        nbh::vec3 p, v;

        /*  Empty constructor.                                                */
        vec6(void);

        /*  Simple method for creating a vector from the components.          */
        vec6(double x, double y, double z, double vx, double vy, double vz);

        /*  Simple method for creating a vector from two 3D vectors.          */
        vec6(const nbh::vec3 &pos, const nbh::vec3 &vel);

        /*  Operator for vector addition.                                     */
        inline void operator += (const vec6 &u);

        /*  Operator for vector subtraction.                                  */
        inline void operator -= (const vec6 &u);

        /*  Operator for scalar multiplication.                               */
        inline void operator *= (double r);

        /*  Method for computing the dot product with another vector.         */
        inline double dot(const vec6 &u) const;

        /*  Method for computing the Euclidean, or L2, norm of the vector.    */
        inline double norm(void) const;

        /*  Method for the square of the norm. Removes redundant square root. */
        inline double normsq(void) const;
    };

    /**************************************************************************
     *  Constructor:                                                          *
     *      nbh::vec6                                                         *
     *  Purpose:                                                              *
     *      Creates an "empty" vec6 struct. Nothing is initialized.           *
     *  Arguments:                                                            *
     *      None (void).                                                      *
     *  Outputs:                                                              *
     *      v (nbh::vec6):                                                    *
     *          An uninitialized vector.                                      *
     **************************************************************************/
    vec6::vec6(void)
    {
        return;
    }

    /**************************************************************************
     *  Constructor:                                                          *
     *      nbh::vec6                                                         *
     *  Purpose:                                                              *
     *      Creates a 6D vector from the specified Cartesian coordinates.     *
     *  Arguments:                                                            *
     *      x (double):                                                       *
     *          The x component of the position vector.                       *
     *      y (double):                                                       *
     *          The y component of the position vector.                       *
     *      z (double):                                                       *
     *          The z component of the position vector.                       *
     *      vx (double):                                                      *
     *          The x component of the velocity vector.                       *
     *      vy (double):                                                      *
     *          The y component of the velocity vector.                       *
     *      vz (double):                                                      *
     *          The z component of the velocity vector.                       *
     *  Outputs:                                                              *
     *      u (nbh::vec6):                                                    *
     *          The 6D vector (x, y, z, vx, vy, vz).                          *
     **************************************************************************/
    vec6::vec6(double x, double y, double z, double vx, double vy, double vz)
    {
        /*  Set each of the components and return.                            */
        p.x = x;
        p.y = y;
        p.z = z;
        v.x = vx;
        v.y = vy;
        v.z = vz;
    }

    /**************************************************************************
     *  Constructor:                                                          *
     *      nbh::vec6                                                         *
     *  Purpose:                                                              *
     *      Creates a 6D vector from the specified position and velocity.     *
     *  Arguments:                                                            *
     *      pos (const nbh::vec3 &):                                          *
     *          The position vector.                                          *
     *      vel (const nbh::vec3 &):                                          *
     *          The velocity vector.                                          *
     *  Outputs:                                                              *
     *      v (nbh::vec6):                                                    *
     *          The 6D vector (p, v).                                         *
     **************************************************************************/
    vec6::vec6(const nbh::vec3 &pos, const nbh::vec3 &vel)
    {
        p = pos;
        v = vel;
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      +                                                                 *
     *  Purpose:                                                              *
     *      Adds two vectors in R^6.                                          *
     *  Arguments:                                                            *
     *      u0 (const nbh::vec6 &):                                           *
     *          A 6D vector.                                                  *
     *      u1 (const nbh::vec6 &):                                           *
     *          Another 6D vector.                                            *
     *  Outputs:                                                              *
     *      sum (nbh::vec6):                                                  *
     *          The vector sum of u0 and u1.                                  *
     *  Method:                                                               *
     *      Vector addition is performed component-wise. That is, given two   *
     *      vectors u0 = (p0, v0) and u1 = (p1. v1), the sum is:              *
     *                                                                        *
     *          sum = u0 + u1                                                 *
     *              = (p0, v0) + (p1, v1)                                     *
     *              = (p0 + p1, v0 + v1)                                      *
     *                                                                        *
     *      This is computed and the sum is returned.                         *
     **************************************************************************/
    inline vec6 operator + (const vec6 &u0, const vec6 &u1)
    {
        /*  Use vector addition in R^3 and sum the components.                */
        return vec6(u0.p + u1.p, u0.v + u1.v);
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      +=                                                                *
     *  Purpose:                                                              *
     *      Adds two vectors in R^6 and stores the result in *this*.          *
     *  Arguments:                                                            *
     *      u (const nbh::vec6 &):                                            *
     *          A 6D vector.                                                  *
     *  Outputs:                                                              *
     *      None (void).                                                      *
     *  Method:                                                               *
     *      Add the components of u to *this*.                                *
     **************************************************************************/
    inline void vec6::operator += (const vec6 &u)
    {
        /*  Use vector addition in R^3 and sum the components.                */
        p += u.p;
        v += u.v;
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      -                                                                 *
     *  Purpose:                                                              *
     *      Subtracts two vectors in R^6.                                     *
     *  Arguments:                                                            *
     *      u0 (const nbh::vec6 &):                                           *
     *          A 6D vector.                                                  *
     *      u1 (const nbh::vec6 &):                                           *
     *          Another 6D vector.                                            *
     *  Outputs:                                                              *
     *      diff (nbh::vec6):                                                 *
     *          The vector difference of u0 and u1.                           *
     *  Method:                                                               *
     *      Vector subtraction is performed component-wise. That is, given    *
     *      two vectors u0 = (p0, v0) and u1 = (p1. v1), the difference is:   *
     *                                                                        *
     *          diff = u0 - u1                                                *
     *               = (p0, v0) - (p1, v1)                                    *
     *               = (p0 - p1, v0 - v1)                                     *
     *                                                                        *
     *      This is computed and the difference is returned.                  *
     **************************************************************************/
    inline vec6 operator - (const vec6 &u0, const vec6 &u1)
    {
        /*  Use vector subtraction in R^3 on each component.                  */
        return vec6(u0.p - u1.p, u0.v - u1.v);
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      -=                                                                *
     *  Purpose:                                                              *
     *      Subtracts two vectors in R^6 and stores the result in *this*.     *
     *  Arguments:                                                            *
     *      u (const nbh::vec6 &):                                            *
     *          A 6D vector.                                                  *
     *  Outputs:                                                              *
     *      None (void).                                                      *
     *  Method:                                                               *
     *      Subtract the components of u from *this*.                         *
     **************************************************************************/
    inline void vec6::operator -= (const vec6 &u)
    {
        p -= u.p;
        v -= u.v;
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      *                                                                 *
     *  Purpose:                                                              *
     *      Performs scalar multiplication on the left.                       *
     *  Arguments:                                                            *
     *      a (double):                                                       *
     *          A real number.                                                *
     *      u (const nbh::vec6 &):                                            *
     *          A reference to a vector.                                      *
     *  Outputs:                                                              *
     *      prod (nbh::vec6):                                                 *
     *          The scalar product a*u.                                       *
     *  Method:                                                               *
     *      Scalar multiplication is performed component-wise. That is, if u  *
     *      is the vector u = (p, v), then the product is:                    *
     *                                                                        *
     *          prod = a*u                                                    *
     *               = a*(p, v)                                               *
     *               = (a*p, a*v)                                             *
     *                                                                        *
     *      This is computed and the product is returned.                     *
     **************************************************************************/
    inline vec6 operator * (double a, const vec6 &u)
    {
        return vec6(a*u.p, a*u.v);
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      *                                                                 *
     *  Purpose:                                                              *
     *      Performs scalar multiplication on the right.                      *
     *  Arguments:                                                            *
     *      u (const nbh::vec6 &):                                            *
     *          A reference to a vector.                                      *
     *      a (double):                                                       *
     *          A real number.                                                *
     *  Outputs:                                                              *
     *      prod (nbh::vec6):                                                 *
     *          The scalar product u*a.                                       *
     *  Method:                                                               *
     *      Mimicry of scalar multiplication on the left.                     *
     **************************************************************************/
    inline vec6 operator * (const vec6 &u, double a)
    {
        return vec6(u.p*a, u.v*a);
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
    inline void vec6::operator *= (double r)
    {
        p *= r;
        v *= r;
    }

    /**************************************************************************
     *  Method:                                                               *
     *      dot                                                               *
     *  Purpose:                                                              *
     *      Performs the Euclidean dot product in R^6.                        *
     *  Arguments:                                                            *
     *      u (const nbh::vec6 &):                                            *
     *          A reference to a vector.                                      *
     *  Outputs:                                                              *
     *      dot (double):                                                     *
     *          The dot product *this* . u.                                   *
     *  Method:                                                               *
     *      The Euclidean dot product is linear over the components. That is, *
     *      given two vectors u0 = (p0, v0) and u1 = (p1, v1),                *
     *      the dot product is:                                               *
     *                                                                        *
     *          v.w = (p0, v0) . (p1, v1)                                     *
     *              = (p0 . p1) + (v0 . v1)                                   *
     *                                                                        *
     *      This is computed and the sum of products is returned.             *
     **************************************************************************/
    inline double vec6::dot(const vec6 &u) const
    {
        return p.dot(u.p) + v.dot(u.v);
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      %                                                                 *
     *  Purpose:                                                              *
     *      Provides an operator for the dot product. Shorthand use.          *
     *  Arguments:                                                            *
     *      u0 (const nbh::vec6 &):                                           *
     *          A reference to a vector.                                      *
     *      u1 (const nbh::vec6 &):                                           *
     *          Another reference to a vector.                                *
     *  Outputs:                                                              *
     *      dot (double):                                                     *
     *          The dot product u0 . u1.                                      *
     *  Method:                                                               *
     *      Use the "dot" method and return.                                  *
     **************************************************************************/
    inline double operator % (const vec6 &u0, const vec6 &u1)
    {
        return u0.dot(u1);
    }

    /**************************************************************************
     *  Method:                                                               *
     *      normsq                                                            *
     *  Purpose:                                                              *
     *      Computes the square of the Euclidean norm of a vector in R^6.     *
     *  Arguments:                                                            *
     *      None (void).                                                      *
     *  Outputs:                                                              *
     *      norm_u_sq (double):                                               *
     *          The square of the magnitude of *this*.                        *
     *  Method:                                                               *
     *      Use the Pythagorean theorem. Given u = (p, v) we have:            *
     *                                                                        *
     *          ||u||^2 = ||(p, v)||^2                                        *
     *                  = ||p||^2 + ||v||^2                                   *
     *                                                                        *
     *      Compute and return.                                               *
     **************************************************************************/
    inline double vec6::normsq(void) const
    {
        /*  Return the sum of the squares of the norms of the components.     */
        return p.normsq() + v.normsq();
    }

    /**************************************************************************
     *  Method:                                                               *
     *      norm                                                              *
     *  Purpose:                                                              *
     *      Computes the Euclidean, or L2, norm of a vector in R^6.           *
     *  Arguments:                                                            *
     *      None (void).                                                      *
     *  Outputs:                                                              *
     *      norm_u (double):                                                  *
     *          The norm, or magnitude, or length of the vector u.            *
     *  Method:                                                               *
     *      Return the square root of the normsq method.                      *
     **************************************************************************/
    inline double vec6::norm(void) const
    {
        return std::sqrt(this->normsq());
    }
}
/*  End of namespace nbh.                                                     */

#endif
/*  End of include guard.                                                     */
