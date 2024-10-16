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
 *      Provides a basic double-precision 6D vector struct.                   *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2024/10/16                                                        *
 ******************************************************************************/

/*  Check if we are compiling using GCC.                                      */
#ifdef __GNUC__

/*  GCC 12 does not treat Foundation.h as a system header. -Wall and -Wextra  *
 *  will generate warnings from this file. Suppress them.                     */
#pragma GCC system_header
#endif

/*  NSObject found here, base object for the Vec6 class.                      */
#import <Foundation/Foundation.h>

/*  sqrt function found here, normal C header file.                           */
#include <math.h>

/*  C standard library providing printf, FILE, and puts.                      */
#include <stdio.h>

/*  Vec3 class provided here.                                                 */
#import "vec3.M"

/*  Basic 6D vector class.                                                    */
@interface Vec6:NSObject
{
    /*  Data for the object. Consists of 3D position and velocity vectors.    */
    @private
        Vec3 *pos, *vel;
}

    /*  Empty constructor, initialize self to the origin.                     */
    - (id) init;

    /*  Create a Vec6 object from six doubles.                                */
    - (id) init:
        (double) pxVal PY: (double) pyVal PZ: (double) pzVal
        VX: (double) vxVal VY: (double) vyVal VZ: (double) vzVal;

    /*  Create a Vec6 object from position and velocity.                      */
    - (id) init: (const Vec3 * const) p Vel: (const Vec3 * const) v;

    /*  Free's memory allocated to the object.                                */
    - (void) dealloc;

    /*  Setter for the x component of the position vector.                    */
    - (id) px: (double) xVal;

    /*  Getter for the x component of the position vector.                    */
    - (double) px;

    /*  Setter for the y component of the position vector.                    */
    - (id) py: (double) yVal;

    /*  Getter for the y component of the position vector.                    */
    - (double) py;

    /*  Setter for the z component of the position vector.                    */
    - (id) pz: (double) zVal;

    /*  Getter for the z component of the position vector.                    */
    - (double) pz;

    /*  Setter for the x component of the velocity vector.                    */
    - (id) vx: (double) xVal;

    /*  Getter for the x component of the velocity vector.                    */
    - (double) vx;

    /*  Setter for the y component of the velocity vector.                    */
    - (id) vy: (double) yVal;

    /*  Getter for the y component of the velocity vector.                    */
    - (double) vy;

    /*  Setter for the z component of the velocity vector.                    */
    - (id) vz: (double) zVal;

    /*  Getter for the z component of the velocity vector.                    */
    - (double) vz;

    /*  Computes the square of the Euclidean norm of the vector.              */
    - (double) normSq;

    /*  Computes the Euclidean norm of the vector.                            */
    - (double) norm;

    /*  Computes the Euclidean dot product of self with another vector.       */
    - (double) dot: (Vec6 *) other;

    /*  Performs vector addition with another vector.                         */
    - (id) plus: (Vec6 *) other;

    /*  Performs vector addition in-place without creating a new variable.    */
    - (id) plusEqual: (Vec6 *) other;

    /*  Performs vector subtraction with another vector.                      */
    - (id) minus: (Vec6 *) other;

    /*  Performs vector subtraction in-place without creating a new variable. */
    - (id) minusEqual: (Vec6 *) other;

    /*  Performs scalar multiplication with a real number.                    */
    - (id) times: (double) scale;

    /*  Performs scalar multiplication in-place without creating a new object.*/
    - (id) timesEqual: (double) scale;
@end
