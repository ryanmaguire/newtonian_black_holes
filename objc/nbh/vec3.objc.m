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
 *      Provides a basic double-precision 3D vector struct.                   *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2024/10/15                                                        *
 ******************************************************************************/

/*  Check if we are compiling using GCC.                                      */
#ifdef __GNUC__

/*  GCC 12 does not treat Foundation.h as a system header. -Wall and -Wextra  *
 *  will generate warnings from this file. Suppress them.                     */
#pragma GCC system_header
#endif

/*  NSObject found here, base object for the Vec3 class.                      */
#import <Foundation/Foundation.h>

/*  sqrt function found here, normal C header file.                           */
#include <math.h>

/*  C standard library providing printf, FILE, and puts.                      */
#include <stdio.h>

/*  Basic 3D vector class.                                                    */
@interface Vec3:NSObject
{
    /*  Data for the object. Mimic the GSL style, use a contiguous array.     */
    @private
        double dat[3];
}

    /*  Empty constructor, initialize self to the origin.                     */
    - (id) init;

    /*  Create a Vec3 object from three doubles.                              */
    - (id) init: (double)xVal Y: (double)yVal Z: (double)zVal;

    /*  Copies the contents of self to another vector.                        */
    - (id) copy;

    /*  Free's memory allocated to the object.                                */
    - (void) dealloc;

    /*  Setter for the x component of the vector.                             */
    - (id) x: (double) xVal;

    /*  Getter for the x component of the vector.                             */
    - (double) x;

    /*  Setter for the y component of the vector.                             */
    - (id) y: (double) yVal;

    /*  Getter for the y component of the vector.                             */
    - (double) y;

    /*  Setter for the z component of the vector.                             */
    - (id) z: (double) zVal;

    /*  Getter for the z component of the vector.                             */
    - (double) z;

    /*  Computes the square of the Euclidean norm of the vector.              */
    - (double) normSq;

    /*  Computes the Euclidean norm of the vector.                            */
    - (double) norm;

    /*  Computes the Euclidean dot product of self with another vector.       */
    - (double) dot: (const Vec3 * const) other;

    /*  Performs vector addition with another vector.                         */
    - (id) plus: (const Vec3 * const) other;

    /*  Performs vector addition in-place without creating a new variable.    */
    - (id) plusEqual: (const Vec3 * const) other;

    /*  Performs vector subtraction with another vector.                      */
    - (id) minus: (const Vec3 * const) other;

    /*  Performs vector subtraction in-place without creating a new variable. */
    - (id) minusEqual: (const Vec3 * const) other;

    /*  Performs scalar multiplication with a real number.                    */
    - (id) times: (double) scale;

    /*  Performs scalar multiplication in-place without creating a new object.*/
    - (id) timesEqual: (double) scale;

    /*  Computes the standard cross product of self with other.               */
    - (id) cross: (const Vec3 * const) other;

    /*  Computes the cross product in-place without creating a new variable.  */
    - (id) crossEqual: (const Vec3 * const) other;

    /*  Computes the square of the magnitude of the cylindrical part of self. */
    - (double) rhoSq;

    /*  Computes the magnitude of the cylindrical part of self.               */
    - (double) rho;
@end

/*  Implement the Vec3 class with all of its methods.                         */
@implementation Vec3

/******************************************************************************
 *  Method:                                                                   *
 *      init                                                                  *
 *  Purpose:                                                                  *
 *      Creates a vector from three doubles.                                  *
 *  Arguments:                                                                *
 *      xVal (double):                                                        *
 *          The x component of the point.                                     *
 *      yVal (double):                                                        *
 *          The y component of the point.                                     *
 *      zVal (double):                                                        *
 *          The z component of the point.                                     *
 *  Output:                                                                   *
 *      self (Vec3 *):                                                        *
 *          A pointer to a Vec3 object.                                       *
 ******************************************************************************/
- (id) init
{
    self = [super init];

    /*  Ensure that the object is not nil.                                    */
    if (!self)
        return self;

    /*  Otherwise, set the object equal to the zero vector and return.        */
    dat[0] = 0.0;
    dat[1] = 0.0;
    dat[2] = 0.0;

    return self;
}
/*  End of init (empty constructor).                                          */

/******************************************************************************
 *  Method:                                                                   *
 *      init                                                                  *
 *  Purpose:                                                                  *
 *      Creates a vector from three doubles.                                  *
 *  Arguments:                                                                *
 *      xVal (double):                                                        *
 *          The x component of the point.                                     *
 *      yVal (double):                                                        *
 *          The y component of the point.                                     *
 *      zVal (double):                                                        *
 *          The z component of the point.                                     *
 *  Output:                                                                   *
 *      self (Vec3 *):                                                        *
 *          A pointer to a Vec3 object.                                       *
 ******************************************************************************/
- (id) init: (double)xVal Y: (double)yVal Z: (double)zVal
{
    self = [super init];

    /*  Ensure that the object is not nil.                                    */
    if (!self)
        return self;

    /*  Otherwise, set the components to the inputs and return.               */
    dat[0] = xVal;
    dat[1] = yVal;
    dat[2] = zVal;

    return self;
}
/*  End of init (from doubles).                                               */

/******************************************************************************
 *  Method:                                                                   *
 *      copy                                                                  *
 *  Purpose:                                                                  *
 *      Copies the contents of self to another vector.                        *
 *  Arguments:                                                                *
 *      None (void).                                                          *
 *  Output:                                                                   *
 *      duplicate (id):                                                       *
 *          A pointer to a new Vec3 instance with the same data as self.      *
 ******************************************************************************/
- (id) copy
{
    Vec3 *duplicate = [Vec3 alloc];

    /*  Ensure that the object is not nil.                                    */
    if (!duplicate)
        return duplicate;

    /*  Otherwise, copy the data from self.                                   */
    [duplicate init: dat[0] Y:dat[1] Z:dat[2]];
    return duplicate;
}

/******************************************************************************
 *  Method:                                                                   *
 *      dealloc                                                               *
 *  Purpose:                                                                  *
 *      Free's the memory allocated to a Vec3 class.                          *
 *  Arguments:                                                                *
 *      None (void).                                                          *
 *  Output:                                                                   *
 *      None (void).                                                          *
 ******************************************************************************/
- (void) dealloc
{
    [super dealloc];
}

/******************************************************************************
 *  Method:                                                                   *
 *      x (Setter)                                                            *
 *  Purpose:                                                                  *
 *      Sets the x component in a Vec3 class.                                 *
 *  Arguments:                                                                *
 *      xVal (double):                                                        *
 *          The x component of the point.                                     *
 *  Output:                                                                   *
 *      self (Vec3 *):                                                        *
 *          A pointer to a Vec3 object.                                       *
 ******************************************************************************/
- (id) x: (double) xVal
{
    dat[0] = xVal;
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      x (Getter)                                                            *
 *  Purpose:                                                                  *
 *      Retrieves the x component in a Vec3 class.                            *
 *  Arguments:                                                                *
 *      None (void):                                                          *
 *  Output:                                                                   *
 *      x (double):                                                           *
 *          The x component of the vector.                                    *
 ******************************************************************************/
- (double) x
{
    return dat[0];
}

/******************************************************************************
 *  Method:                                                                   *
 *      y (Setter)                                                            *
 *  Purpose:                                                                  *
 *      Sets the y component in a Vec3 class.                                 *
 *  Arguments:                                                                *
 *      yVal (double):                                                        *
 *          The y component of the point.                                     *
 *  Output:                                                                   *
 *      self (Vec3 *):                                                        *
 *          A pointer to a Vec3 object.                                       *
 ******************************************************************************/
- (id) y: (double) yVal
{
    dat[1] = yVal;
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      y (Getter)                                                            *
 *  Purpose:                                                                  *
 *      Retrieves the y component in a Vec3 class.                            *
 *  Arguments:                                                                *
 *      None (void):                                                          *
 *  Output:                                                                   *
 *      y (double):                                                           *
 *          The y component of the vector.                                    *
 ******************************************************************************/
- (double) y
{
   return dat[1];
}

/******************************************************************************
 *  Method:                                                                   *
 *      z (Setter)                                                            *
 *  Purpose:                                                                  *
 *      Sets the z component in a Vec3 class.                                 *
 *  Arguments:                                                                *
 *      zVal (double):                                                        *
 *          The z component of the point.                                     *
 *  Output:                                                                   *
 *      self (Vec3 *):                                                        *
 *          A pointer to a Vec3 object.                                       *
 ******************************************************************************/
- (id) z: (double) zVal
{
   dat[2] = zVal;
   return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      z (Getter)                                                            *
 *  Purpose:                                                                  *
 *      Retrieves the z component in a Vec3 class.                            *
 *  Arguments:                                                                *
 *      None (void):                                                          *
 *  Output:                                                                   *
 *      z (double):                                                           *
 *          The z component of the vector.                                    *
 ******************************************************************************/
- (double) z
{
   return dat[2];
}

/******************************************************************************
 *  Method:                                                                   *
 *      normSq                                                                *
 *  Purpose:                                                                  *
 *      Computes the square of the Euclidean norm of the vector.              *
 *  Arguments:                                                                *
 *      None (void).                                                          *
 *  Output:                                                                   *
 *      norm_squared (double):                                                *
 *          The square of the Euclidean norm of the input.                    *
 ******************************************************************************/
- (double) normSq
{
   return dat[0]*dat[0] + dat[1]*dat[1] + dat[2]*dat[2];
}

/******************************************************************************
 *  Method:                                                                   *
 *      norm                                                                  *
 *  Purpose:                                                                  *
 *      Computes the Euclidean norm of the vector.                            *
 *  Arguments:                                                                *
 *      None (void).                                                          *
 *  Output:                                                                   *
 *      norm (double):                                                        *
 *          The Euclidean norm of the input.                                  *
 ******************************************************************************/
- (double) norm
{
    return sqrt([self normSq]);
}

/******************************************************************************
 *  Method:                                                                   *
 *      dot                                                                   *
 *  Purpose:                                                                  *
 *      Computes the Euclidean dot product with another vector.               *
 *  Arguments:                                                                *
 *      other (const Vec3 * const):                                           *
 *          Another Vec3 class.                                               *
 *  Output:                                                                   *
 *      dot_product (double):                                                 *
 *          The Euclidean dot product of self and other.                      *
 ******************************************************************************/
- (double) dot: (const Vec3 * const) other
{
    const double dx = [self x] * [other x];
    const double dy = [self y] * [other y];
    const double dz = [self z] * [other z];
    return dx + dy + dz;
}

/******************************************************************************
 *  Method:                                                                   *
 *      plus                                                                  *
 *  Purpose:                                                                  *
 *      Performs vector addition.                                             *
 *  Arguments:                                                                *
 *      other (const Vec3 * const):                                           *
 *          Another Vec3 class.                                               *
 *  Output:                                                                   *
 *      sum (Vec3 *):                                                         *
 *          The sum of self and other.                                        *
 ******************************************************************************/
- (id) plus: (const Vec3 * const) other
{
    const double x = [self x] + [other x];
    const double y = [self y] + [other y];
    const double z = [self z] + [other z];
    Vec3 *sum = [Vec3 alloc];

    /*  Check to make sure alloc didn't fail.                                 */
    if (!sum)
        return sum;

    return [sum init: x Y:y Z:z];
}

/******************************************************************************
 *  Method:                                                                   *
 *      plusEqual                                                             *
 *  Purpose:                                                                  *
 *      Performs vector addition in-place.                                    *
 *  Arguments:                                                                *
 *      other (const Vec3 * const):                                           *
 *          Another Vec3 class.                                               *
 *  Output:                                                                   *
 *      self (Vec3 *):                                                        *
 *          A pointer to the vector calling this method.                      *
 ******************************************************************************/
- (id) plusEqual: (const Vec3 * const) other
{
    dat[0] += [other x];
    dat[1] += [other y];
    dat[2] += [other z];
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      minus                                                                 *
 *  Purpose:                                                                  *
 *      Performs vector subtraction.                                          *
 *  Arguments:                                                                *
 *      other (const Vec3 * const):                                           *
 *          Another Vec3 class.                                               *
 *  Output:                                                                   *
 *      diff (Vec3 *):                                                        *
 *          The difference of self and other.                                 *
 ******************************************************************************/
- (id) minus: (const Vec3 * const) other
{
    const double x = [self x] - [other x];
    const double y = [self y] - [other y];
    const double z = [self z] - [other z];
    Vec3 *diff = [Vec3 alloc];

    /*  Check to make sure alloc didn't fail.                                 */
    if (!diff)
        return diff;

    return [diff init: x Y:y Z:z];
}

/******************************************************************************
 *  Method:                                                                   *
 *      minusEqual                                                            *
 *  Purpose:                                                                  *
 *      Performs vector subtraction in-place.                                 *
 *  Arguments:                                                                *
 *      other (const Vec3 * const):                                           *
 *          Another Vec3 class.                                               *
 *  Output:                                                                   *
 *      self (Vec3 *):                                                        *
 *          A pointer to the vector calling this method.                      *
 ******************************************************************************/
- (id) minusEqual: (const Vec3 * const) other
{
    dat[0] -= [other x];
    dat[1] -= [other y];
    dat[2] -= [other z];
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      times                                                                 *
 *  Purpose:                                                                  *
 *      Performs scalar multiplication.                                       *
 *  Arguments:                                                                *
 *      scale (double):                                                       *
 *          The scalar multiplier.                                            *
 *  Output:                                                                   *
 *      prod (Vec3 *):                                                        *
 *          The product of self and scale.                                    *
 ******************************************************************************/
- (id) times: (double) scale
{
    const double x = scale * [self x];
    const double y = scale * [self y];
    const double z = scale * [self z];
    Vec3 *prod = [Vec3 alloc];

    /*  Check to make sure alloc didn't fail.                                 */
    if (!prod)
        return prod;

    return [prod init: x Y:y Z:z];
}

/******************************************************************************
 *  Method:                                                                   *
 *      timesEqual                                                            *
 *  Purpose:                                                                  *
 *      Performs scalar multiplication in-place.                              *
 *  Arguments:                                                                *
 *      scale (double):                                                       *
 *          The scalar multiplier.                                            *
 *  Output:                                                                   *
 *      self (Vec3 *):                                                        *
 *          A pointer to the vector calling this method.                      *
 ******************************************************************************/
- (id) timesEqual: (double) scale
{
    dat[0] *= scale;
    dat[1] *= scale;
    dat[2] *= scale;
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      cross                                                                 *
 *  Purpose:                                                                  *
 *      Computes the standard cross product in three dimensions.              *
 *  Arguments:                                                                *
 *      other (const Vec3 * const):                                           *
 *          Another Vec3 class.                                               *
 *  Output:                                                                   *
 *      prod (Vec3 *):                                                        *
 *          The cross product of self and other.                              *
 ******************************************************************************/
- (id) cross: (const Vec3 * const) other
{
    const double x = [self y] * [other z] - [self z] * [other y];
    const double y = [self z] * [other x] - [self x] * [other z];
    const double z = [self x] * [other y] - [self y] * [other x];
    Vec3 *prod = [Vec3 alloc];

    /*  Check to make sure alloc didn't fail.                                 */
    if (!prod)
        return prod;

    return [prod init: x Y:y Z:z];
}

/******************************************************************************
 *  Method:                                                                   *
 *      crossEqual                                                            *
 *  Purpose:                                                                  *
 *      Computes the standard cross product in-place.                         *
 *  Arguments:                                                                *
 *      other (const Vec3 * const):                                           *
 *          Another Vec3 class.                                               *
 *  Output:                                                                   *
 *      self (Vec3 *):                                                        *
 *          A pointer to the vector calling this method.                      *
 ******************************************************************************/
- (id) crossEqual: (const Vec3 * const) other
{
    const double x = dat[0];
    const double y = dat[1];

    dat[0] = y * [other z] - [self z] * [other y];
    dat[1] = [self z] * [other x] - x * [other z];
    dat[2] = x * [other y] - y * [other x];
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      rhoSq                                                                 *
 *  Purpose:                                                                  *
 *      Computes the square of the magnitude of the cylindrical component.    *
 *  Arguments:                                                                *
 *      None (void).                                                          *
 *  Output:                                                                   *
 *      rho_squared (double):                                                 *
 *          The square of the magnitude of the cylindrical part of self.      *
 ******************************************************************************/
- (double) rhoSq
{
   return dat[0]*dat[0] + dat[1]*dat[1];
}

/******************************************************************************
 *  Method:                                                                   *
 *      rho                                                                   *
 *  Purpose:                                                                  *
 *      Computes the magnitude of the cylindrical component.                  *
 *  Arguments:                                                                *
 *      None (void).                                                          *
 *  Output:                                                                   *
 *     rho_norm (double):                                                     *
 *          The magnitude of the cylindrical part of self.                    *
 ******************************************************************************/
- (double) rho
{
   return sqrt([self rhoSq]);
}


@end
