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

    /*  Copy the data of self to another 6D vector.                           */
    - (id) copy;

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
    - (double) dot: (const Vec6 * const) other;

    /*  Performs vector addition with another vector.                         */
    - (id) plus: (const Vec6 * const) other;

    /*  Performs vector addition in-place without creating a new variable.    */
    - (id) plusEqual: (const Vec6 * const) other;

    /*  Performs vector subtraction with another vector.                      */
    - (id) minus: (const Vec6 * const) other;

    /*  Performs vector subtraction in-place without creating a new variable. */
    - (id) minusEqual: (const Vec6 * const) other;

    /*  Performs scalar multiplication with a real number.                    */
    - (id) times: (double) scale;

    /*  Performs scalar multiplication in-place without creating a new object.*/
    - (id) timesEqual: (double) scale;
@end
/*  Implement the Vec6 class with all of its methods.                         */
@implementation Vec6

/******************************************************************************
 *  Method:                                                                   *
 *      init                                                                  *
 *  Purpose:                                                                  *
 *      Creates a 6D zero vector.                                             *
 *  Arguments:                                                                *
 *      None (void).                                                          *
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

    /*  Otherwise, allocate memory for the position component.                */
    pos = [Vec3 alloc];

    /*  Same check as before, ensure that pos is not nil.                     */
    if (!pos)
        return self;

    /*  Create the velocity component.                                        */
    vel = [Vec3 alloc];

    /*  alloc worked for pos. Free pos if alloc didn't work for vel.          */
    if (!vel)
        [pos release];

    /*  Use the Vec3 init methods to initialize both components to zero.      */
    pos = [pos init];
    vel = [vel init];
    return self;
}
/*  End of init (empty constructor).                                          */

/******************************************************************************
 *  Method:                                                                   *
 *      init                                                                  *
 *  Purpose:                                                                  *
 *      Creates a vector from three doubles.                                  *
 *  Arguments:                                                                *
 *      pxVal (double):                                                       *
 *          The x component of the position.                                  *
 *      pyVal (double):                                                       *
 *          The y component of the position.                                  *
 *      pzVal (double):                                                       *
 *          The z component of the position.                                  *
 *      vxVal (double):                                                       *
 *          The x component of the velocity.                                  *
 *      vyVal (double):                                                       *
 *          The y component of the velocity.                                  *
 *      vzVal (double):                                                       *
 *          The z component of the velocity.                                  *
 *  Output:                                                                   *
 *      self (Vec3 *):                                                        *
 *          A pointer to a Vec3 object.                                       *
 ******************************************************************************/
- (id) init:
    (double)pxVal PY: (double)pyVal PZ: (double)pzVal
    VX: (double)vxVal VY: (double)vyVal VZ: (double)vzVal
{
    self = [super init];

    /*  Ensure that the object is not nil.                                    */
    if (!self)
        return self;

    /*  Otherwise, allocate memory for the position component.                */
    pos = [Vec3 alloc];

    /*  Same check as before, ensure that pos is not nil.                     */
    if (!pos)
        return self;

    /*  Create the velocity component.                                        */
    vel = [Vec3 alloc];

    /*  alloc worked for pos. Free pos if alloc didn't work for vel.          */
    if (!vel)
        [pos release];

    /*  Otherwise, set the components to the inputs and return.               */
    pos = [pos init: pxVal Y: pyVal Z: pzVal];
    vel = [vel init: vxVal Y: vyVal Z: vzVal];

    return self;
}
/*  End of init (from doubles).                                               */

/******************************************************************************
 *  Method:                                                                   *
 *      init                                                                  *
 *  Purpose:                                                                  *
 *      Creates a vector from three doubles.                                  *
 *  Arguments:                                                                *
 *      pos (const Vec3 *):                                                   *
 *          The position vector.                                              *
 *      vel (const Vec3 *):                                                   *
 *          The velocity vector.                                              *
 *  Output:                                                                   *
 *      self (Vec3 *):                                                        *
 *          A pointer to a Vec3 object.                                       *
 ******************************************************************************/
- (id) init: (const Vec3 * const) p Vel: (const Vec3 * const) v;
{
    self = [super init];

    /*  Ensure that the object is not nil.                                    */
    if (!self)
        return self;

    /*  Otherwise, create the position component.                             */
    pos = [p copy];

    /*  Same check as before, ensure that pos is not nil.                     */
    if (!pos)
        return self;

    /*  Create the velocity component.                                        */
    vel = [v copy];

    /*  alloc worked for pos. Free pos if alloc didn't work for vel.          */
    if (!vel)
        [pos release];

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
    Vec6 *duplicate = [Vec6 alloc];

    /*  Ensure that the object is not nil.                                    */
    if (!duplicate)
        return duplicate;

    /*  Otherwise, copy the data from self.                                   */
    return [duplicate init: pos Vel: vel];
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
 *      px (Setter)                                                           *
 *  Purpose:                                                                  *
 *      Sets the x component for position in a Vec6 class.                    *
 *  Arguments:                                                                *
 *      pxVal (double):                                                       *
 *          The x component of the position.                                  *
 *  Output:                                                                   *
 *      self (Vec6 *):                                                        *
 *          A pointer to a Vec6 object.                                       *
 ******************************************************************************/
- (id) px: (double) pxVal
{
    [pos x: pxVal];
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      px (Getter)                                                           *
 *  Purpose:                                                                  *
 *      Retrieves the x component for position in a Vec6 class.               *
 *  Arguments:                                                                *
 *      None (void):                                                          *
 *  Output:                                                                   *
 *      px (double):                                                          *
 *          The x component of the position vector.                           *
 ******************************************************************************/
- (double) px
{
    return [pos x];
}

/******************************************************************************
 *  Method:                                                                   *
 *      py (Setter)                                                           *
 *  Purpose:                                                                  *
 *      Sets the y component for position in a Vec6 class.                    *
 *  Arguments:                                                                *
 *      pyVal (double):                                                       *
 *          The y component of the position.                                  *
 *  Output:                                                                   *
 *      self (Vec6 *):                                                        *
 *          A pointer to a Vec6 object.                                       *
 ******************************************************************************/
- (id) py: (double) pyVal
{
    [pos y: pyVal];
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      py (Getter)                                                           *
 *  Purpose:                                                                  *
 *      Retrieves the y component for position in a Vec6 class.               *
 *  Arguments:                                                                *
 *      None (void):                                                          *
 *  Output:                                                                   *
 *      py (double):                                                          *
 *          The y component of the position vector.                           *
 ******************************************************************************/
- (double) py
{
    return [pos y];
}

/******************************************************************************
 *  Method:                                                                   *
 *      pz (Setter)                                                           *
 *  Purpose:                                                                  *
 *      Sets the z component for position in a Vec6 class.                    *
 *  Arguments:                                                                *
 *      pyVal (double):                                                       *
 *          The z component of the position.                                  *
 *  Output:                                                                   *
 *      self (Vec6 *):                                                        *
 *          A pointer to a Vec6 object.                                       *
 ******************************************************************************/
- (id) pz: (double) pzVal
{
    [pos z: pzVal];
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      pz (Getter)                                                           *
 *  Purpose:                                                                  *
 *      Retrieves the z component for position in a Vec6 class.               *
 *  Arguments:                                                                *
 *      None (void):                                                          *
 *  Output:                                                                   *
 *      pz (double):                                                          *
 *          The z component of the position vector.                           *
 ******************************************************************************/
- (double) pz
{
    return [pos z];
}

/******************************************************************************
 *  Method:                                                                   *
 *      vx (Setter)                                                           *
 *  Purpose:                                                                  *
 *      Sets the x component for velocity in a Vec6 class.                    *
 *  Arguments:                                                                *
 *      pxVal (double):                                                       *
 *          The x component of the velocity.                                  *
 *  Output:                                                                   *
 *      self (Vec6 *):                                                        *
 *          A pointer to a Vec6 object.                                       *
 ******************************************************************************/
- (id) vx: (double) vxVal
{
    [vel x: vxVal];
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      vx (Getter)                                                           *
 *  Purpose:                                                                  *
 *      Retrieves the x component for velocity in a Vec6 class.               *
 *  Arguments:                                                                *
 *      None (void):                                                          *
 *  Output:                                                                   *
 *      px (double):                                                          *
 *          The x component of the velocity vector.                           *
 ******************************************************************************/
- (double) vx
{
    return [vel x];
}

/******************************************************************************
 *  Method:                                                                   *
 *      vy (Setter)                                                           *
 *  Purpose:                                                                  *
 *      Sets the y component for velocity in a Vec6 class.                    *
 *  Arguments:                                                                *
 *      vyVal (double):                                                       *
 *          The y component of the velocity.                                  *
 *  Output:                                                                   *
 *      self (Vec6 *):                                                        *
 *          A pointer to a Vec6 object.                                       *
 ******************************************************************************/
- (id) vy: (double) vyVal
{
    [vel y: vyVal];
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      vy (Getter)                                                           *
 *  Purpose:                                                                  *
 *      Retrieves the y component for velocity in a Vec6 class.               *
 *  Arguments:                                                                *
 *      None (void):                                                          *
 *  Output:                                                                   *
 *      vy (double):                                                          *
 *          The y component of the velocity vector.                           *
 ******************************************************************************/
- (double) vy
{
    return [vel y];
}

/******************************************************************************
 *  Method:                                                                   *
 *      vz (Setter)                                                           *
 *  Purpose:                                                                  *
 *      Sets the z component for velocity in a Vec6 class.                    *
 *  Arguments:                                                                *
 *      pyVal (double):                                                       *
 *          The z component of the velocity.                                  *
 *  Output:                                                                   *
 *      self (Vec6 *):                                                        *
 *          A pointer to a Vec6 object.                                       *
 ******************************************************************************/
- (id) vz: (double) vzVal
{
    [vel z: vzVal];
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      vz (Getter)                                                           *
 *  Purpose:                                                                  *
 *      Retrieves the v component for position in a Vec6 class.               *
 *  Arguments:                                                                *
 *      None (void):                                                          *
 *  Output:                                                                   *
 *      vz (double):                                                          *
 *          The z component of the position vector.                           *
 ******************************************************************************/
- (double) vz
{
    return [vel z];
}

/******************************************************************************
 *  Method:                                                                   *
 *      normSq                                                                *
 *  Purpose:                                                                  *
 *      Computes the square of the Euclidean norm of self.                    *
 *  Arguments:                                                                *
 *      None (void).                                                          *
 *  Output:                                                                   *
 *      norm_squared (double):                                                *
 *          The square of the norm of self.                                   *
 ******************************************************************************/
- (double) normSq
{
    /*  By Pythagoras, ||pos + vel||^2 = ||pos||^2 + ||vel||^2.               */
    return [pos normSq] + [vel normSq];
}

/******************************************************************************
 *  Method:                                                                   *
 *      normSq                                                                *
 *  Purpose:                                                                  *
 *      Computes the Euclidean norm of self.                                  *
 *  Arguments:                                                                *
 *      None (void).                                                          *
 *  Output:                                                                   *
 *      norm (double):                                                        *
 *          The norm of self.                                                 *
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
 *      other (const Vec6 * const):                                           *
 *          Another 6D vector.                                                *
 *  Output:                                                                   *
 *      prod (double):                                                        *
 *          The dot product of self and other.                                *
 ******************************************************************************/
- (double) dot: (const Vec6 * const) other
{
    /*  The dot product is linear over the components.                        */
    return [pos dot: other->pos] + [vel dot: other->vel];
}

/******************************************************************************
 *  Method:                                                                   *
 *      plus                                                                  *
 *  Purpose:                                                                  *
 *      Performs vector addition.                                             *
 *  Arguments:                                                                *
 *      other (const Vec6 *):                                                 *
 *          Another vector.                                                   *
 *  Output:                                                                   *
 *      sum (Vec6 *):                                                         *
 *          The vector sum of self and other.                                 *
 ******************************************************************************/
- (id) plus: (const Vec6 * const) other
{
    Vec6 *sum = [Vec6 alloc];

    if (!sum)
        return sum;

    /*  Vector addition is linear over components. Add the position vectors.  */
    sum->pos = [pos plus: other->pos];

    /*  Chech that pos was successfully created.                              */
    if (!sum->pos)
        return sum;

    /*  Otherwise, sum the velocity vectors together as well.                 */
    sum->vel = [vel plus: other->vel];
    return sum;
}

/******************************************************************************
 *  Method:                                                                   *
 *      plusEqual                                                             *
 *  Purpose:                                                                  *
 *      Performs vector addition in-place.                                    *
 *  Arguments:                                                                *
 *      other (const Vec6 * const):                                           *
 *          Another vector.                                                   *
 *  Output:                                                                   *
 *      self (Vec6 *):                                                        *
 *          A pointer to the object calling this method.                      *
 ******************************************************************************/
- (id) plusEqual: (const Vec6 * const) other
{
    /*  Use in-place addition on the individual components and return.        */
    [pos plusEqual: other->pos];
    [vel plusEqual: other->vel];
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      minus                                                                 *
 *  Purpose:                                                                  *
 *      Performs vector subtraction.                                          *
 *  Arguments:                                                                *
 *      other (const Vec6 *):                                                 *
 *          Another vector.                                                   *
 *  Output:                                                                   *
 *      diff (Vec6 *):                                                        *
 *          The vector difference of self and other.                          *
 ******************************************************************************/
- (id) minus: (const Vec6 * const) other
{
    Vec6 *diff = [Vec6 alloc];

    if (!diff)
        return diff;

    /*  Vector subtraction is linear over components. Subtract the position.  */
    diff->pos = [pos plus: other->pos];

    /*  Chech that pos was successfully created.                              */
    if (!diff->pos)
        return diff;

    /*  Otherwise, subtract the velocity vectors together as well.            */
    diff->vel = [vel plus: other->vel];
    return diff;
}

/******************************************************************************
 *  Method:                                                                   *
 *      minusEqual                                                            *
 *  Purpose:                                                                  *
 *      Performs vector subtraction in-place.                                 *
 *  Arguments:                                                                *
 *      other (const Vec6 * const):                                           *
 *          Another vector.                                                   *
 *  Output:                                                                   *
 *      self (Vec6 *):                                                        *
 *          A pointer to the object calling this method.                      *
 ******************************************************************************/
- (id) minusEqual: (const Vec6 * const) other
{
    /*  Use in-place addition on the individual components and return.        */
    [pos minusEqual: other->pos];
    [vel minusEqual: other->vel];
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
 *      prod (Vec6 *):                                                        *
 *          The scalar product of scale and self.                             *
 ******************************************************************************/
- (id) times: (double) scale
{
    Vec6 *prod = [Vec6 alloc];

    if (!prod)
        return prod;

    /*  Scalar multiplication is linear over components.                      */
    prod->pos = [pos times: scale];

    /*  Chech that pos was successfully created.                              */
    if (!prod->pos)
        return prod;

    /*  Otherwise, subtract the velocity vectors together as well.            */
    prod->vel = [vel times: scale];
    return prod;
}

/******************************************************************************
 *  Method:                                                                   *
 *      timesEqual                                                            *
 *  Purpose:                                                                  *
 *      Performs vector multiplication in-place.                              *
 *  Arguments:                                                                *
 *      scale (double):                                                       *
 *          The scalar multiplier.                                            *
 *  Output:                                                                   *
 *      self (Vec6 *):                                                        *
 *          A pointer to the object calling this method.                      *
 ******************************************************************************/
- (id) timesEqual: (double) scale
{
    /*  Use in-place multiplication on the individual components and return.  */
    [pos timesEqual: scale];
    [vel timesEqual: scale];
    return self;
}

@end
