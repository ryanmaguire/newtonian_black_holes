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
#import <Foundation/Foundation.h>
#include <math.h>
#include <stdio.h>

@interface Vec3:NSObject
{
    @private
        double dat[3];
}

    /*  Create a Vec3 object from three doubles.                              */
    - (id) init: (double)xVal Y: (double)yVal Z: (double)zVal;

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
    - (double) dot: (Vec3 *) other;
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
/*  End of init.                                                              */

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
 *      other (Vec3 *):                                                       *
 *          Another Vec3 class.                                               *
 *  Output:                                                                   *
 *      dot_product (double):                                                 *
 *          The Euclidean dot product of self and other.                      *
 ******************************************************************************/
- (double) dot: (Vec3 *) other
{
    const double dx = [self x] * [other x];
    const double dy = [self y] * [other y];
    const double dz = [self z] * [other z];
    return dx + dy + dz;
}

@end
