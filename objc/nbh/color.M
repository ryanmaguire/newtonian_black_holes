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

/*  C standard library providing printf, FILE, and puts.                      */
#include <stdio.h>

/*  PPM struct provided here.                                                 */
#import "ppm.M"

/*  Class for working with colors in a PPM file.                              */
@interface Color:NSObject
{
    @private
        unsigned char red, green, blue;
}
    - (id) init;
    - (id) init:
        (unsigned char) r Green: (unsigned char) g Blue: (unsigned char) b;
    - (void) writeToPPM: (PPM *) ppm;
    - (void) writeToFile: (FILE *) fp;
    - (id) times: (double) t;
    - (id) timesEqual: (double) t;
@end

@implementation Color

- (id) init
{
    [super init];
    red = 0x00U;
    green = 0x00U;
    blue = 0x00U;
    return self;
}

- (id) init: (unsigned char) r Green: (unsigned char) g Blue: (unsigned char) b
{
    [super init];
    red = r;
    green = g;
    blue = b;
    return self;
}

- (void) writeToFile: (FILE *) fp
{
    fputc((int)red, fp);
    fputc((int)green, fp);
    fputc((int)blue, fp);
}

- (void) writeToPPM: (PPM *) ppm
{
    [self writeToFile: [ppm file]];
}

- (id) times: (double) t
{
    const unsigned char r = (unsigned char)(t * (double)red);
    const unsigned char g = (unsigned char)(t * (double)green);
    const unsigned char b = (unsigned char)(t * (double)blue);
    Color *prod = [Color alloc];

    if (!prod)
        return prod;

    return [prod init: r Green: g Blue: b];
}

- (id) timesEqual: (double) t
{
    red = (unsigned char)(t * (double)red);
    green = (unsigned char)(t * (double)green);
    blue = (unsigned char)(t * (double)blue);
    return self;
}

@end
