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

#import "setup.M"

@interface PPM:NSObject
{
    @private
        FILE *fp;
}
    - (id) init: (const char *) filename;
    - (void) create: (unsigned int) x Height: (unsigned int) y Type: (int) type;
    - (void) create;
    - (void) close;
@end

@implementation PPM

- (id) init: (const char *) filename
{
    [super init];
    fp = fopen(filename, "w");
    return self;
}

- (void) create: (unsigned int) x Height: (unsigned int) y Type: (int) type
{
    int typenum = 6;

    if (0 < type && type < 6)
        typenum = type;

    fprintf(fp, "P%d\n%u %u\n255\n", typenum, x, y);
}

- (void) create
{
    [self create: xSize Height: ySize type: 6];
}

- (void) close
{
    fclose(fp);
}

@end
