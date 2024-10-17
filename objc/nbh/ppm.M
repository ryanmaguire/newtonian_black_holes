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

/*  Default parameters for the black hole and camera found here.              */
#import "setup.M"

/*  Object for creating PPM files and writing colors to them.                 */
@interface PPM:NSObject
{
    /*  The data is just a file pointer. It points to the ".ppm" file.        */
    @private
        FILE *fp;
}
    /*  Create a PPM object with a given filename. It should end with .ppm.   */
    - (id) init: (const char *) filename;

    /*  Create a PPM file with a given width, height, and type.               */
    - (void) create: (unsigned int) x Height: (unsigned int) y Type: (int) type;

    /*  Create a PPM file with the default width, height, and type.           */
    - (void) create;

    /*  Getter for the file pointer.                                          */
    - (FILE *) file;

    /*  Close the PPM file (closes the file points the class contains).       */
    - (void) close;
@end

/*  Implementation for the basic PPM struct. Similar to the C++ version.      */
@implementation PPM

/******************************************************************************
 *  Method:                                                                   *
 *      init                                                                  *
 *  Purpose:                                                                  *
 *      Initializes a PPM file with a given file name.                        *
 *  Arguments:                                                                *
 *      filename (const char *):                                              *
 *          The name of the PPM file.                                         *
 *  Output:                                                                   *
 *      self (id):                                                            *
 *          A pointer to the PPM class that has been created.                 *
 ******************************************************************************/
- (id) init: (const char *) filename
{
    [super init];
    fp = fopen(filename, "w");
    return self;
}

/******************************************************************************
 *  Method:                                                                   *
 *      create                                                                *
 *  Purpose:                                                                  *
 *      Writes the starting parameters for the PPM file to the file.          *
 *  Arguments:                                                                *
 *      x (unsigned int):                                                     *
 *          The number of pixels in the x axis.                               *
 *      y (unsigned int):                                                     *
 *          The number of pixels in the y axis.                               *
 *      type (int):                                                           *
 *          The type of PPM file. This is a number 1 <= type <= 6.            *
 *  Output:                                                                   *
 *      None (void).                                                          *
 ******************************************************************************/
- (void) create: (unsigned int) x Height: (unsigned int) y Type: (int) type
{
    /*  Default type is 6, which is binary-based PPM with RGB colors.         */
    int typenum = 6;

    /*  If the user wanted a different style, result the typenum.             */
    if (0 < type && type < 6)
        typenum = type;

    /*  Preamble is Pn, width height, color-depth, where n is the typenum.    *
     *  We use 8-bit color channels, which produce 24-bit RGB colors. The     *
     *  depth is then 2^8 - 1 = 255.                                          */
    fprintf(fp, "P%d\n%u %u\n255\n", typenum, x, y);
}

/******************************************************************************
 *  Method:                                                                   *
 *      file                                                                  *
 *  Purpose:                                                                  *
 *      Getter for the file pointer.                                          *
 *  Arguments:                                                                *
 *      None (void).                                                          *
 *  Output:                                                                   *
 *      fp (FILE *):                                                          *
 *          The file pointer inside the PPM class.                            *
 ******************************************************************************/
- (FILE *) file
{
    return fp;
}

/******************************************************************************
 *  Method:                                                                   *
 *      create                                                                *
 *  Purpose:                                                                  *
 *      Writes the default parameters for the PPM file to the file.           *
 *  Arguments:                                                                *
 *      None (void).                                                          *
 *  Output:                                                                   *
 *      None (void).                                                          *
 ******************************************************************************/
- (void) create
{
    [self create: xSize Height: ySize type: 6];
}

/******************************************************************************
 *  Method:                                                                   *
 *      close                                                                 *
 *  Purpose:                                                                  *
 *      Close the PPM file.                                                   *
 *  Arguments:                                                                *
 *      None (void).                                                          *
 *  Output:                                                                   *
 *      None (void).                                                          *
 ******************************************************************************/
- (void) close
{
    fclose(fp);
}

@end
