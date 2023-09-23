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
 *      Provides a struct for working with PPM files.                         *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/23                                                        *
 ******************************************************************************/
package main

import (
    "fmt"   /*  Fprintf found here. */
    "log"   /*  log.Fatal function. */
    "os"    /*  File data type.     */
)

/*  Struct for creating and writing to ppm files.                             */
type PPM struct {

    /*  The data in a PPM struct is the File pointer it represents.           */
    Fp *os.File
}

/******************************************************************************
 *                         PPM Functions and Methods                          *
 ******************************************************************************/

/******************************************************************************
 *  Function:                                                                 *
 *      PPM.Create                                                            *
 *  Purpose:                                                                  *
 *      Creates a PPM file with a given file name.                            *
 *  Arguments:                                                                *
 *      name (const char *):                                                  *
 *          The file name of the output PPM (ex. "black_hole.ppm").           *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 ******************************************************************************/
func (ppm *PPM) Create(name string) {

    /*  Variable for the error message if os.Create fails.                    */
    var err error

    /*  Open the file and give it write permissions.                          */
    ppm.Fp, err = os.Create(name)

    if (err != nil) {
        log.Fatal(err)
    }
}
/*  End of PPM.Create.                                                        */

/******************************************************************************
 *  Function:                                                                 *
 *      PPM.InitFromVals                                                      *
 *  Purpose:                                                                  *
 *      Print the preamble to the PPM file. A PPM file wants Pn followed by   *
 *      three numbers. P6 means we're encoding an RGB image in binary format. *
 *      The first two numbers are the number of pixels in the x and y axes.   *
 *      The last number is the size of our color spectrum, which is 255.      *
 *  Arguments:                                                                *
 *      x (uint32):                                                           *
 *          The number of pixels in the x axis.                               *
 *      y (uint32):                                                           *
 *          The number of pixels in the y axis.                               *
 *      type (int):                                                           *
 *          The type of the PPM, options are 1 through 6.                     *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 ******************************************************************************/
func (ppm *PPM) InitFromVals(x, y uint32, ptype int) {

    /*  For integers between 1 and 5 we can pass the value to the preamble.   */
    if (0 < ptype && ptype < 6) {
        fmt.Fprintf(ppm.Fp, "P%d\n%d %d\n255\n", ptype, x, y)

    /*  The only other legal value is 6. All illegal values default to 6.     */
    } else {
        fmt.Fprintf(ppm.Fp, "P6\n%d %d\n255\n", x, y)
    }
}
/*  End of PPM.InitFromVals.                                                  */

/******************************************************************************
 *  Function:                                                                 *
 *      PPM.Init                                                              *
 *  Purpose:                                                                  *
 *      Initialize a PPM using the default values.                            *
 *  Arguments:                                                                *
 *      None.                                                                 *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 *  Method:                                                                   *
 *      Pass the default parameters to PPM.InitFromVals.                      *
 ******************************************************************************/
func (ppm *PPM) Init() {
    ppm.InitFromVals(X_Size, Y_Size, 6)
}
/*  End of PPM.Init.                                                          */

/******************************************************************************
 *  Function:                                                                 *
 *      PPM.Close                                                             *
 *  Purpose:                                                                  *
 *      Closes the file pointer in a PPM struct.                              *
 *  Arguments:                                                                *
 *      None.                                                                 *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 ******************************************************************************/
func (ppm *PPM) Close() {

    /*  Ensure the pointer is not nil before trying to close it.              */
    if (ppm.Fp != nil) {
        ppm.Fp.Close()
    }
}
/*  End of PPM.Close.                                                         */
