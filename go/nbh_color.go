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
 *      Provides a struct for working with colors in RGB 24-bit format.       *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/23                                                        *
 ******************************************************************************/
package main

import (
    "math"  /*  Ceil function found here.   */
    "os"    /*  File data type found here.  */
)

/*  Struct for working with colors in RGB format.                             */
type Color struct {

    /*  A color is completely defined by its red, green, and blue values.     */
    Red, Green, Blue uint8
}

/******************************************************************************
 *                        Color Functions and Methods                         *
 ******************************************************************************/

/******************************************************************************
 *  Function:                                                                 *
 *      Color.WriteToFile                                                     *
 *  Purpose:                                                                  *
 *      Writes a color to a File pointer.                                     *
 *  Arguments:                                                                *
 *      fp (*os.File):                                                        *
 *          A pointer to the file the color is being written to.              *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 ******************************************************************************/
func (c *Color) WriteToFile(fp *os.File) {
    rgb := []byte{c.Red, c.Green, c.Blue}
    fp.Write(rgb)
}
/*  End of Color.WriteToFile.                                                 */

/******************************************************************************
 *  Function:                                                                 *
 *      Color.WriteToPPM                                                      *
 *  Purpose:                                                                  *
 *      Writes a color to a PPM.                                              *
 *  Arguments:                                                                *
 *      fp (*os.File):                                                        *
 *          A pointer to the file the color is being written to.              *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 ******************************************************************************/
func (c *Color) WriteToPPM(ppm *PPM) {
    c.WriteToFile(ppm.Fp)
}
/*  End of Color.WriteToPPM.                                                  */

/******************************************************************************
 *  Function:                                                                 *
 *      ColorScale                                                            *
 *  Purpose:                                                                  *
 *      Scales a color by a real number. Used for darkening a color.          *
 *  Arguments:                                                                *
 *      c (*Color):                                                           *
 *          A pointer to a color.                                             *
 *      t (float64):                                                          *
 *          The scale factor, usually between 0 and 1.                        *
 *  Outputs:                                                                  *
 *      scaled_c (Color):                                                     *
 *          The input color c with RGB components scaled by t.                *
 ******************************************************************************/
func ColorScale(c *Color, t float64) Color {
    var r uint8 = uint8(t * float64(c.Red))
    var g uint8 = uint8(t * float64(c.Green))
    var b uint8 = uint8(t * float64(c.Blue))
    return Color{r, g, b}
}
/*  End of ColorScale.                                                        */

/******************************************************************************
 *  Function:                                                                 *
 *      ColorScaleBy                                                          *
 *  Purpose:                                                                  *
 *      Scales a color by a real number. Used for darkening a color.          *
 *  Arguments:                                                                *
 *      c (*Color):                                                           *
 *          A pointer to a color.                                             *
 *      t (float64):                                                          *
 *          The scale factor, usually between 0 and 1.                        *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 ******************************************************************************/
func ColorScaleBy(c *Color, t float64) {
    c.Red = uint8(t * float64(c.Red))
    c.Green = uint8(t * float64(c.Green))
    c.Blue = uint8(t * float64(c.Blue))
}
/*  End of ColorScaleBy.                                                      */

/******************************************************************************
 *  Function:                                                                 *
 *      Black                                                                 *
 *  Purpose:                                                                  *
 *      Returns the color black in RGB format.                                *
 *  Arguments:                                                                *
 *      None.                                                                 *
 *  Outputs:                                                                  *
 *      black (Color):                                                        *
 *          The color black in RGB.                                           *
 *  Method:                                                                   *
 *      Set each of the RGB components to zero and return.                    *
 ******************************************************************************/
func Black() Color {
    return Color{0x00, 0x00, 0x00}
}

/******************************************************************************
 *  Function:                                                                 *
 *      Red                                                                   *
 *  Purpose:                                                                  *
 *      Returns the color red in RGB format.                                  *
 *  Arguments:                                                                *
 *      t (float64):                                                          *
 *          The intensity of the color, should be between 0 and 1.            *
 *  Outputs:                                                                  *
 *      red (Color):                                                          *
 *          The color red in RGB.                                             *
 *  Method:                                                                   *
 *      Set the green and blue components to zero, and the red component to t.*
 ******************************************************************************/
func Red(t float64) Color {

    /*  8-bit colors have a max intensity of 255. Scale this by t.            */
    var val uint8 = uint8(t * 255.0)
    return Color{val, 0x00, 0x00}
}

/******************************************************************************
 *  Function:                                                                 *
 *      Green                                                                 *
 *  Purpose:                                                                  *
 *      Returns the color green in RGB format.                                *
 *  Arguments:                                                                *
 *      t (float64):                                                          *
 *          The intensity of the color, should be between 0 and 1.            *
 *  Outputs:                                                                  *
 *      green (Color):                                                        *
 *          The color green in RGB.                                           *
 *  Method:                                                                   *
 *      Set the red and blue components to zero, and the green component to t.*
 ******************************************************************************/
func Green(t float64) Color {

    /*  8-bit colors have a max intensity of 255. Scale this by t.            */
    var val uint8 = uint8(t * 255.0)
    return Color{0x00, val, 0x00}
}

/******************************************************************************
 *  Function:                                                                 *
 *      Blue                                                                  *
 *  Purpose:                                                                  *
 *      Returns the color blue in RGB format.                                 *
 *  Arguments:                                                                *
 *      t (float64):                                                          *
 *          The intensity of the color, should be between 0 and 1.            *
 *  Outputs:                                                                  *
 *      blue (Color):                                                         *
 *          The color blue in RGB.                                            *
 *  Method:                                                                   *
 *      Set the red and green components to zero, and the blue component to t.*
 ******************************************************************************/
func Blue(t float64) Color {

    /*  8-bit colors have a max intensity of 255. Scale this by t.            */
    var val uint8 = uint8(t * 255.0)
    return Color{0x00, 0x00, val}
}

/******************************************************************************
 *  Function:                                                                 *
 *      Yellow                                                                *
 *  Purpose:                                                                  *
 *      Returns the color yellow in RGB format.                               *
 *  Arguments:                                                                *
 *      t (float64):                                                          *
 *          The intensity of the color, should be between 0 and 1.            *
 *  Outputs:                                                                  *
 *      yellow (Color):                                                       *
 *          The color yellow in RGB.                                          *
 *  Method:                                                                   *
 *      Set the red and green components to t, and the blue component to zero.*
 ******************************************************************************/
func Yellow(t float64) Color {

    /*  8-bit colors have a max intensity of 255. Scale this by t.            */
    var val uint8 = uint8(t * 255.0)
    return Color{val, val, 0x00}
}

/******************************************************************************
 *  Function:                                                                 *
 *      White                                                                 *
 *  Purpose:                                                                  *
 *      Returns the color white in RGB format.                                *
 *  Arguments:                                                                *
 *      t (float64):                                                          *
 *          The intensity of the color, should be between 0 and 1.            *
 *  Outputs:                                                                  *
 *      white (Color):                                                        *
 *          The color white in RGB.                                           *
 *  Method:                                                                   *
 *      Set the red, green, and blue components to t.                         *
 ******************************************************************************/
func White(t float64) Color {
    var val uint8 = uint8(t * 255.0)
    return Color{val, val, val}
}

/******************************************************************************
 *  Function:                                                                 *
 *      ColorAdd                                                              *
 *  Purpose:                                                                  *
 *      Adds two colors by averaging over their components.                   *
 *  Arguments:                                                                *
 *      c0 (*Color):                                                          *
 *          A pointer to the first color.                                     *
 *      c1 (*Color):                                                          *
 *          A pointer to the second color.                                    *
 *  Outputs:                                                                  *
 *      sum (Color):                                                          *
 *          The sum of the two colors.                                        *
 *  Method:                                                                   *
 *      Convert the components to float64 and take their average.             *
 ******************************************************************************/
func ColorAdd(c0, c1 *Color) Color {

    /*  Declare a variable for the output.                                    */
    var sum Color

    /*  Avoid overflow by converting to doubles and then taking the average.  */
    var r float64 = 0.5*(float64(c0.Red) + float64(c1.Red))
    var g float64 = 0.5*(float64(c0.Green) + float64(c1.Green))
    var b float64 = 0.5*(float64(c0.Blue) + float64(c1.Blue))

    /*  Convert back to unsigned char's and store these in the color.         */
    sum.Red = uint8(r)
    sum.Green = uint8(g)
    sum.Blue = uint8(b)
    return sum
}
/*  End of ColorAdd.                                                          */

/******************************************************************************
 *  Function:                                                                 *
 *      ColorAddTo                                                            *
 *  Purpose:                                                                  *
 *      Adds two colors by averaging over their components.                   *
 *  Arguments:                                                                *
 *      c0 (*Color):                                                          *
 *          A pointer to the first color.                                     *
 *      c1 (*Color):                                                          *
 *          A pointer to the second color.                                    *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 *  Method:                                                                   *
 *      Convert the components to float64 and take their average.             *
 ******************************************************************************/
func ColorAddTo(c0, c1 *Color) {

    /*  Avoid overflow by converting to doubles and then taking the average.  */
    var r float64 = 0.5*(float64(c0.Red) + float64(c1.Red))
    var g float64 = 0.5*(float64(c0.Green) + float64(c1.Green))
    var b float64 = 0.5*(float64(c0.Blue) + float64(c1.Blue))

    /*  Convert back to unsigned char's and store these in the color.         */
    c0.Red = uint8(r)
    c0.Green = uint8(g)
    c0.Blue = uint8(b)
}
/*  End of ColorAddTo.                                                        */

/******************************************************************************
 *  Function:                                                                 *
 *      CheckerBoard                                                          *
 *  Purpose:                                                                  *
 *      Creates a checker-board pattern on the detector.                      *
 *  Arguments:                                                                *
 *      u (*Vec6):                                                            *
 *          The position and velocity of the particle as it hit the detector. *
 *  Outputs:                                                                  *
 *      c (Color):                                                            *
 *          The color given on the detector.                                  *
 ******************************************************************************/
func CheckerBoard(u *Vec6) Color {

    /*  Factor for darkening the checker board.                               */
    var cfact float64 = Z_Detector_Sq / u.P.NormSq()

    /*  If the photon didn't make it, color the pixel black.                  */
    if (u.P.Z > Z_Detector) {
        return Black()

    /*  Otherwise use a bit-wise trick to color the plane.                    */
    } else if (uint32(math.Ceil(u.P.X) + math.Ceil(u.P.Y)) & 1 == 1) {
        return White(cfact)
    } else {
        return Red(cfact)
    }
}
/*  End of CheckerBoard.                                                      */

/******************************************************************************
 *  Function:                                                                 *
 *      CheckerBoardHighlight                                                 *
 *  Purpose:                                                                  *
 *      Creates a checker-board pattern and highlights the origin.            *
 *  Arguments:                                                                *
 *      u (*Vec6):                                                            *
 *          The position and velocity of the particle as it hit the detector. *
 *  Outputs:                                                                  *
 *      c (Color):                                                            *
 *          The color given on the detector.                                  *
 ******************************************************************************/
func CheckerBoardHighlight(u *Vec6) Color {

    /*  Factor for darkening the checker board.                               */
    var cfact float64 = Z_Detector_Sq / u.P.NormSq()

    /*  If the photon didn't make it, color the pixel black.                  */
    if (u.P.Z > Z_Detector) {
        return Black()

    /*  If the point is close to the origin, highlight it blue.               */
    } else if (u.P.RhoSq() < Highlight_Threshold) {
        return Blue(1.0)

    /*  Otherwise use a bit-wise trick to color the plane.                    */
    } else if (uint32(math.Ceil(u.P.X) + math.Ceil(u.P.Y)) & 1 == 1) {
        return White(cfact)
    } else {
        return Red(cfact)
    }
}
/*  End of CheckerBoardHighlight.                                             */

/******************************************************************************
 *  Function:                                                                 *
 *      BrightCheckerBoard                                                    *
 *  Purpose:                                                                  *
 *      Creates a brighter checker-board pattern on the detector.             *
 *  Arguments:                                                                *
 *      u (*Vec6):                                                            *
 *          The position and velocity of the particle as it hit the detector. *
 *  Outputs:                                                                  *
 *      c (Color):                                                            *
 *          The color given on the detector.                                  *
 ******************************************************************************/
func BrightCheckerBoard(u *Vec6) Color {

    /*  Factor for darkening the checker board.                               */
    var cfact float64 = 0.5*(Z_Detector_Sq / u.P.NormSq() + 1.0)

    /*  If the photon didn't make it, color the pixel black.                  */
    if (u.P.Z > Z_Detector) {
        return Black()

    /*  Otherwise use a bit-wise trick to color the plane.                    */
    } else if (uint32(math.Ceil(u.P.X) + math.Ceil(u.P.Y)) & 1 == 1) {
        return White(cfact)
    } else {
        return Red(cfact)
    }
}
/*  End of BrightCheckerBoard.                                                */

/******************************************************************************
 *  Function:                                                                 *
 *      CheckerBoardFour                                                      *
 *  Purpose:                                                                  *
 *      Creates a checker-board pattern on the detector with four colors.     *
 *  Arguments:                                                                *
 *      u (*Vec6):                                                            *
 *          The position and velocity of the particle as it hit the detector. *
 *  Outputs:                                                                  *
 *      c (Color):                                                            *
 *          The color given on the detector.                                  *
 ******************************************************************************/
func CheckerBoardFour(u *Vec6) Color {

    /*  Factor for darkening the checker board.                               */
    var cfact float64 = Z_Detector_Sq / u.P.NormSq()

    /*  Integers that determine the color.                                    */
    var nx uint32 = uint32(math.Ceil(u.P.X)) & 1
    var ny uint32 = uint32(math.Ceil(u.P.Y)) & 1
    var n uint32 = nx + (ny << 1)

    /*  If the photon didn't make it, color the pixel black.                  */
    if (u.P.Z > Z_Detector) {
        return Black()
    }

    /*  Otherwise use a bit-wise trick to color the plane.                    */
    switch (n) {
        case 0:
            return White(cfact)
        case 1:
            return Yellow(cfact)
        case 2:
            return Green(cfact)
        default:
            return Red(cfact)
    }
}
/*  End of CheckerBoardFour.                                                  */

/******************************************************************************
 *  Function:                                                                 *
 *      CheckerBoardFourHighlight                                             *
 *  Purpose:                                                                  *
 *      Creates a four-colored checker-board and highlights the origin.       *
 *  Arguments:                                                                *
 *      u (*Vec6):                                                            *
 *          The position and velocity of the particle as it hit the detector. *
 *  Outputs:                                                                  *
 *      c (Color):                                                            *
 *          The color given on the detector.                                  *
 ******************************************************************************/
func CheckerBoardFourHighlight(u *Vec6) Color {

    /*  Factor for darkening the checker board.                               */
    var cfact float64 = Z_Detector_Sq / u.P.NormSq()

    /*  Integers that determine the color.                                    */
    var nx uint32 = uint32(math.Ceil(u.P.X)) & 1
    var ny uint32 = uint32(math.Ceil(u.P.Y)) & 1
    var n uint32 = nx + (ny << 1)

    /*  If the photon didn't make it, color the pixel black.                  */
    if (u.P.Z > Z_Detector) {
        return Black()

    /*  If the point is close to the origin, highlight it blue.               */
    } else if (u.P.RhoSq() < Highlight_Threshold) {
        return Blue(1.0)
    }

    /*  Otherwise use a bit-wise trick to color the plane.                    */
    switch (n) {
        case 0:
            return White(cfact)
        case 1:
            return Yellow(cfact)
        case 2:
            return Green(cfact)
        default:
            return Red(cfact)
    }
}
/*  End of CheckerBoardFourHighlight.                                         */

/******************************************************************************
 *  Function:                                                                 *
 *      AngleGradientColor                                                    *
 *  Purpose:                                                                  *
 *      Creates a rainbow gradient of color based on the angle the velocity   *
 *      vector of the particle makes with the detector on impact.             *
 *  Arguments:                                                                *
 *      u (*Vec6):                                                            *
 *          The position and velocity of the particle as it hit the detector. *
 *  Outputs:                                                                  *
 *      c (Color):                                                            *
 *          The color given on the detector.                                  *
 ******************************************************************************/
func AngleGradientColor(u *Vec6) Color {

    /*  Declare unsigned char's for computing the output color.               */
    var red, green, blue uint8

    /*  We want the zenith angle of the velocity vector. This can be          *
     *  computed using the cylindrical coordinates of the vector.             */
    var angle float64 = math.Atan2(math.Abs(u.V.Z), u.V.RhoSq())

    /*  Scale the angle so that it falls between 0 and 255.                   */
    var scaled float64 = 255.0 * angle / (0.5*math.Pi)

    /*  Use an RGB rainbow gradient to color the current pixel. We'll set     *
     *  blue to correspond to the least value and red for the greatest,       *
     *  with a continuous gradient in between. First blue to cyan.            */
    if (scaled < 64.0) {
        red = 0x00
        green = uint8(4.0*scaled)
        blue = 0xFF

    /*  Next, cyan to green.                                                  */
    } else if (scaled < 128.0) {
        red = 0x00
        green = 0xFF
        blue = uint8(255.0 - 4.0*(scaled - 64.0))

    /*  Green to yellow.                                                      */
    } else if (scaled < 192.0) {
        red = uint8(4.0*(scaled - 128.0))
        green = 0xFF
        blue = 0x00

    /*  Yellow to red.                                                        */
    } else if (scaled < 255.0) {
        red = 0xFF
        green = uint8(255.0 - 4.0*(scaled - 192.0))
        blue = 0x00

    /*  And lastly, red.                                                      */
    } else {
        red = 0xFF
        green = 0x00
        blue = 0x00
    }

    return Color{red, green, blue}
}
/*  End of AngleGradientColor.                                                */

/******************************************************************************
 *  Function:                                                                 *
 *      ColorGradientCheckerBoard                                             *
 *  Purpose:                                                                  *
 *      Creates a checker-board pattern on the detector and adds the rainbow  *
 *      gradient defined in AngleGradientColor.                               *
 *  Arguments:                                                                *
 *      u (*Vec6):                                                            *
 *          The position and velocity of the particle as it hit the detector. *
 *  Outputs:                                                                  *
 *      c (Color):                                                            *
 *          The color given on the detector.                                  *
 ******************************************************************************/
func ColorGradientCheckerBoard(u *Vec6) Color {

    /*  Declare necessary variables.                                          */
    var rainbow, checker_board Color

    /*  If the photon didn't make it, color the pixel black.                  */
    if (u.P.Z > Z_Detector) {
        return Black()
    }

    /*  Take the average of the checkerboard and the raindbow gradient.       */
    rainbow = AngleGradientColor(u)
    checker_board = BrightCheckerBoard(u)
    return ColorAdd(&checker_board, &rainbow)
}
/*  End of ColorGradientCheckerBoard.                                         */
