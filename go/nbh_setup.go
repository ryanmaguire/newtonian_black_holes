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
 *      Provides the default parameters for the black holes.                  *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/23                                                        *
 ******************************************************************************/
package main

/******************************************************************************
 *                              Setup Parameters                              *
 ******************************************************************************/

/*  The z value for the source.                                               */
const Z_Src float64 = 10.0

/*  The start and end parameters for the "source". The source is the          *
 *  square [start, end] x [start, end] at height z_src.                       */
const Start float64 = -10.0
const End float64 = 10.0

/*  The height of the detector plane.                                         */
const Z_Detector float64 = -10.0

/*  The square of the height of the detector, used frequently.                */
const Z_Detector_Sq float64 = 100.0

/*  The radius of the black hole.                                             */
var Black_Hole_Radius float64 = 1.0

/*  The square of the black hole radius, also used frequently.                */
var Black_Hole_Radius_Sq float64 = 1.0

/*  Number of pixels in the x and y axes.                                     */
const X_Size uint32 = 1024
const Y_Size uint32 = 1024

/*  Factor used for converting from pixels to points in space. These have the *
 *  values (setup_end - setup_start) / (size - 1), where size is the number   *
 *  of pixels in the x and y axes, respectively. If you change X_Size or      *
 *  Y_Size, reset these numbers too.                                          */
const PX_Factor float64 = 0.019550342130987292
const PY_Factor float64 = 0.019550342130987292

/*  Threshold for highlighting features (usually the origin).                 */
const Highlight_Threshold float64 = 0.02

/*  For two black holes, the centers lie on the x axis.                       */
const BHX1 float64 = -3.0
const BHX2 float64 = +3.0

/******************************************************************************
 *                              Setup Functions                               *
 ******************************************************************************/

/******************************************************************************
 *  Function:                                                                 *
 *      ResetRadius                                                           *
 *  Purpose:                                                                  *
 *      Resets the radius of the black hole.                                  *
 *  Arguments:                                                                *
 *      r (double):                                                           *
 *          The new radius.                                                   *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 ******************************************************************************/
func ResetRadius(r float64) {
    Black_Hole_Radius = r
    Black_Hole_Radius_Sq = r*r
}
/*  End of ResetRadius.                                                       */

/******************************************************************************
 *  Function:                                                                 *
 *      PixelToPoint                                                          *
 *  Purpose:                                                                  *
 *      Converts a pixel (x, y) on the PPM to a point in space.               *
 *  Arguments:                                                                *
 *      x (uint32):                                                           *
 *          The x coordinate of the pixel.                                    *
 *      y (uint32):                                                           *
 *          The y coordinate of the pixel.                                    *
 *  Outputs:                                                                  *
 *      p (Vec3):                                                             *
 *          The corresponding point in space to the given pixel.              *
 *  Method:                                                                   *
 *      The point on the detector lies on the z = Z_Src plane. Use this and   *
 *      convert the (x, y) components of the pixel to the (x, y) components   *
 *      on the detector.                                                      *
 ******************************************************************************/
func PixelToPoint(x, y uint32) Vec3 {
    var xpt float64 = Start + PX_Factor*float64(x)
    var ypt float64 = Start + PY_Factor*float64(y)
    return Vec3{xpt, ypt, Z_Src}
}
/*  End of PixelToPoint.                                                      */

/******************************************************************************
 *  Function:                                                                 *
 *      Stop                                                                  *
 *  Purpose:                                                                  *
 *      Determines if a photon is still in motion for one black hole.         *
 *  Arguments:                                                                *
 *      v (*Vec3):                                                            *
 *          The vector corresponding to the given photon.                     *
 *  Outputs:                                                                  *
 *      stop (bool):                                                          *
 *          Boolean for if the photon is still moving.                        *
 ******************************************************************************/
func Stop(v *Vec3) bool {

    /*  Case 1: The photon has reached the detector.                          */
    if (v.Z < Z_Detector) {
        return true

    /*  Case 2: The black hole swallowed the photon.                          */
    } else if (v.NormSq() < Black_Hole_Radius_Sq) {
        return true

    /*  Otherwise, the photon is still moving. Don't stop.                    */
    } else {
        return false
    }
}
/*  End of Stop.                                                              */

/******************************************************************************
 *  Function:                                                                 *
 *      Stop2                                                                 *
 *  Purpose:                                                                  *
 *      Determines if a photon is still in motion for two black holes.        *
 *  Arguments:                                                                *
 *      v (*Vec3):                                                            *
 *          The vector corresponding to the given photon.                     *
 *  Outputs:                                                                  *
 *      stop (bool):                                                          *
 *          Boolean for if the photon is still moving.                        *
 ******************************************************************************/
func Stop2(p *Vec3) bool {

    /*  The black holes lie on the x axis. Compute the displacements to p.    */
    var r1 Vec3 = Vec3{p.X - BHX1, p.Y, p.Z}
    var r2 Vec3 = Vec3{p.X - BHX2, p.Y, p.Z}

    /*  Case 1: The photon has reached the detector.                          */
    if (p.Z < Z_Detector) {
        return true

    /*  Case 2: The first black hole swallowed the photon.                    */
    } else if (r1.NormSq() < Black_Hole_Radius_Sq) {
        return true

    /*  Case 3: The second black hole swallowed the photon.                   */
    } else if (r2.NormSq() < Black_Hole_Radius_Sq) {
        return true

    /*  Otherwise, the photon is still moving. Don't stop.                    */
    } else {
        return false
    }
}
/*  End of Stop2.                                                             */

/******************************************************************************
 *  Function:                                                                 *
 *      Gravity                                                               *
 *  Purpose:                                                                  *
 *      Computes the acceleration given by the inverse square law from        *
 *      Newton's universal law of gravitation.                                *
 *  Arguments:                                                                *
 *      p (*Vec3):                                                            *
 *          The position vector of the particle.                              *
 *  Outputs:                                                                  *
 *      a (Vec3):                                                             *
 *          The acceleration of the particle.                                 *
 ******************************************************************************/
func Gravity(p *Vec3) Vec3 {

    /*  Given a vector p, Newton's universal law of gravitation says the      *
     *  acceleration is proportional to p/||p||^3 = p_hat/||p||^2, where      *
     *  p_hat is the unit vector for p. We can compute p/||p||^3 in terms     *
     *  of the norm of p and the square of the norm of p. We have:            */
    var factor float64 = 1.0 / (p.Norm() * p.NormSq())

    /*  The acceleration is the minus of p times this factor. The reason it   *
     *  is minus p is because gravity pulls inward, so the acceleration is    *
     *  towards the black hole.                                               */
    return Vec3{-p.X*factor, -p.Y*factor, -p.Z*factor}
}
/*  End of Gravity.                                                           */

/******************************************************************************
 *  Function:                                                                 *
 *      Gravity2                                                              *
 *  Purpose:                                                                  *
 *      Computes the acceleration given by the inverse square law from        *
 *      Newton's universal law of gravitation for two gravitating objects.    *
 *      This is done using the principle of superposition.                    *
 *  Arguments:                                                                *
 *      p (*Vec3):                                                            *
 *          The position vector of the particle.                              *
 *  Outputs:                                                                  *
 *      a (Vec3):                                                             *
 *          The acceleration of the particle.                                 *
 ******************************************************************************/
func Gravity2(p *Vec3) Vec3 {

    /*  The force from one black hole is -R / ||R||^3, where R is the         *
     *  relative position vector from the point p to the center of the        *
     *  black hole. Compute this expression for both black holes.             */
    var f1 Vec3 = Vec3{BHX1 - p.X, -p.Y, -p.Z}
    var f2 Vec3 = Vec3{BHX2 - p.X, -p.Y, -p.Z}

    /*  We'll use the principle of superposition for the two black holes. */
    var factor1 float64 = 1.0 / (f1.Norm() * f1.NormSq())
    var factor2 float64 = 1.0 / (f2.Norm() * f2.NormSq())
    Vec3ScaleBy(factor1, &f1)
    Vec3ScaleBy(factor2, &f2)

    /*  The net force is computed by the principle of superposition.          *
     *  Add the two individual forces and return.                             */
    Vec3AddTo(&f1, &f2)
    return f1
}
/*  End of Gravity2.                                                          */
