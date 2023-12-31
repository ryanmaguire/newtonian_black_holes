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
 *      Provides the basic setup parameters of the black hole images.         *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/04/04                                                        *
 ******************************************************************************/

/*  Include guard to prevent including this file twice.                       */
#ifndef NBH_SETUP_H
#define NBH_SETUP_H

/*  NBH_INLINE macro found here.                                              */
#include "nbh_inline.h"

/*  Booleans given here.                                                      */
#include "nbh_bool.h"

/*  Simple 3D vectors found here.                                             */
#include "nbh_vec3.h"

/*  The z value for the source.                                               */
static const double nbh_setup_z_src = 10.0;

/*  The start and end parameters for the "source". The source is the          *
 *  square [start, end] x [start, end] at height z_src.                       */
static const double nbh_setup_start = -10.0;
static const double nbh_setup_end = 10.0;

/*  The height of the detector plane.                                         */
static const double nbh_setup_z_detector = -10.0;

/*  The square of the height of the detector, used frequently.                */
static const double nbh_setup_z_detector_sq = 100.0;

/*  The radius of the black hole.                                             */
static double nbh_setup_black_hole_radius = 1.0;

/*  The square of the black hole radius, also used frequently.                */
static double nbh_setup_black_hole_radius_sq = 1.0;

/*  Number of pixels in the x and y axes.                                     */
static unsigned int nbh_setup_xsize = 1024U;
static unsigned int nbh_setup_ysize = 1024U;

/*  Factor used for converting from pixels to points in space. These have the *
 *  values (nbh_setup_end - nbh_setup_start) / (size - 1), where size is the  *
 *  number of pixels in the x and y axes, respectively. If you change         *
 *  nbh_setup_xsize or nbh_setup_xsize, reset these numbers too.              */
static double nbh_setup_pxfact = 0.019550342130987292;
static double nbh_setup_pyfact = 0.019550342130987292;

/*  Threshold for highlighting features (usually the origin).                 */
static const double nbh_setup_highlight_threshold = 0.02;

/*  For two black holes, the centers lie on the x axis.                       */
static const double nbh_setup_bhx1 = -3.0;
static const double nbh_setup_bhx2 = +3.0;

/******************************************************************************
 *  Function:                                                                 *
 *      nbh_setup_reset_radius                                                *
 *  Purpose:                                                                  *
 *      Resets the radius of the black hole.                                  *
 *  Arguments:                                                                *
 *      r (double):                                                           *
 *          The new radius.                                                   *
 *  Outputs:                                                                  *
 *      None (void).                                                          *
 ******************************************************************************/
NBH_INLINE void
nbh_setup_reset_radius(double r)
{
    nbh_setup_black_hole_radius = r;
    nbh_setup_black_hole_radius_sq = r*r;
}
/*  End of nbh_setup_reset_radius.                                            */

/******************************************************************************
 *  Function:                                                                 *
 *      nbh_setup_reset_size                                                  *
 *  Purpose:                                                                  *
 *      Resets the size of the output PPM.                                    *
 *  Arguments:                                                                *
 *      x (unsigned int):                                                     *
 *          The number of pixels in the x direction.                          *
 *      y (unsigned int):                                                     *
 *          The number of pixels in the y direction.                          *
 *  Outputs:                                                                  *
 *      None (void).                                                          *
 ******************************************************************************/
NBH_INLINE void
nbh_setup_reset_size(unsigned int x, unsigned int y)
{
    nbh_setup_xsize = x;
    nbh_setup_ysize = y;
    nbh_setup_pxfact = (nbh_setup_end - nbh_setup_start) / (double)(x - 1U);
    nbh_setup_pyfact = (nbh_setup_end - nbh_setup_start) / (double)(y - 1U);
}
/*  End of nbh_setup_reset_size.                                              */

/******************************************************************************
 *  Function:                                                                 *
 *      nbh_pixel_to_point.                                                   *
 *  Purpose:                                                                  *
 *      Converts a pixel (x, y) on the PPM to a point in space.               *
 *  Arguments:                                                                *
 *      x (unsigned int):                                                     *
 *          The x coordinate of the pixel.                                    *
 *      y (unsigned int):                                                     *
 *          The y coordinate of the pixel.                                    *
 *  Outputs:                                                                  *
 *      p (struct nbh_vec3):                                                  *
 *          The corresponding point in space to the given pixel.              *
 *  Method:                                                                   *
 *      The point on the detector lies on the z = nbh_setup_z_src plane. Use  *
 *      this and convert the (x, y) components of the pixel to the (x, y)     *
 *      component on the detector.                                            *
 ******************************************************************************/
NBH_INLINE struct nbh_vec3
nbh_pixel_to_point(unsigned int x, unsigned int y)
{
    const double xpt = nbh_setup_start + nbh_setup_pxfact*(double)x;
    const double ypt = nbh_setup_start + nbh_setup_pyfact*(double)y;
    return nbh_vec3_rect(xpt, ypt, nbh_setup_z_src);
}
/*  End of nbh_pixel_to_point.                                                */

/******************************************************************************
 *  Function:                                                                 *
 *      nbh_stop                                                              *
 *  Purpose:                                                                  *
 *      Determines if a photon is still in motion for one black hole.         *
 *  Arguments:                                                                *
 *      v (const struct nbh_vec3 *):                                          *
 *          The vector corresponding to the given photon.                     *
 *  Outputs:                                                                  *
 *      stop (nbh_bool):                                                      *
 *          Boolean for if the photon is still moving.                        *
 ******************************************************************************/
NBH_INLINE nbh_bool
nbh_stop(const struct nbh_vec3 *v)
{
    /*  Case 1: The photon has reached the detector.                          */
    if (v->z < nbh_setup_z_detector)
        return nbh_true;

    /*  Case 2: The black hole swallowed the photon.                          */
    else if (nbh_vec3_normsq(v) < nbh_setup_black_hole_radius_sq)
        return nbh_true;

    /*  Otherwise, the photon is still moving. Don't stop.                    */
    return nbh_false;
}
/*  End of nbh_stop.                                                          */

/******************************************************************************
 *  Function:                                                                 *
 *      nbh_stop2                                                             *
 *  Purpose:                                                                  *
 *      Determines if a photon is still in motion for two black holes.        *
 *  Arguments:                                                                *
 *      v (const struct nbh_vec3 *):                                          *
 *          The vector corresponding to the given photon.                     *
 *  Outputs:                                                                  *
 *      stop (nbh_bool):                                                      *
 *          Boolean for if the photon is still moving.                        *
 ******************************************************************************/
NBH_INLINE nbh_bool
nbh_stop2(const struct nbh_vec3 *p)
{
    /*  The black holes lie on the x axis. Compute the displacements to p.    */
    const struct nbh_vec3 r1 = nbh_vec3_rect(p->x - nbh_setup_bhx1, p->y, p->z);
    const struct nbh_vec3 r2 = nbh_vec3_rect(p->x - nbh_setup_bhx2, p->y, p->z);

    /*  Case 1: The photon has reached the detector.                          */
    if (p->z < nbh_setup_z_detector)
        return nbh_true;

    /*  Case 2: The first black hole swallowed the photon.                    */
    else if (nbh_vec3_normsq(&r1) < nbh_setup_black_hole_radius_sq)
        return nbh_true;

    /*  Case 3: The second black hole swallowed the photon.                   */
    else if (nbh_vec3_normsq(&r2) < nbh_setup_black_hole_radius_sq)
        return nbh_true;

    /*  Otherwise, the photon is still moving. Don't stop.                    */
    return nbh_false;
}
/*  End of nbh_stop2.                                                         */

/******************************************************************************
 *  Function:                                                                 *
 *      nbh_gravity                                                           *
 *  Purpose:                                                                  *
 *      Computes the acceleration given by the inverse square law from        *
 *      Newton's universal law of gravitation.                                *
 *  Arguments:                                                                *
 *      p (const struct nbh_vec3 *):                                          *
 *          The position vector of the particle.                              *
 *  Outputs:                                                                  *
 *      a (struct nbh_vec3):                                                  *
 *          The acceleration of the particle.                                 *
 ******************************************************************************/
NBH_INLINE struct nbh_vec3
nbh_gravity(const struct nbh_vec3 *p)
{
    /*  Given a vector p, Newton's universal law of gravitation says the      *
     *  acceleration is proportional to p/||p||^3 = p_hat/||p||^2, where      *
     *  p_hat is the unit vector for p. We can compute p/||p||^3 in terms     *
     *  of the norm of p and the square of the norm of p. We have:            */
    const double factor = 1.0 / (nbh_vec3_norm(p) * nbh_vec3_normsq(p));

    /*  The acceleration is the minus of p times this factor. The reason it   *
     *  is minus p is because gravity pulls inward, so the acceleration is    *
     *  towards the black hole.                                               */
    return nbh_vec3_rect(-p->x*factor, -p->y*factor, -p->z*factor);
}
/*  End of nbh_gravity.                                                       */

/******************************************************************************
 *  Function:                                                                 *
 *      nbh_gravity2                                                          *
 *  Purpose:                                                                  *
 *      Computes the acceleration given by the inverse square law from        *
 *      Newton's universal law of gravitation for two gravitating objects.    *
 *      This is done using the principle of superposition.                    *
 *  Arguments:                                                                *
 *      p (const struct nbh_vec3 *):                                          *
 *          The position vector of the particle.                              *
 *  Outputs:                                                                  *
 *      a (struct nbh_vec3):                                                  *
 *          The acceleration of the particle.                                 *
 ******************************************************************************/
NBH_INLINE struct nbh_vec3
nbh_gravity2(const struct nbh_vec3 *p)
{
    /*  The force from one black hole is -R / ||R||^3, where R is the         *
     *  relative position vector from the point p to the center of the        *
     *  black hole. Compute this expression for both black holes.             */
    struct nbh_vec3 f1 = nbh_vec3_rect(nbh_setup_bhx1 - p->x, -p->y, -p->z);
    struct nbh_vec3 f2 = nbh_vec3_rect(nbh_setup_bhx2 - p->x, -p->y, -p->z);

    /*  We'll use the principle of superposition for the two black holes. */
    const double factor1 = 1.0 / (nbh_vec3_norm(&f1) * nbh_vec3_normsq(&f1));
    const double factor2 = 1.0 / (nbh_vec3_norm(&f2) * nbh_vec3_normsq(&f2));
    nbh_vec3_scaleby(factor1, &f1);
    nbh_vec3_scaleby(factor2, &f2);

    /*  The net force is computed by the principle of superposition.          *
     *  Add the two individual forces and return.                             */
    nbh_vec3_addto(&f1, &f2);
    return f1;
}
/*  End of nbh_gravity2.                                                      */

#endif
/*  End of include guard.                                                     */
