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
 *  Date:   2023/02/28                                                        *
 ******************************************************************************/

/*  Include guard to prevent including this file twice.                       */
#ifndef NBH_SETUP_HPP
#define NBH_SETUP_HPP

/*  Simple 3D vectors found here.                                             */
#include "nbh_vec3.hpp"

/*  Namespace for the mini-project. "Newtonian Black Holes."                  */
namespace nbh {

    /*  Another namespace to avoid name conflicts with these constants.       */
    namespace setup {

        /*  The z value for the source.                                       */
        static const double z_src = 10.0;

        /*  The start and end parameters for the "source". The source is the  *
         *  square [start, end] x [start, end] at height z_src.               */
        static const double start = -10.0;
        static const double end = 10.0;

        /*  The height of the detector plane.                                 */
        static const double z_detector = -10.0;

        /*  The square of the height of the detector, used frequently.        */
        static const double z_detector_sq = 100.0;

        /*  The radius of the black hole.                                     */
        static double black_hole_radius = 1.0;

        /*  The square of the black hole radius, also used frequently.        */
        static double black_hole_radius_sq = 1.0;

        /*  Number of pixels in the x and y axes.                             */
        static unsigned int xsize = 1024U;
        static unsigned int ysize = 1024U;

        /*  Factor used for converting from pixels to points in space.        */
        static double pxfact = 0.019550342130987292;
        static double pyfact = 0.019550342130987292;

        /*  Threshold for highlighting features (usually the origin).         */
        static const double highlight_threshold = 0.02;

        /*  Function for changing the radius of the black hole.               */
        inline void reset_radius(double r);

        /*  Function for resetting the resolution of the PPM file.            */
        inline void reset_size(unsigned int x, unsigned int y);

        /*  For two black holes, the centers lie on the x axis.               */
        static const double bhx1 = -3.0;
        static const double bhx2 = +3.0;
    }
    /*  End of "setup" namespace.                                             */

    /**************************************************************************
     *  Function:                                                             *
     *      setup::reset_radius                                               *
     *  Purpose:                                                              *
     *      Resets the radius of the black hole.                              *
     *  Arguments:                                                            *
     *      r (double):                                                       *
     *          The new radius.                                               *
     *  Outputs:                                                              *
     *      None (void).                                                      *
     **************************************************************************/
    inline void setup::reset_radius(double r)
    {
        black_hole_radius = r;
        black_hole_radius_sq = r*r;
    }

    /**************************************************************************
     *  Function:                                                             *
     *      setup::reset_size                                                 *
     *  Purpose:                                                              *
     *      Resets the size of the output PPM.                                *
     *  Arguments:                                                            *
     *      x (unsigned int):                                                 *
     *          The number of pixels in the x direction.                      *
     *      y (unsigned int):                                                 *
     *          The number of pixels in the y direction.                      *
     *  Outputs:                                                              *
     *      None (void).                                                      *
     **************************************************************************/
    inline void setup::reset_size(unsigned int x, unsigned int y)
    {
        setup::xsize = x;
        setup::ysize = y;
        setup::pxfact = (setup::end - setup::start) / double(x - 1U);
        setup::pyfact = (setup::end - setup::start) / double(y - 1U);
    }

    /**************************************************************************
     *  Function:                                                             *
     *      pixel_to_point.                                                   *
     *  Purpose:                                                              *
     *      Converts a pixel (x, y) on the PPM to a point in space.           *
     *  Arguments:                                                            *
     *      x (unsigned int):                                                 *
     *          The x coordinate of the pixel.                                *
     *      y (unsigned int):                                                 *
     *          The y coordinate of the pixel.                                *
     *  Outputs:                                                              *
     *      p (nbh::vec3):                                                    *
     *          The corresponding point in space to the given pixel.          *
     *  Method:                                                               *
     *      The point on the detector lies on the z = setup::z_src plane.     *
     *      Use this and convert the (x, y) components of the pixel to the    *
     *      (x, y) component on the detector.                                 *
     **************************************************************************/
    inline nbh::vec3 pixel_to_point(unsigned int x, unsigned int y)
    {
        const double xpt = setup::start + setup::pxfact*static_cast<double>(x);
        const double ypt = setup::start + setup::pyfact*static_cast<double>(y);
        return nbh::vec3(xpt, ypt, setup::z_src);
    }

    /**************************************************************************
     *  Function:                                                             *
     *      stop                                                              *
     *  Purpose:                                                              *
     *      Determines if a photon is still in motion for one black hole.     *
     *  Arguments:                                                            *
     *      v (const nbh::vec3 &):                                            *
     *          The vector corresponding to the given photon.                 *
     *  Outputs:                                                              *
     *      halt (bool):                                                      *
     *          Boolean for if the photon is still moving.                    *
     **************************************************************************/
    inline bool stop(const nbh::vec3 &v)
    {
        /*  Case 1: The photon has reached the detector.                      */
        if (v.z < setup::z_detector)
            return true;

        /*  Case 2: The black hole swallowed the photon.                      */
        else if (v.normsq() < setup::black_hole_radius_sq)
            return true;

        /*  Otherwise, the photon is still moving. Don't stop.                */
        return false;
    }

    /**************************************************************************
     *  Function:                                                             *
     *      stop2                                                             *
     *  Purpose:                                                              *
     *      Determines if a photon is still in motion for two black holes.    *
     *  Arguments:                                                            *
     *      v (const nbh::vec3 &):                                            *
     *          The vector corresponding to the given photon.                 *
     *  Outputs:                                                              *
     *      halt (bool):                                                      *
     *          Boolean for if the photon is still moving.                    *
     **************************************************************************/
    inline bool stop2(const nbh::vec3 &p)
    {
        /*  The black holes lie on the x axis. Compute the displacements to p.*/
        nbh::vec3 r1 = nbh::vec3(p.x - setup::bhx1, p.y, p.z);
        nbh::vec3 r2 = nbh::vec3(p.x - setup::bhx2, p.y, p.z);

        /*  Case 1: The photon has reached the detector.                      */
        if (p.z < setup::z_detector)
            return true;

        /*  Case 2: The first black hole swallowed the photon.                */
        else if (r1.normsq() < setup::black_hole_radius_sq)
            return true;

        /*  Case 3: The second black hole swallowed the photon.               */
        else if (r2.normsq() < setup::black_hole_radius_sq)
            return true;

        /*  Otherwise, the photon is still moving. Don't stop.                */
        return false;
    }

    /**************************************************************************
     *  Function:                                                             *
     *      gravity                                                           *
     *  Purpose:                                                              *
     *      Computes the acceleration given by the inverse square law from    *
     *      Newton's universal law of gravitation.                            *
     *  Arguments:                                                            *
     *      p (const nbh::vec3 &):                                            *
     *          The position vector of the particle.                          *
     *  Outputs:                                                              *
     *      a (nbh::vec3):                                                    *
     *          The acceleration of the particle.                             *
     **************************************************************************/
    inline nbh::vec3 gravity(const nbh::vec3 &p)
    {
        /*  Given a vector p, Newton's universal law of gravitation says the  *
         *  acceleration is proportional to p/||p||^3 = p_hat/||p||^2, where  *
         *  p_hat is the unit vector for p. We can compute p/||p||^3 in terms *
         *  of the norm of p and the square of the norm of p. We have:        */
        const double factor = 1.0 / (p.normsq() * p.norm());

        /*  The acceleration is the minus of p times this factor. The reason  *
         *  it is minus p is because gravity pulls inward, so the             *
         *  acceleration is towards the blacks hole.                          */
        return nbh::vec3(-p.x*factor, -p.y*factor, -p.z*factor);
    }

    /**************************************************************************
     *  Function:                                                             *
     *      gravity2                                                          *
     *  Purpose:                                                              *
     *      Computes the acceleration given by the inverse square law from    *
     *      Newton's universal law of gravitation for two gravitating objects.*
     *      This is done using the principle of superposition.                *
     *  Arguments:                                                            *
     *      p (const nbh::vec3 &):                                            *
     *          The position vector of the particle.                          *
     *  Outputs:                                                              *
     *      a (nbh::vec3):                                                    *
     *          The acceleration of the particle.                             *
     **************************************************************************/
    inline nbh::vec3 gravity2(const nbh::vec3 &p)
    {
        /*  The force from one black hole is -R / ||R||^3, where R is the     *
         *  relative position vector from the point p to the center of the    *
         *  black hole. Compute this expression for both black holes.         */
        nbh::vec3 r1 = nbh::vec3(setup::bhx1 - p.x, -p.y, -p.z);
        nbh::vec3 r2 = nbh::vec3(setup::bhx2 - p.x, -p.y, -p.z);

        /*  We'll use the principle of superposition for the two black holes. */
        const double factor1 = 1.0 / (r1.normsq() * r1.norm());
        const double factor2 = 1.0 / (r2.normsq() * r2.norm());
        nbh::vec3 f1 = r1 * factor1;
        nbh::vec3 f2 = r2 * factor2;

        /*  The net force is computed by the principle of superposition.      *
         *  Add the two individual forces and return.                         */
        return f1 + f2;
    }
}
/*  End of "nbh" namespace.                                                   */

#endif
/*  End of include guard.                                                     */
