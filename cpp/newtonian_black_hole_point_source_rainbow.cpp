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
 *      Basic test of the nbh routines. Generates a black hole and draws      *
 *      the checkerboard pattern with a rainbow gradient to indicate the      *
 *      at which the photon hit the plane.                                    *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2024/04/04                                                        *
 ******************************************************************************/

/*  All header files for nbh are including here.                              */
#include "nbh/nbh.hpp"

/*  Main function for performing the raytracing.                              */
int main(void)
{
    /*  Name of the output ppm file.                                          */
    const char *name = "newtonian_black_hole_point_source_rainbow.ppm";

    /*  Reset the number of maximum iterations to something much higher.      */
    nbh::euler::reset_max_iters(1000000U);

    /*  Reset the radius of the black hole to be a point.                     */
    nbh::setup::reset_radius(0.01);

    /*  Use the template function to render the image.                        */
#ifdef _OPENMP
    nbh::prun(nbh::gravity, nbh::stop, nbh::color_gradient_checkerboard,
              nbh::euler::path, name);
#else
    nbh::run(nbh::gravity, nbh::stop, nbh::color_gradient_checkerboard,
             nbh::euler::path, name);
#endif
    return 0;
}
/*  End of main.                                                              */
