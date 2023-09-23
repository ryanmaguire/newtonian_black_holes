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
 *      Basic test of the nbh routines. Generates a single black hole.        *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/23                                                        *
 ******************************************************************************/

/*  All header files for nbh are including here.                              */
#include "nbh/nbh.h"

/*  Main function for performing the raytracing.                              */
int main(void)
{
    /*  Name of the output ppm file.                                          */
    const char *name = "newtonian_black_hole_normalized.ppm";

    /*  The Schwarzschild radius is 1. Numerical errors cause some particles  *
     *  with an orbital radius of around 1 to escape and make it to the       *
     *  detector. Fix this by increasing the radius slightly. This only       *
     *  effects the "stopper" function, and not the actual ODE.               */
    nbh_setup_reset_radius(1.1);

    /*  Use the template functions to render the image.                       */
#ifdef _OPENMP
    nbh_prun(nbh_gravity, nbh_stop, nbh_checker_board,
             nbh_euler_normalized_path, name);
#else
    nbh_run(nbh_gravity, nbh_stop, nbh_checker_board,
            nbh_euler_normalized_path, name);
#endif
    return 0;
}
/*  End of main.                                                              */
