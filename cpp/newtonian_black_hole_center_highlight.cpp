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
 *      Basic test of the nbh routines. Generates a single black hole and     *
 *      highlights the center of the detector with a blue color.              *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/02/28                                                        *
 ******************************************************************************/

/*  All header files for nbh are including here.                              */
#include "nbh.hpp"

/*  Main function for performing the raytracing.                              */
int main(void)
{
    /*  Name of the output ppm file.                                          */
    const char *name = "newtonian_black_hole_center_highlight.ppm";

    /*  Use the template function to render the image.                        */
#ifdef _OPENMP
    nbh::prun(nbh::gravity, nbh::stop, nbh::checker_board_highlight,
              nbh::euler::path, name);
#else
    nbh::run(nbh::gravity, nbh::stop, nbh::checker_board_highlight,
             nbh::euler::path, name);
#endif
    return 0;
}
/*  End of main.                                                              */
