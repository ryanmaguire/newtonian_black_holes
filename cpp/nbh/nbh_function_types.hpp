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
 *      Provides function types that are commonly used.                       *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/20                                                        *
 ******************************************************************************/

/*  Include guard to prevent including this file twice.                       */
#ifndef NBH_FUNCTION_TYPES_HPP
#define NBH_FUNCTION_TYPES_HPP

/*  3D vectors found here.                                                    */
#include "nbh_vec3.hpp"

/*  6D vectors defined here.                                                  */
#include "nbh_vec6.hpp"

/*  Colors for working with PPM files provided here.                          */
#include "nbh_color.hpp"

/*  Functions for describing equations of motion.                             */
typedef nbh::vec3 (*acceleration)(const nbh::vec3 &);

/*  Functions for determining if a photon is still in motion.                 */
typedef bool (*stopper)(const nbh::vec3 &);

/*  Functions that color the plane behind the black hole.                     */
typedef nbh::color (*colorer)(const nbh::vec6 &);

/*  Functions for raytracing the path of light.                               */
typedef void (*raytracer)(nbh::vec6 &, acceleration, stopper);

#endif
/*  End of include guard.                                                     */
