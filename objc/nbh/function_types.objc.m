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
 *  Date:   2024/10/16                                                        *
 ******************************************************************************/

/*  3D vectors found here.                                                    */
#import "vec3.objc.m"

/*  6D vectors defined here.                                                  */
#import "vec6.objc.m"

/*  Colors for working with PPM files provided here.                          */
#import "color.objc.m"

/*  Functions for describing equations of motion.                             */
typedef id (*acceleration)(const Vec3 * const);

/*  Functions for determining if a photon is still in motion.                 */
typedef BOOL (*stopper)(const Vec3 * const);

/*  Functions that color the plane behind the black hole.                     */
typedef id (*colorer)(const Vec6 * const);

/*  Functions for raytracing the path of light.                               */
typedef void (*raytracer)(Vec6 *, acceleration, stopper);
