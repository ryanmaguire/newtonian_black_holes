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
 *      Provides typedefs for commonly used function types in this project.   *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/23                                                        *
 ******************************************************************************/
package main

/******************************************************************************
 *                             Types of Functions                             *
 ******************************************************************************/

/*  Used to define the force of gravity.                                      */
type Acceleration func(*Vec3) Vec3

/*  Stopper functions determine when light "stops" in the raytracing.         */
type Stopper func(*Vec3) bool

/*  Colorers determine how the detector is colored behind the black hole.     */
type Colorer func(*Vec6) Color

/*  Raytracers trace out the path of light under a given acceleration.        */
type Raytracer func(*Vec6, Acceleration, Stopper)
