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
 ******************************************************************************/
package nbh;

public class Setup {
    public static double z_src = -10.0;
    public static double z_detector = 10.0;
    public static double z_detector_squared = z_detector*z_detector;
    public static double start = -10.0;
    public static double end = 10.0;
    public static int x_size = 1024;
    public static int y_size = 1024;
    public static double px_factor = (end - start) / (double)(x_size - 1);
    public static double py_factor = (end - start) / (double)(y_size - 1);
    public static double black_hole_radius = 1.0;
    public static double black_hole_radius_squared
         = black_hole_radius*black_hole_radius;

    public static Vec3 pixelToPoint(int x, int y) {
        double xpt = start + px_factor * (double)x;
        double ypt = start + py_factor * (double)y;
        double zpt = z_src;
        return new Vec3(xpt, ypt, zpt);
    }
}
