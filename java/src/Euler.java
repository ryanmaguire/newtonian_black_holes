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

public class Euler {
    static int max_iterations = 65535;
    static double time_increment = 0.01;

    public static void path(Vec6 u, Acceleration acc, Stopper stopper) {
        int iters = 0;
        Vec3 a = new Vec3(0.0, 0.0, 0.0);

        while (!stopper.stop(u) && iters < max_iterations) {
            a = acc.acceleration(u.p);
            u.p.scaledAddTo(time_increment, u.v);
            u.v.scaledAddTo(time_increment, a);
            iters += 1;
        }
    }
}
