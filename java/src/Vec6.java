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
 *      Provides a basic double-precision 3D vector struct.                   *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/21                                                        *
 ******************************************************************************/
package nbh;

public class Vec6 {
    public Vec3 p, v;

    public Vec6(Vec3 position, Vec3 velocity) {
        p = position;
        v = velocity;
    }

    public double normSquared() {
        return p.normSquared() + v.normSquared();
    }

    public double norm() {
        return Math.sqrt(normSquared());
    }

    public Vec6 add(Vec6 u) {
        return new Vec6(p.add(u.p), v.add(u.p));
    }

    public void addTo(Vec6 u) {
        p.addTo(u.p);
        v.addTo(u.v);
    }

    public Vec6 subtract(Vec6 u) {
        return new Vec6(p.subtract(u.p), v.subtract(u.p));
    }

    public void subtractFrom(Vec6 u) {
        p.subtractFrom(u.p);
        v.subtractFrom(u.v);
    }

    public Vec6 multiply(double a) {
        return new Vec6(p.multiply(a), v.multiply(a));
    }

    public void multiplyBy(double a) {
        p.multiplyBy(a);
        v.multiplyBy(a);
    }

    public double dotProduct(Vec6 u) {
        return p.dotProduct(u.p) + v.dotProduct(u.v);
    }
}
