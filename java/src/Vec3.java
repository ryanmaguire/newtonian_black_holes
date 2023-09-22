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

public class Vec3 {
    public double x, y, z;

    public Vec3(double a, double b, double c) {
        x = a;
        y = b;
        z = c;
    }

    public double normSquared() {
        return x*x + y*y + z*z;
    }

    public double norm() {
        return Math.sqrt(normSquared());
    }

    public double rhoSquared() {
        return x*x + y*y;
    }

    public double rho() {
        return Math.sqrt(rhoSquared());
    }

    public double dotProduct(Vec3 v) {
        return x*v.x + y*v.y + z*v.z;
    }

    public Vec3 add(Vec3 v) {
        return new Vec3(x + v.x, y + v.y, z + v.z);
    }

    public void addTo(Vec3 v) {
        x += v.x;
        y += v.y;
        z += v.z;
    }

    public Vec3 multiply(double a) {
        return new Vec3(a*x, a*y, a*z);
    }

    public void multiplyBy(double a) {
        x *= a;
        y *= a;
        z *= a;
    }

    public Vec3 subtract(Vec3 v) {
        return new Vec3(x - v.x, y - v.y, z - v.z);
    }

    public void subtractFrom(Vec3 v) {
        x -= v.x;
        y -= v.y;
        z -= v.z;
    }

    public Vec3 divide(double a) {
        double rcpr = 1.0 / a;
        return new Vec3(rcpr * x, rcpr * y, rcpr * z);
    }

    public void divideBy(double a) {
        double rcpr = 1.0 / a;
        x *= rcpr;
        y *= rcpr;
        z *= rcpr;
    }

    public Vec3 scaledAdd(double a, Vec3 v) {
        return new Vec3(x + a*v.x, y + a*v.y, z + a*v.z);
    }

    public void scaledAddTo(double a, Vec3 v) {
        x += a*v.x;
        y += a*v.y;
        z += a*v.z;
    }

    public Vec3 crossProduct(Vec3 v) {
        double a = y*v.z - z*v.y;
        double b = z*v.x - x*v.z;
        double c = x*v.y - y*v.x;
        return new Vec3(a, b, c);
    }

    public void crossWith(Vec3 v) {
        double a = x;
        double b = y;

        x = b*v.z - z*v.y;
        y = z*v.x - a*v.z;
        z = a*v.y - b*v.x;
    }
}
