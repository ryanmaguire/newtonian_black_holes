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

public class Color {
    public int red, green, blue;

    public Color(int r, int g, int b) {
        red = r;
        green = g;
        blue = b;
    }

    public void write(PPM ppm) {
        ppm.write(String.format("%d %d %d%n", red, green, blue));
    }

    public Color scale(double t) {
        int r = (int)(t * red);
        int g = (int)(t * green);
        int b = (int)(t * blue);
        return new Color(r, g, b);
    }

    public void scaleBy(double t) {
        red = (int)(t * red);
        green = (int)(t * green);
        blue = (int)(t * blue);
    }

    public static Color checkerBoard(Vec6 u) {
        int cx, cy, use_white;
        double color_factor = Setup.z_detector_squared / u.p.normSquared();
        int val = (int)(255.0 * color_factor);

        if (u.p.z < Setup.z_detector) {
            return new Color(0, 0, 0);
        }

        cx = (int)(Math.ceil(u.p.x));
        cy = (int)(Math.ceil(u.p.y));
        use_white = (cx + cy) & 1;

        if (use_white != 0) {
            return new Color(val, val, val);
        } else {
            return new Color(val, 0, 0);
        }
    }
}
