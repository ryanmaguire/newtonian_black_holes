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
import nbh.Vec3;
import nbh.Vec6;
import nbh.PPM;
import nbh.Color;
import nbh.Setup;
import nbh.Gravity;
import nbh.Stop;
import nbh.Euler;

public class Main {
    public static void main(String[] args) {
        int x, y;
        Color c;

        double prog_factor = 100.0 / (double)(Setup.y_size);
        Gravity gravity = new Gravity();
        Stop stop = new Stop();

        PPM ppm = new PPM("newtonian_black_holes.ppm");
        ppm.init();

        for (y = 0; y < Setup.y_size; ++y) {
            for (x = 0; x < Setup.x_size; ++x) {
                Vec3 p = Setup.pixelToPoint(x, y);
                Vec3 v = new Vec3(0.0, 0.0, 1.0);
                Vec6 u = new Vec6(p, v);

                Euler.path(u, gravity, stop);

                c = Color.checkerBoard(u);
                c.write(ppm);
            }

            if (y % 20 == 0) {
              System.out.printf("Progress: %f%%     \r", prog_factor*(double)y);
            }
        }
        System.out.println("Progress: 100%         ");
        System.out.println("Done.");
        ppm.close();
    }
}
