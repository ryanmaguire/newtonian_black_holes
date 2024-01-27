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
import std.stdio : writeln;
import nbh;

void main()
{
    nbh.ppm.PPM ppm = new nbh.ppm.PPM("test.ppm");
    uint x, y;
    nbh.vec3.Vec3 vstart = new nbh.vec3.Vec3(0.0, 0.0, -1.0);
    nbh.vec6.Vec6 u = new nbh.vec6.Vec6;
    immutable double prog_factor = 100.0 / cast(double)nbh.setup.ysize;

    for (y = 0U; y < nbh.setup.ysize; ++y)
    {
        for (x = 0U; x < nbh.setup.xsize; ++x)
        {
            u.pos = nbh.tools.pixelToPoint(x, y);
            u.vel = vstart;
            path(u, &nbh.tools.gravity, &nbh.tools.stop);
            nbh.color.Color c = nbh.color.checkerBoard(u.pos);
            c.write(ppm);
        }

        if ((y % 20U) == 0U)
            writeln(prog_factor * cast(double)y);
    }

    ppm.close;
}
