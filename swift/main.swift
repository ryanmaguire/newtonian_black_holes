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
let prog_factor: Double = 100.0 / Double(ySize)
var x, y: UInt32
var ppm: PPM = PPM(name: "newtonian_black_hole.ppm")
ppm.initialize()

for y in 0..<ySize {
    for x in 0..<ySize {
        let p: Vec3 = pixelToPoint(x: x, y: y)
        let v: Vec3 = initialVelocity
        let c: Color
        var u: Vec6 = Vec6(p: p, v: v)
        eulerPath(u: &u, acc: gravity, halt: stop)
        c = checkerBoard(point: u.p)
        c.writeToPPM(ppm: ppm)
    }

    if y % 20 == 0 {
        print("Progress: \(Double(y)*prog_factor)%")
    }
}
