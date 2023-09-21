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

let zSrc: Double = -10.0
let zDetector: Double = 10.0
let zDetectorSquared: Double = zDetector*zDetector
let start: Double = -10.0
let end: Double = 10.0
let blackHoleRadius: Double = 1.0
let blackHoleRadiusSquared = blackHoleRadius*blackHoleRadius
let xSize: UInt32 = 1024
let ySize: UInt32 = 1024
let pxFactor: Double = (end - start) / Double(xSize - 1)
let pyFactor: Double = (end - start) / Double(ySize - 1)
let highlightThreshold: Double = 0.02
let bx1: Double = -3.0
let bx2: Double = +3.0
let initialVelocity: Vec3 = Vec3(x: 0.0, y: 0.0, z: 1.0)

func pixelToPoint(x: UInt32, y: UInt32) -> Vec3 {
    let px: Double = start + pxFactor * Double(x)
    let py: Double = start + pyFactor * Double(y)
    return Vec3(x: px, y: py, z: zSrc)
}

func stop(u: Vec6) -> Bool {
    if u.p.z >= zDetector {
        return true
    } else if u.p.normSquared() <= blackHoleRadiusSquared {
        return true
    } else {
        return false
    }
}

func gravity(point: Vec3) -> Vec3 {
    let norm: Double = point.norm()
    let norm_squared = norm*norm
    let factor: Double = -1.0 / (norm * norm_squared)
    return factor * point
}
