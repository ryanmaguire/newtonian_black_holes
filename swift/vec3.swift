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

/*  sqrt function is provided here. Different import for macOS vs. Linux.     */
#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#else
import Foundation
#endif

struct Vec3 {
    var x, y, z: Double

    func normSquared() -> Double {
        return x*x + y*y + z*z
    }

    func norm() -> Double {
        return sqrt(self.normSquared())
    }

    func rhoSquared() -> Double {
        return x*x + y*y
    }

    func rho() -> Double {
        return sqrt(self.rhoSquared())
    }

    func dotProduct(right: Vec3) -> Double {
        return x*right.x + y*right.y + z*right.z
    }

    func crossProduct(right: Vec3) -> Vec3 {
        let cx: Double = y*right.z - z*right.y
        let cy: Double = z*right.x - x*right.z
        let cz: Double = x*right.y - y*right.x
        return Vec3(x: cx, y: cy, z: cz)
    }
}

extension Vec3 {
    static func + (left: Vec3, right: Vec3) -> Vec3 {
        let sum_x: Double = left.x + right.x
        let sum_y: Double = left.y + right.y
        let sum_z: Double = left.z + right.z
        return Vec3(x: sum_x, y: sum_y, z: sum_z)
    }

    static func += (left: inout Vec3, right: Vec3) {
        left = left + right
    }

    static func - (left: Vec3, right: Vec3) -> Vec3 {
        let diff_x: Double = left.x - right.x
        let diff_y: Double = left.y - right.y
        let diff_z: Double = left.z - right.z
        return Vec3(x: diff_x, y: diff_y, z: diff_z)
    }

    static func -= (left: inout Vec3, right: Vec3) {
        left = left - right
    }

    static func * (left: Double, right: Vec3) -> Vec3 {
        return Vec3(x: left*right.x, y: left*right.y, z: left*right.z)
    }

    static func * (left: Vec3, right: Double) -> Vec3 {
        return Vec3(x: left.x*right, y: left.y*right, z: left.z*right)
    }

    static func *= (left: inout Vec3, right: Double) {
        left = left * right
    }

    static func / (left: Vec3, right: Double) -> Vec3 {
        let rcpr: Double = 1.0 / right
        return rcpr * left
    }

    static func /= (left: inout Vec3, right: Double) {
        left = left / right
    }

    static func ^ (left: Vec3, right: Vec3) -> Vec3 {
        return left.crossProduct(right: right)
    }

    static func ^= (left: inout Vec3, right: Vec3) {
        left = left ^ right
    }

    static func % (left: Vec3, right: Vec3) -> Double {
        return left.dotProduct(right: right)
    }
}
