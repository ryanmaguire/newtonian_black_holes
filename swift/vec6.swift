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

struct Vec6 {
    var p, v: Vec3

    func normSquared() -> Double {
        return p.normSquared() + v.normSquared()
    }

    func norm() -> Double {
        return sqrt(self.normSquared())
    }

    func dotProduct(right: Vec6) -> Double {
        return p.dotProduct(right: right.p) + v.dotProduct(right: right.v)
    }
}

extension Vec6 {
    static func + (left: Vec6, right: Vec6) -> Vec6 {
        return Vec6(p: left.p + right.p, v: left.p + right.p)
    }

    static func += (left: inout Vec6, right: Vec6) {
        left = left + right
    }

    static func - (left: Vec6, right: Vec6) -> Vec6 {
        return Vec6(p: left.p - right.p, v: left.p - right.p)
    }

    static func -= (left: inout Vec6, right: Vec6) {
        left = left - right
    }

    static func * (left: Double, right: Vec6) -> Vec6 {
        return Vec6(p: left*right.p, v: left*right.v)
    }

    static func * (left: Vec6, right: Double) -> Vec6 {
        return Vec6(p: left.p*right, v: left.v*right)
    }

    static func *= (left: inout Vec6, right: Double) {
        left = left * right
    }

    static func / (left: Vec6, right: Double) -> Vec6 {
        let rcpr: Double = 1.0 / right
        return rcpr * left
    }

    static func /= (left: inout Vec6, right: Double) {
        left = left / right
    }

    static func % (left: Vec6, right: Vec6) -> Double {
        return left.dotProduct(right: right)
    }
}
