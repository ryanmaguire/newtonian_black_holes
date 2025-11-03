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

/*  C input-output routines provided here, like fopen, fclose, and fputs.     */
#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#else
import Foundation
#endif

struct Color {
    var red, green, blue: Int32

    func writeToFile(file: UnsafeMutablePointer<FILE>) {
        putc(red, file)
        putc(green, file)
        putc(blue, file)
    }

    func writeToPPM(ppm: PPM) {
        self.writeToFile(file: ppm.file)
    }
}

extension Color {
    static func + (left: Color, right: Color) -> Color {
        let red: Int32 = Int32(0.5*(Double(left.red) + Double(right.red)))
        let green: Int32 = Int32(0.5*(Double(left.green) + Double(right.green)))
        let blue: Int32 = Int32(0.5*(Double(left.blue) + Double(right.blue)))
        return Color(red: red, green: green, blue: blue)
    }

    static func += (left: inout Color, right: Color) {
        left = left + right
    }

    static func * (left: Color, right: Double) -> Color {
        let red: Int32 = Int32(Double(left.red) * right)
        let green: Int32 = Int32(Double(left.green) * right)
        let blue: Int32 = Int32(Double(left.blue) * right)
        return Color(red: red, green: green, blue: blue)
    }

    static func * (left: Double, right: Color) -> Color {
        let red: Int32 = Int32(left * Double(right.red))
        let green: Int32 = Int32(left * Double(right.green))
        let blue: Int32 = Int32(left * Double(right.blue))
        return Color(red: red, green: green, blue: blue)
    }

    static func *= (left: inout Color, right: Double) {
        left = left * right
    }
}

let black: Color = Color(red:   0, green:   0, blue:   0)
let white: Color = Color(red: 255, green: 255, blue: 255)
let red:   Color = Color(red: 255, green:   0, blue:   0)

func checkerBoard(point: Vec3) -> Color {
    let color_factor = zDetectorSquared / point.normSquared()
    if point.z < zDetector {
        return black
    } else if point.normSquared() <= blackHoleRadiusSquared {
        return black
    } else {
        let cx: Int32 = Int32(ceil(point.x))
        let cy: Int32 = Int32(ceil(point.y))
        let use_white: Int32 = (cx + cy) & 1

        if use_white != 0 {
            return color_factor * white
        } else {
            return color_factor * red
        }
    }
}
