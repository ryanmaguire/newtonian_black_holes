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

struct PPM {
    var file: Optional<UnsafeMutablePointer<FILE>>

    init(name: String) {
        file = fopen(name, "w")
    }

    func initializeFromValues(x: UInt32, y: UInt32, type: Int) {
        fputs("P\(type)\n\(x) \(y)\n255\n", file)
    }

    func initialize() {
        self.initializeFromValues(x: 1024, y: 1024, type: 6)
    }

    func close() {
        fclose(file)
    }
}
