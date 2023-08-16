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

/*  Function for printing a simple message. Demonstrates basic syntax.        */
func MyPrintFunction(x: Double) -> String {

    /*  The sqrt function is imported from the C standard library above.      */
    let y: Double = sqrt(x)

    /*  Triple-quotes allow for line-continuation and implicit newlines.      */
    return """
        \rHello, World!
        \rsqrt(\(x)) = \(y)
    """
}
/*  End of MyPrintFunction.                                                   */

/*  The input to the function.                                                */
let input: Double = 2.0

/*  The output. Pass the input to the function.                               */
let output: String = MyPrintFunction(x: input)

/*  The print function appends a newline automatically.                       */
print(output)
