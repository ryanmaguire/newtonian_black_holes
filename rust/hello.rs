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

/*  Function for printing a simple message. Demonstrates basic syntax.        */
fn my_print_function(x: f64) -> String {

    /*  The square root function is a method for f64.                         */
    let y: f64 = x.sqrt();

    /*  The format macro can create formatted strings.                        */
    return format!("Hello, World!\nsqrt({}) = {}", x, y);
}
/*  End of my_print_function.                                                 */

/*  Function for testing basic rust syntax.                                   */
fn main() {

    /*  The input to the function.                                            */
    let input: f64 = 2.0;

    /*  The output. Pass the input to the function.                           */
    let output: String = my_print_function(input);

    /*  The println macro appends a newline automatically.                    */
    println!("{}", output);
}
/*  End of main.                                                              */
