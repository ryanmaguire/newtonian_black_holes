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
 ******************************************************************************
 *  Purpose:                                                                  *
 *      Routines for working with colors in RGB format.                       *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/07                                                        *
 ******************************************************************************/

/*  Needed for operator overloading.                                          */
use std::ops;

/*  Needed for writing to files.                                              */
use std::fs;
use std::io::prelude::*;

/*  Struct for RGB colors.                                                    */
pub struct Color {

    /*  A color is represented by three 8-bit channels. No alpha parameter.   */
    pub red: u8,
    pub green: u8,
    pub blue: u8
}

/*  Constant colors that are useful in drawings.                              */
pub const BLACK: Color = Color{red: 0x00, green: 0x00, blue: 0x00};
pub const WHITE: Color = Color{red: 0xFF, green: 0xFF, blue: 0xFF};
pub const RED: Color = Color{red: 0xFF, green: 0x00, blue: 0x00};
pub const GREEN: Color = Color{red: 0x00, green: 0xFF, blue: 0x00};
pub const BLUE: Color = Color{red: 0x00, green: 0x00, blue: 0xFF};
pub const CYAN: Color = Color{red: 0x00, green: 0xFF, blue: 0xFF};
pub const YELLOW: Color = Color{red: 0xFF, green: 0xFF, blue: 0x00};
pub const MAGENTA: Color = Color{red: 0xFF, green: 0x00, blue: 0xFF};

/*  Copies the bits of a Color to another.                                    */
impl Copy for Color {}

/*  Implement Clone for Color. Does basically the same thing as Copy.         */
impl Clone for Color {
    fn clone(&self) -> Color {
        return *self;
    }
}

/*  Scalar multiplication.                                                    */
impl ops::Mul<f64> for Color {
    type Output = Color;

    /*  Scalar multiplication with the real number on the right.              */
    fn mul(self: Color, rhs: f64) -> Color {

        /*  Cast the color channels in the Color struct to f64.               */
        let rf64: f64 = self.red as f64;
        let gf64: f64 = self.green as f64;
        let bf64: f64 = self.blue as f64;

        /*  Perform the multiplication component-wise and cast to u8.         */
        let r: u8 = (rhs * rf64) as u8;
        let g: u8 = (rhs * gf64) as u8;
        let b: u8 = (rhs * bf64) as u8;
        return Color{red: r, green: g, blue: b};
    }
}

impl ops::Mul<Color> for f64 {
    type Output = Color;

    /*  Scalar multiplication with the real number on the left.               */
    fn mul(self: f64, rhs: Color) -> Color {

        /*  Cast the color channels in the Color struct to f64.               */
        let rf64: f64 = rhs.red as f64;
        let gf64: f64 = rhs.green as f64;
        let bf64: f64 = rhs.blue as f64;

        /*  Perform the multiplication component-wise and cast to u8.         */
        let r: u8 = (self * rf64) as u8;
        let g: u8 = (self * gf64) as u8;
        let b: u8 = (self * bf64) as u8;
        return Color{red: r, green: g, blue: b};
    }
}

impl Color {
    pub fn write(self: Color, mut file: &fs::File) {
        let _ = file.write_all(&[self.red, self.green, self.blue]);
    }
}
