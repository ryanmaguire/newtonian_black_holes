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
 *      Routines for raytracing Newtonian black holes in rust.                *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/06                                                        *
 ******************************************************************************/
#![crate_type = "lib"]
#![crate_name = "nbh"]

pub mod pi {
    pub const PI: f64 = 3.1415926535897932384626433832795028;
    pub const HALF_PI: f64 = 1.5707963267948966192313216916397514;
    pub const TWHO_PI: f64 = 6.28318530717958647692528676655900559;
}

/******************************************************************************
 *                                 3D Vectors                                 *
 ******************************************************************************/

pub mod vec3 {

    /*  Needed for operator overloading.                                      */
    use std::ops;

    /*  Simple struct for vectors in R^3.                                     */
    pub struct Vec3 {

        /*  A vector is defined by it's Cartesian components.                 */
        pub x: f64,
        pub y: f64,
        pub z: f64
    }

    /*  Copies the bits of a Vec3 to another.                                 */
    impl Copy for Vec3 {}

    /*  Implement Clone for Vec3. Does basically the same thing as Copy.      */
    impl Clone for Vec3 {
        fn clone(&self) -> Vec3 {
            return *self;
        }
    }

    /*  Vector addition. This is performed component-wise.                    */
    impl ops::Add<Vec3> for Vec3 {
        type Output = Vec3;

        /*  Adder function. Add the individual components of the two vectors. */
        fn add(self: Vec3, rhs: Vec3) -> Vec3 {
            return Vec3{
                x: self.x + rhs.x,
                y: self.y + rhs.y,
                z: self.z + rhs.z
            };
        }
    }

    /*  Vector addition. The output is stored in the left-hand side.          */
    impl ops::AddAssign<Vec3> for Vec3 {

        /*  Adder function. Increment the components of self by rhs.          */
        fn add_assign(self: &mut Vec3, rhs: Vec3) {
            self.x += rhs.x;
            self.y += rhs.y;
            self.z += rhs.z;
        }
    }

    /*  Vector subtraction. This is performed component-wise.                 */
    impl ops::Sub<Vec3> for Vec3 {
        type Output = Vec3;

        /*  Subtraction function. Subtract the components of the two vectors. */
        fn sub(self: Vec3, rhs: Vec3) -> Vec3 {
            return Vec3{
                x: self.x - rhs.x,
                y: self.y - rhs.y,
                z: self.z - rhs.z
            };
        }
    }

    /*  Vector subtraction. The output is stored in the left-hand side.       */
    impl ops::SubAssign<Vec3> for Vec3 {

        /*  Subtraction function. Deccrement the components of self by rhs.   */
        fn sub_assign(self: &mut Vec3, rhs: Vec3) {
            self.x -= rhs.x;
            self.y -= rhs.y;
            self.z -= rhs.z;
        }
    }

    /*  Scalar multiplication.                                                */
    impl ops::Mul<f64> for Vec3 {
        type Output = Vec3;

        /*  Scalar multiplication with the real number on the right.          */
        fn mul(self: Vec3, rhs: f64) -> Vec3 {
            return Vec3{x: rhs*self.x, y: rhs*self.y, z: rhs*self.z};
        }
    }

    impl ops::Mul<Vec3> for f64 {
        type Output = Vec3;

        /*  Scalar multiplication with the real number on the left.           */
        fn mul(self: f64, rhs: Vec3) -> Vec3 {
            return Vec3{x: self*rhs.x, y: self*rhs.y, z: self*rhs.z};
        }
    }

    /*  Methods for the Vec3 struct. Cross product, dot product, norm, etc.   */
    impl Vec3 {

        /*  Euclidean cross-product in R^3.                                   */
        pub fn cross(self: Vec3, rhs: Vec3) -> Vec3 {
            let x: f64 = self.y*rhs.z - self.z*rhs.y;
            let y: f64 = self.z*rhs.x - self.x*rhs.z;
            let z: f64 = self.x*rhs.y - self.y*rhs.x;
            return Vec3{x: x, y: y, z: z};
        }

        /*  Euclidean dot-product. Add the component-wise products.           */
        pub fn dot(self: Vec3, rhs: Vec3) -> f64 {
            return self.x*rhs.x + self.y*rhs.y + self.z*rhs.z;
        }

        /*  Square of the Euclidean norm. Avoids the square root call.        */
        pub fn norm_sq(self: Vec3) -> f64 {
            return self.x*self.x + self.y*self.y + self.z*self.z;
        }

        /*  Euclidean norm, also the L2 norm.                                 */
        pub fn norm(self: Vec3) -> f64 {
            return self.norm_sq().sqrt();
        }

        /*  Square of the azimuthal component.                                */
        pub fn rho_sq(self: Vec3) -> f64 {
            return self.x*self.x + self.y*self.y;
        }

        /*  Euclidean norm of the azimuthal part.                             */
        pub fn rho(self: Vec3) -> f64 {
            return self.rho_sq().sqrt();
        }
    }
    /*  End of impl Vec3.                                                     */
}
/*  End of mod::vec3.                                                         */

/******************************************************************************
 *                                 6D Vectors                                 *
 ******************************************************************************/

pub mod vec6 {

    /*  Needed for operator overloading.                                      */
    use std::ops;

    /*  3D Vectors are used to define 6D vectors and perform arithmetic.      */
    use vec3;

    /*  Struct for six-dimensional vectors.                                   */
    pub struct Vec6 {

        /*  The components of the vector are 3D vectors, the position and     *
         *  velocity of the particle the vector represents.                   */
        pub p: vec3::Vec3,
        pub v: vec3::Vec3
    }

    /*  Copies the bits of a Vec6 to another.                                 */
    impl Copy for Vec6 {}

    /*  Implement Clone for Vec6. Does basically the same thing as Copy.      */
    impl Clone for Vec6 {
        fn clone(&self) -> Vec6 {
            return *self;
        }
    }

    /*  Vector addition. This is performed component-wise.                    */
    impl ops::Add<Vec6> for Vec6 {
        type Output = Vec6;

        /*  Adder function. Add the individual components of the two vectors. */
        fn add(self: Vec6, rhs: Vec6) -> Vec6 {
            return Vec6{p: self.p + rhs.p, v: self.v + rhs.v};
        }
    }

    /*  Vector addition. The output is stored in the left-hand side.          */
    impl ops::AddAssign<Vec6> for Vec6 {

        /*  Adder function. Increment the components of self by rhs.          */
        fn add_assign(self: &mut Vec6, rhs: Vec6) {
            self.p += rhs.p;
            self.v += rhs.v;
        }
    }

    /*  Vector subtraction. This is performed component-wise.                 */
    impl ops::Sub<Vec6> for Vec6 {
        type Output = Vec6;

        /*  Subtraction function. Subtract the components of the two vectors. */
        fn sub(self: Vec6, rhs: Vec6) -> Vec6 {
            return Vec6{p: self.p - rhs.p, v: self.v - rhs.v};
        }
    }

    /*  Vector subtraction. The output is stored in the left-hand side.       */
    impl ops::SubAssign<Vec6> for Vec6 {

        /*  Subtraction function. Deccrement the components of self by rhs.   */
        fn sub_assign(self: &mut Vec6, rhs: Vec6) {
            self.p -= rhs.p;
            self.v -= rhs.v;
        }
    }

    /*  Scalar multiplication.                                                */
    impl ops::Mul<f64> for Vec6 {
        type Output = Vec6;

        /*  Scalar multiplication with the real number on the right.          */
        fn mul(self: Vec6, rhs: f64) -> Vec6 {
            return Vec6{p: rhs*self.p, v: rhs*self.v};
        }
    }

    impl ops::Mul<Vec6> for f64 {
        type Output = Vec6;

        /*  Scalar multiplication with the real number on the left.           */
        fn mul(self: f64, rhs: Vec6) -> Vec6 {
            return Vec6{p: self*rhs.p, v: self*rhs.v};
        }
    }

    /*  Methods for the 6D vector struct.                                     */
    impl Vec6 {

        /*  Euclidean dot product. Add the products of the components.        */
        pub fn dot(self: Vec6, rhs: Vec6) -> f64 {

            /*  The dot product is linear. Use the 3D dot product twice.      */
            return self.p.dot(rhs.p) + self.v.dot(rhs.v);
        }

        /*  Square of the Euclidean norm.                                     */
        pub fn norm_sq(self: Vec6) -> f64 {

            /*  The square of the norm for a 6D vector is the sum of the      *
             *  squares of the norms of the individual 3D components.         */
            return self.p.norm_sq() + self.v.norm_sq();
        }

        /*  The Euclidean norm, the length of the 6D vector.                  */
        pub fn norm(self: Vec6) -> f64 {

            /*  Use the norm_sq method above and take the square root of this.*/
            return self.norm_sq().sqrt();
        }
    }

    /*  Create a 6D vector from 6 real numbers.                               */
    pub fn
    from_real(x: f64, y: f64, z: f64, vx: f64, vy: f64, vz: f64) -> Vec6 {
        let p: vec3::Vec3 = vec3::Vec3{x: x, y: y, z: z};
        let v: vec3::Vec3 = vec3::Vec3{x: vx, y: vy, z: vz};
        return Vec6{p: p, v: v};
    }
}
/*  End of mod::vec6.                                                         */

/******************************************************************************
 *                              Setup Parameters                              *
 ******************************************************************************/

pub mod setup {
    use vec3;
    pub const Z_SRC: f64 = 10.0;
    pub const START: f64 = -10.0;
    pub const END: f64 = 10.0;
    pub const Z_DETECTOR: f64 = -10.0;
    pub const Z_DETECTOR_SQ: f64 = 100.0;
    pub const BLACK_HOLE_RADIUS: f64 = 1.0;
    pub const BLACK_HOLE_RADIUS_SQ: f64 = 1.0;
    pub const XSIZE: u32 = 1024;
    pub const YSIZE: u32 = 1024;
    pub const PXFACT: f64 = (END - START) / ((XSIZE - 1) as f64);
    pub const PYFACT: f64 = (END - START) / ((YSIZE - 1) as f64);
    pub const HIGHLIGHT_THRESHOLD: f64 = 0.02;
    pub const BHX1: f64 = -3.0;
    pub const BHX2: f64 = 3.0;

    pub fn pixel_to_point(x: u32, y: u32) -> vec3::Vec3 {
        let xpt: f64 = START + PXFACT * (x as f64);
        let ypt: f64 = START + PYFACT * (y as f64);
        return vec3::Vec3{x: xpt, y: ypt, z: Z_SRC};
    }

    pub fn stop(v: vec3::Vec3) -> bool {

        /*  Case 1: The photon has reached the detector.                      */
        if v.z < Z_DETECTOR {
            return true;
        }

        /*  Case 2: The black hole swallowed the photon.                      */
        else if v.norm_sq() < BLACK_HOLE_RADIUS_SQ {
            return true;
        }

        /*  Otherwise, the photon is still moving. Don't stop.                */
        return false;
    }

    pub fn stop2(p: vec3::Vec3) -> bool {

        /*  The black holes lie on the x axis. Compute the displacements to p.*/
        let r1: vec3::Vec3 = vec3::Vec3{x: p.x - BHX1, y: p.y, z: p.z};
        let r2: vec3::Vec3 = vec3::Vec3{x: p.x - BHX2, y: p.y, z: p.z};

        /*  Case 1: The photon has reached the detector.                      */
        if p.z < Z_DETECTOR {
            return true;
        }

        /*  Case 2: The first black hole swallowed the photon.                */
        else if r1.norm_sq() < BLACK_HOLE_RADIUS_SQ {
            return true;
        }

        /*  Case 3: The second black hole swallowed the photon.               */
        else if r2.norm_sq() < BLACK_HOLE_RADIUS_SQ {
            return true;
        }

        /*  Otherwise, the photon is still moving. Don't stop.                */
        return false;
    }

    pub fn gravity(p: vec3::Vec3) -> vec3::Vec3 {

        /*  Given a vector p, Newton's universal law of gravitation says the  *
         *  acceleration is proportional to p/||p||^3 = p_hat/||p||^2, where  *
         *  p_hat is the unit vector for p. We can compute p/||p||^3 in terms *
         *  of the norm of p and the square of the norm of p. We have:        */
        let factor: f64 = -1.0 / (p.norm() * p.norm_sq());

        /*  The acceleration is the minus of p times this factor. The reason  *
         *  it is minus p is because gravity pulls inward, so the             *
         *  acceleration is towards the black hole.                           */
        return factor * p;
    }

    pub fn gravity2(p: vec3::Vec3) -> vec3::Vec3 {

        let r1: vec3::Vec3 = vec3::Vec3{x: p.x - BHX1, y: p.y, z: p.z};
        let r2: vec3::Vec3 = vec3::Vec3{x: p.x - BHX2, y: p.y, z: p.z};
        let factor1: f64 = -1.0 / (r1.norm() * r1.norm_sq());
        let factor2: f64 = -1.0 / (r2.norm() * r2.norm_sq());
        return factor1*r1 + factor2*r2;
    }
}
/*  End of mod::setup.                                                        */

/******************************************************************************
 *                               PPM Functions                                *
 ******************************************************************************/

pub mod ppm {

    /*  Needed for writing to files.                                          */
    use std::fs;
    use std::path;
    use std::io::prelude::*;
    use setup;

    pub struct PPM {

        /*  A PPM struct is represented by the PPM file it points to.         */
        pub file: fs::File
    }

    impl PPM {
        pub fn create<P: AsRef<path::Path>>(path: P) -> PPM {

            let file_result = fs::File::create(path);

            let out_file = match file_result {
                Ok(file) => file,
                Err(error) => panic!("Problem opening file: {:?}", error),
            };

            return PPM{file: out_file};
        }

        pub fn init_from_vals(self: &mut PPM, x: u32, y: u32, t: i32) {

            /*  For integers between 1 and 5 pass the value to the preamble.  */
            if 0 < t && t < 6 {
                let _ = write!(self.file, "P{}\n{} {}\n255\n", t, x, y);
            }

            else {
                let _ = write!(self.file, "P6\n{} {}\n255\n", x, y);
            }
        }

        pub fn init(self: &mut PPM) {
            self.init_from_vals(setup::XSIZE, setup::YSIZE, 6);
        }
    }
}
/*  End of mod::ppm.                                                          */

/******************************************************************************
 *                              Color Functions                               *
 ******************************************************************************/

pub mod color {

    /*  Needed for operator overloading.                                      */
    use std::ops;

    /*  Needed for writing to files.                                          */
    use std::fs;
    use std::io::prelude::*;

    /*  Used for coloring functions at the bottom of this file.               */
    use vec6;
    use setup;
    use pi;

    /*  Struct for RGB colors.                                                */
    pub struct Color {

        /*  A color is represented by three 8-bit channels. No alpha.         */
        pub red: u8,
        pub green: u8,
        pub blue: u8
    }

    /*  Constant colors that are useful in drawings.                          */
    pub const BLACK: Color = Color{red: 0x00, green: 0x00, blue: 0x00};
    pub const WHITE: Color = Color{red: 0xFF, green: 0xFF, blue: 0xFF};
    pub const RED: Color = Color{red: 0xFF, green: 0x00, blue: 0x00};
    pub const GREEN: Color = Color{red: 0x00, green: 0xFF, blue: 0x00};
    pub const BLUE: Color = Color{red: 0x00, green: 0x00, blue: 0xFF};
    pub const CYAN: Color = Color{red: 0x00, green: 0xFF, blue: 0xFF};
    pub const YELLOW: Color = Color{red: 0xFF, green: 0xFF, blue: 0x00};
    pub const MAGENTA: Color = Color{red: 0xFF, green: 0x00, blue: 0xFF};

    /*  Copies the bits of a Color to another.                                */
    impl Copy for Color {}

    /*  Implement Clone for Color. Does basically the same thing as Copy.     */
    impl Clone for Color {
        fn clone(&self) -> Color {
            return *self;
        }
    }

    /*  Scalar multiplication.                                                */
    impl ops::Mul<f64> for Color {
        type Output = Color;

        /*  Scalar multiplication with the real number on the right.          */
        fn mul(self: Color, rhs: f64) -> Color {

            /*  Cast the color channels in the Color struct to f64.           */
            let rf64: f64 = self.red as f64;
            let gf64: f64 = self.green as f64;
            let bf64: f64 = self.blue as f64;

            /*  Perform the multiplication component-wise and cast to u8.     */
            let r: u8 = (rhs * rf64) as u8;
            let g: u8 = (rhs * gf64) as u8;
            let b: u8 = (rhs * bf64) as u8;
            return Color{red: r, green: g, blue: b};
        }
    }

    impl ops::Mul<Color> for f64 {
        type Output = Color;

        /*  Scalar multiplication with the real number on the left.           */
        fn mul(self: f64, rhs: Color) -> Color {

            /*  Cast the color channels in the Color struct to f64.           */
            let rf64: f64 = rhs.red as f64;
            let gf64: f64 = rhs.green as f64;
            let bf64: f64 = rhs.blue as f64;

            /*  Perform the multiplication component-wise and cast to u8.     */
            let r: u8 = (self * rf64) as u8;
            let g: u8 = (self * gf64) as u8;
            let b: u8 = (self * bf64) as u8;
            return Color{red: r, green: g, blue: b};
        }
    }

    /*  Scalar multiplication. The result is stored on the left-hand side.    */
    impl ops::MulAssign<f64> for Color {

        /*  Scalar multiplication with the real number on the right.          */
        fn mul_assign(self: &mut Color, rhs: f64) {

            /*  Cast the color channels in the Color struct to f64.           */
            let rf64: f64 = self.red as f64;
            let gf64: f64 = self.green as f64;
            let bf64: f64 = self.blue as f64;

            /*  Perform the multiplication component-wise and cast to u8.     */
            self.red = (rhs * rf64) as u8;
            self.green = (rhs * gf64) as u8;
            self.blue = (rhs * bf64) as u8;
        }
    }

    /*  Blends two colors by averaging over the color channels.               */
    impl ops::Add<Color> for Color {
        type Output = Color;

        /*  Adds two colors by averaging them.                                */
        fn add(self: Color, rhs: Color) -> Color {

            /*  Cast the color channels in the Color struct and average.      */
            let rf64: f64 = 0.5*((self.red as f64) + (rhs.red as f64));
            let gf64: f64 = 0.5*((self.green as f64) + (rhs.green as f64));
            let bf64: f64 = 0.5*((self.blue as f64) + (rhs.blue as f64));

            /*  Covert back to u8 for the output color.                       */
            let r: u8 = rf64 as u8;
            let g: u8 = gf64 as u8;
            let b: u8 = bf64 as u8;
            return Color{red: r, green: g, blue: b};
        }
    }

    /*  Blends two colors by averaging over the color channels.               */
    impl ops::AddAssign<Color> for Color {

        /*  Adds two colors by averaging them.                                */
        fn add_assign(self: &mut Color, rhs: Color) {

            /*  Cast the color channels in the Color struct and average.      */
            let rf64: f64 = 0.5*((self.red as f64) + (rhs.red as f64));
            let gf64: f64 = 0.5*((self.green as f64) + (rhs.green as f64));
            let bf64: f64 = 0.5*((self.blue as f64) + (rhs.blue as f64));

            /*  Covert back to u8 for the output color.                       */
            self.red = rf64 as u8;
            self.green = gf64 as u8;
            self.blue = bf64 as u8;
        }
    }

    impl Color {
        pub fn write(self: Color, mut file: &fs::File) {
            let _ = file.write_all(&[self.red, self.green, self.blue]);
        }
    }

    pub fn checker_board(u: vec6::Vec6) -> Color {

        /*  Factor for darkening the checker board.                           */
        let cfact: f64 = setup::Z_DETECTOR_SQ / u.p.norm_sq();

        /*  If the photon didn't make it, color the pixel black.              */
        if u.p.z > setup::Z_DETECTOR {
            return BLACK;
        }

        /*  Otherwise use a bit-wise trick to color the plane.                */
        else if ((u.p.x.ceil() + u.p.y.ceil()) as i32) & 1 == 0 {
            return cfact * RED;
        }

        return cfact * WHITE;
    }

    pub fn bright_checker_board(u: vec6::Vec6) -> Color {

        /*  Factor for darkening the checker board.                           */
        let cfact: f64 = 0.5*(setup::Z_DETECTOR_SQ / u.p.norm_sq() + 1.0);

        /*  If the photon didn't make it, color the pixel black.              */
        if u.p.z > setup::Z_DETECTOR {
            return BLACK;
        }

        /*  Otherwise use a bit-wise trick to color the plane.                */
        else if (u.p.x.ceil() + u.p.y.ceil()) as i32 & 1 == 0 {
            return cfact * RED;
        }

        return cfact * WHITE;
    }

    pub fn checker_board_highlight(u: vec6::Vec6) -> Color {

        /*  Factor for darkening the checker board.                           */
        let cfact: f64 = setup::Z_DETECTOR_SQ / u.p.norm_sq();

        /*  If the photon didn't make it, color the pixel black.              */
        if u.p.z > setup::Z_DETECTOR {
            return BLACK;
        }

        /*  If the center of the plane was hit, color blue.                   */
        else if u.p.rho_sq() < setup::HIGHLIGHT_THRESHOLD {
            return BLUE;
        }

        /*  Otherwise use a bit-wise trick to color the plane.                */
        else if (u.p.x.ceil() + u.p.y.ceil()) as i32 & 1 == 0 {
            return cfact * RED;
        }

        return cfact * WHITE;
    }

    pub fn checker_board_four(u: vec6::Vec6) -> Color {

        /*  Factor for darkening the checker board.                           */
        let cfact: f64 = setup::Z_DETECTOR_SQ / u.p.norm_sq();

        /*  Integers that determine the color.                                */
        let nx: i32 = (u.p.x.ceil() as i32) & 1;
        let ny: i32 = (u.p.y.ceil() as i32) & 1;
        let n: i32 = nx + (ny << 1);

        /*  If the photon didn't make it, color the pixel black.              */
        if u.p.z > setup::Z_DETECTOR {
            return BLACK;
        }

        match n {
            0 => return cfact * WHITE,
            1 => return cfact * YELLOW,
            2 => return cfact * GREEN,
            _ => return cfact * RED
        }
    }

    pub fn bright_checker_board_four(u: vec6::Vec6) -> Color {

        /*  Factor for darkening the checker board.                           */
        let cfact: f64 = 0.5*(setup::Z_DETECTOR_SQ / u.p.norm_sq() + 1.0);

        /*  Integers that determine the color.                                */
        let nx: i32 = (u.p.x.ceil() as i32) & 1;
        let ny: i32 = (u.p.y.ceil() as i32) & 1;
        let n: i32 = nx + (ny << 1);

        /*  If the photon didn't make it, color the pixel black.              */
        if u.p.z > setup::Z_DETECTOR {
            return BLACK;
        }

        match n {
            0 => return cfact * WHITE,
            1 => return cfact * YELLOW,
            2 => return cfact * GREEN,
            _ => return cfact * RED
        }
    }

    pub fn checker_board_four_highlight(u: vec6::Vec6) -> Color {

        /*  Factor for darkening the checker board.                           */
        let cfact: f64 = setup::Z_DETECTOR_SQ / u.p.norm_sq();

        /*  Integers that determine the color.                                */
        let nx: i32 = (u.p.x.ceil() as i32) & 1;
        let ny: i32 = (u.p.y.ceil() as i32) & 1;
        let n: i32 = nx + (ny << 1);

        /*  If the photon didn't make it, color the pixel black.              */
        if u.p.z > setup::Z_DETECTOR {
            return BLACK;
        }

        /*  If the center of the plane was hit, color blue.                   */
        else if u.p.rho_sq() < setup::HIGHLIGHT_THRESHOLD {
            return BLUE;
        }

        match n {
            0 => return cfact * WHITE,
            1 => return cfact * YELLOW,
            2 => return cfact * GREEN,
            _ => return cfact * RED
        }
    }

    pub fn angle_gradient(u: vec6::Vec6) -> Color {
        let r: u8;
        let g: u8;
        let b: u8;

        let angle: f64 = u.v.z.abs().atan2(u.v.rho());
        let scaled: f64 = 255.0 * angle / pi::HALF_PI;

        if scaled < 64.0 {
            r = 0x00;
            g = (4.0*scaled) as u8;
            b = 0xFF;
        }

        else if scaled < 128.0 {
            r = 0x00;
            g = 0xFF;
            b = (255.0 - 4.0*(scaled - 64.0)) as u8;
        }

        else if scaled < 192.0 {
            r = (4.0*(scaled - 128.0)) as u8;
            g = 0xFF;
            b = 0x00;
        }

        else if scaled < 255.0 {
            r = 0xFF;
            g = (255.0 - 4.0*(scaled - 192.0)) as u8;
            b = 0x00;
        }

        else {
            r = 0xFF;
            g = 0x00;
            b = 0x00;
        }

        return Color{red: r, green: g, blue: b};
    }

    pub fn color_gradient_checker_board(u: vec6::Vec6) -> Color {

        /*  If the photon didn't make it, color the pixel black.              */
        if u.p.z > setup::Z_DETECTOR {
            return BLACK;
        }

        let rainbow: Color = angle_gradient(u);
        let checker_board: Color = bright_checker_board(u);
        return rainbow + checker_board;
    }
}
/*  End of mod::color.                                                        */

/******************************************************************************
 *                               Functions Types                              *
 ******************************************************************************/

pub mod function_types {
    use vec3;
    use vec6;
    use color;

    pub type Acceleration = fn(vec3::Vec3) -> vec3::Vec3;
    pub type Stopper = fn(vec3::Vec3) -> bool;
    pub type Colorer = fn(vec6::Vec6) -> color::Color;
    pub type Raytracer = fn(&mut vec6::Vec6, Acceleration, Stopper);
}
/*  End of mod::function_types.                                               */

/******************************************************************************
 *                               Euler's Method                               *
 ******************************************************************************/

pub mod euler {
    use vec3;
    use vec6;
    use function_types::{Acceleration, Stopper};

    const MAX_ITERS: u32 = 65535;
    const TIME_INCREMENT: f64 = 0.01;

    pub fn path(u: &mut vec6::Vec6, acc: Acceleration, stop: Stopper) {
        let mut n: u32 = 0;

        while !stop(u.p) && n < MAX_ITERS {
            let a: vec3::Vec3 = acc(u.p);
            u.p += TIME_INCREMENT*u.v;
            u.v += TIME_INCREMENT*a;
            n += 1;
        }
    }
}

/******************************************************************************
 *                            Raytracing Routines                             *
 ******************************************************************************/

pub mod raytrace {
    use vec3;
    use vec6;
    use color;
    use function_types::{Acceleration, Stopper, Raytracer, Colorer};
    use ppm;
    use setup;
    use std::path;

    pub fn run<P: AsRef<path::Path>>(acc: Acceleration, stop: Stopper,
                                     color: Colorer, path: Raytracer, name: P) {
        const V_START: vec3::Vec3 = vec3::Vec3{x: 0.0, y: 0.0, z: -1.0};
        //let prog_factor: f64 = 100.0 / (setup::YSIZE as f64);
        let mut out = ppm::PPM::create(name);
        out.init();

        for y in 0..=setup::YSIZE-1 {
            for x in 0..=setup::XSIZE-1 {
                let mut u = vec6::Vec6{
                    p: setup::pixel_to_point(x, y),
                    v: V_START
                };
                path(&mut u, acc, stop);
                let c: color::Color = color(u);
                c.write(&out.file);
            }


        }
        println!();
    }
}
