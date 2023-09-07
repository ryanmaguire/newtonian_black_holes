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
 *      Routines for working with vectors in R^6. Used for phase space.       *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/07                                                        *
 ******************************************************************************/

/*  Needed for operator overloading.                                          */
use std::ops;

/*  6D vectors are represented as two points in R^3. Position and velocity.   */
#[path = "vec3.rs"]
mod vec3;

/*  Struct for six-dimensional vectors.                                       */
pub struct Vec6 {

    /*  The components of the vector are 3D vectors, position and velocity.   */
    pub p: vec3::Vec3,
    pub v: vec3::Vec3
}

/*  Copies the bits of a Vec6 to another.                                     */
impl Copy for Vec6 {}

/*  Implement Clone for Vec6. Does basically the same thing as Copy.          */
impl Clone for Vec6 {
    fn clone(&self) -> Vec6 {
        return *self;
    }
}

/*  Vector addition. This is performed component-wise.                        */
impl ops::Add<Vec6> for Vec6 {
    type Output = Vec6;

    /*  Adder function. Add the individual components of the two vectors.     */
    fn add(self: Vec6, rhs: Vec6) -> Vec6 {
        return Vec6{p: self.p + rhs.p, v: self.v + rhs.v};
    }
}

/*  Vector addition. The output is stored in the left-hand side.              */
impl ops::AddAssign<Vec6> for Vec6 {

    /*  Adder function. Increment the components of self by rhs.              */
    fn add_assign(self: &mut Vec6, rhs: Vec6) {
        self.p += rhs.p;
        self.v += rhs.v;
    }
}

/*  Vector subtraction. This is performed component-wise.                     */
impl ops::Sub<Vec6> for Vec6 {
    type Output = Vec6;

    /*  Subtraction function. Subtract the components of the two vectors.     */
    fn sub(self: Vec6, rhs: Vec6) -> Vec6 {
        return Vec6{p: self.p - rhs.p, v: self.v - rhs.v};
    }
}

/*  Vector subtraction. The output is stored in the left-hand side.           */
impl ops::SubAssign<Vec6> for Vec6 {

    /*  Subtraction function. Deccrement the components of self by rhs.       */
    fn sub_assign(self: &mut Vec6, rhs: Vec6) {
        self.p -= rhs.p;
        self.v -= rhs.v;
    }
}

/*  Scalar multiplication.                                                    */
impl ops::Mul<f64> for Vec6 {
    type Output = Vec6;

    /*  Scalar multiplication with the real number on the right.              */
    fn mul(self: Vec6, rhs: f64) -> Vec6 {
        return Vec6{p: rhs*self.p, v: rhs*self.v};
    }
}

impl ops::Mul<Vec6> for f64 {
    type Output = Vec6;

    /*  Scalar multiplication with the real number on the left.               */
    fn mul(self: f64, rhs: Vec6) -> Vec6 {
        return Vec6{p: self*rhs.p, v: self*rhs.v};
    }
}

/*  Methods for the 6D vector struct.                                         */
impl Vec6 {

    /*  Euclidean dot product. Add the products of the components.            */
    pub fn dot(self: Vec6, rhs: Vec6) -> f64 {

        /*  The dot product is linear over component. Use the 3D dot product. */
        return self.p.dot(rhs.p) + self.v.dot(rhs.v);
    }

    /*  Square of the Euclidean norm.                                         */
    pub fn norm_sq(self: Vec6) -> f64 {

        /*  The square of the norm for a 6D vector is the sum of the squares  *
         *  of the norms of the individual 3D components. Compute this.       */
        return self.p.norm_sq() + self.v.norm_sq();
    }

    /*  The Euclidean norm, the length of the 6D vector.                      */
    pub fn norm(self: Vec6) -> f64 {

        /*  Use the norm_sq method above and take the square root of this.    */
        return self.norm_sq().sqrt();
    }
}

/*  Create a 6D vector from 6 real numbers.                                   */
pub fn from_real(x: f64, y: f64, z: f64, vx: f64, vy: f64, vz: f64) -> Vec6 {
    let p: vec3::Vec3 = vec3::Vec3{x: x, y: y, z: z};
    let v: vec3::Vec3 = vec3::Vec3{x: vx, y: vy, z: vz};
    return Vec6{p: p, v: v};
}
