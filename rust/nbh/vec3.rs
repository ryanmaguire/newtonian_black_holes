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
 *      Routines for working with vectors in R^3.                             *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/05/08                                                        *
 ******************************************************************************/

/*  Needed for operator overloading.                                          */
use std::ops;

/*  Simple struct for vectors in R^3.                                         */
pub struct Vec3 {

    /*  A vector is defined by it's Cartesian components.                     */
    pub x: f64,
    pub y: f64,
    pub z: f64
}

/*  Copies the bits of a Vec3 to another.                                     */
impl Copy for Vec3 {}

/*  Implement Clone for Vec3. Does basically the same thing as Copy.          */
impl Clone for Vec3 {
    fn clone(&self) -> Vec3 {
        return *self
    }
}

/*  Vector addition. This is performed component-wise.                        */
impl ops::Add<Vec3> for Vec3 {
    type Output = Vec3;

    /*  Adder function. Add the individual components of the two vectors.     */
    fn add(self: Vec3, rhs: Vec3) -> Vec3 {
        return Vec3{x: self.x + rhs.x, y: self.y + rhs.y, z: self.z + rhs.z};
    }
}

/*  Vector addition. The output is stored in the left-hand side.              */
impl ops::AddAssign<Vec3> for Vec3 {

    /*  Adder function. Increment the components of self by rhs.              */
    fn add_assign(self: &mut Vec3, rhs: Vec3) {
        self.x += rhs.x;
        self.y += rhs.y;
        self.z += rhs.z;
    }
}

/*  Vector subtraction. This is performed component-wise.                     */
impl ops::Sub<Vec3> for Vec3 {
    type Output = Vec3;

    /*  Subtraction function. Subtract the components of the two vectors.     */
    fn sub(self: Vec3, rhs: Vec3) -> Vec3 {
        return Vec3{x: self.x - rhs.x, y: self.y - rhs.y, z: self.z - rhs.z};
    }
}

/*  Vector subtraction. The output is stored in the left-hand side.           */
impl ops::SubAssign<Vec3> for Vec3 {

    /*  Subtraction function. Deccrement the components of self by rhs.       */
    fn sub_assign(self: &mut Vec3, rhs: Vec3) {
        self.x -= rhs.x;
        self.y -= rhs.y;
        self.z -= rhs.z;
    }
}

/*  Scalar multiplication.                                                    */
impl ops::Mul<f64> for Vec3 {
    type Output = Vec3;

    /*  Scalar multiplication with the real number on the right.              */
    fn mul(self: Vec3, rhs: f64) -> Vec3 {
        return Vec3{x: rhs*self.x, y: rhs*self.y, z: rhs*self.z};
    }
}

impl ops::Mul<Vec3> for f64 {
    type Output = Vec3;

    /*  Scalar multiplication with the real number on the left.               */
    fn mul(self: f64, rhs: Vec3) -> Vec3 {
        return Vec3{x: self*rhs.x, y: self*rhs.y, z: self*rhs.z};
    }
}

/*  Methods for the Vec3 struct. Cross product, dot product, norm, etc.       */
impl Vec3 {

    /*  Euclidean cross-product in R^3.                                       */
    pub fn cross(self: Vec3, rhs: Vec3) -> Vec3 {
        let x: f64 = self.y*rhs.z - self.z*rhs.y;
        let y: f64 = self.z*rhs.x - self.x*rhs.z;
        let z: f64 = self.x*rhs.y - self.y*rhs.x;
        return Vec3{x: x, y: y, z: z};
    }

    /*  Euclidean dot-product. Add the component-wise products.               */
    pub fn dot(self: Vec3, rhs: Vec3) -> f64 {
        return self.x*rhs.x + self.y*rhs.y + self.z*rhs.z;
    }

    /*  Euclidean norm, also the L2 norm.                                     */
    pub fn norm(self: Vec3) -> f64 {
        return (self.x*self.x + self.y*self.y + self.z*self.z).sqrt();
    }

    /*  Square of the Euclidean norm. Avoids the square root call.            */
    pub fn norm_sq(self: Vec3) -> f64 {
        return self.x*self.x + self.y*self.y + self.z*self.z;
    }

    /*  Euclidean norm of the azimuthal part.                                 */
    pub fn rho(self: Vec3) -> f64 {
        return (self.x*self.x + self.y*self.y).sqrt();
    }

    /*  Square of the azimuthal component.                                    */
    pub fn rho_sq(self: Vec3) -> f64 {
        return self.x*self.x + self.y*self.y;
    }
}
