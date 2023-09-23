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
 *      Provides routines for performing Euler's method for ODEs.             *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/23                                                        *
 ******************************************************************************/
package main

/******************************************************************************
 *                         Euler's Method Constants.                          *
 ******************************************************************************/

/*  Time step used in Euler's method. The user may change this.               */
var Euler_Time_Increment float64 = 0.01

/*  The max number of iterations in Euler's method.                           */
var Euler_Max_Iters uint32 = 65535

/******************************************************************************
 *                         Euler's Method Functions.                          *
 ******************************************************************************/

/*  Function for resetting the max number of iterations allowed.              */
func EulerResetMaxIters(n uint32) {
    Euler_Max_Iters = n
}

/*  Function for resetting the step size in Euler's method.                   */
func EulerResetTimeIncrement(dt float64) {
    Euler_Time_Increment = dt
}

/******************************************************************************
 *  Function:                                                                 *
 *      EulerPath                                                             *
 *  Purpose:                                                                  *
 *      Given a vector-valued acceleration a = acc(r), a starting position p, *
 *      an initial velocity v, and a stopping condition stop, perform Euler's *
 *      method method to numerically solve the system of motion. The initial  *
 *      conditions (p, v) are given as the 6D vector u.                       *
 *  Arguments:                                                                *
 *      u (*Vec6):                                                            *
 *          A pointer to a 6D vector that represents the initial position and *
 *          velocity vectors of the particle.                                 *
 *      acc (Acceleration):                                                   *
 *          A function that describes the equation of motion for the particle.*
 *      stop (Stopper):                                                       *
 *          A stopper function that determines when to stop Euler's method.   *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 *  Method:                                                                   *
 *      Apply Euler's method. Given initial conditions (p0, v0) and time      *
 *      increment dt, we iteratively compute:                                 *
 *                                                                            *
 *          v_{n+1} = dt*acc(p_{n+1}) + v_{n}                                 *
 *          p_{n+1} = dt*v_{n+1} + p_{n}                                      *
 *                                                                            *
 *      Do this until the stopper function tells you to stop, or until you've *
 *      done to many iterations.                                              *
 ******************************************************************************/
func EulerPath(u *Vec6, acc Acceleration, stop Stopper) {

    /*  Use of this function with nbh makes a very naive assumption. Newton's *
     *  Second Law states that F = ma, where a is the acceleration. For       *
     *  gravity we obtain the vector-valued differential equation:            *
     *      -GMm p / ||p||^3 = m d^2/dt^2 p                                   *
     *  Where G is the universal gravitational constant, and M is the mass of *
     *  the black hole (m being the mass of the object under consideration).  *
     *  We can take G*M to be 1 for simplicity, since we never specified the  *
     *  units we're in. Now, if m is any non-zero value we can cancel to get: *
     *      p / ||p||^3 = d^2/dt^2 p                                          *
     *  Solving this vector-valued differential equation results in the       *
     *  trajectory of the object. The only problem is it is generally         *
     *  believed that photons, which are particles of light, have zero mass.  *
     *  So let's pretend they have a mass that is so stupidly small it would  *
     *  be impossible to measure, but not zero. Given this we could apply     *
     *  Newtonian mechanics to get a rough sketch of a black hole.            */
    var a Vec3

    /*  In the main use of this function the black hole is of radius r at     *
     *  the origin and our detector is the plane z = z0. Our source of light  *
     *  is some plane z = z1. So the light is coming down and heading towards *
     *  our detector. We'll increment time using a small value dt, and we'll  *
     *  keep incrementing until the light either hits the detector or is      *
     *  absorbed by the black hole. dt is given by the time_increment value,  *
     *  and the stopping condition (hitting the detector, or being absorbed   *
     *  by a black hole) is determined by the "stop" function.                */
    var n uint32

    /*  It is possible that a photon was captured into orbit, but not         *
     *  absorbed into the black hole. To avoid an infinite loop abort the     *
     *  computation once n gets too large.                                    */
    for n = 0; n < Euler_Max_Iters; n += 1 {

        /*  We numerically solve d^2/dt^2 p = F(p) in two steps. First we     *
         *  compute the velocity dp/dt, meaning we need to solve dv/dt = F(p).*
         *  We solve numerically with Euler's method. Then we use this v to   *
         *  compute p via dp/dt = v, again solving numerically with Euler's   *
         *  method. So long as dt is small, the error should be small as well.*/
        a = acc(&u.P)
        Vec3ScaledAddTo(&u.P, Euler_Time_Increment, &u.V)
        Vec3ScaledAddTo(&u.V, Euler_Time_Increment, &a)

        /*  Check if we can stop.                                             */
        if (stop(&u.P)) {
            break
        }
    }
}
/*  End of EulerPath.                                                         */
