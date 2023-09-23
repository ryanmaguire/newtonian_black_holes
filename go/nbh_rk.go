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
 *      Provides routines for performing the Runge-Kutta method for ODEs.     *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/09/23                                                        *
 ******************************************************************************/
package main

/******************************************************************************
 *                       Runge-Kutta Method Constants.                        *
 ******************************************************************************/

/*  Time step used in the RK4 method. The user may change this.               */
var RK_Time_Increment float64 = 0.1

/*  The max number of iterations in the RK4 method.                           */
var RK_Max_Iters uint32 = 65535

/******************************************************************************
 *                        Runge-Kutta Method Functions                        *
 ******************************************************************************/

/*  Function for resetting the max number of iterations allowed.              */
func RKResetMaxIters(n uint32) {
    RK_Max_Iters = n
}

/*  Function for resetting the step size in the RK4 method.                   */
func RKResetTimeIncrement(dt float64) {
    RK_Time_Increment = dt
}

/******************************************************************************
 *  Function:                                                                 *
 *      RK4Factor                                                             *
 *  Purpose:                                                                  *
 *      Computes the Runge-Kutta factor at step "h" with direction "u1".      *
 *  Arguments:                                                                *
 *      u0 (*Vec6):                                                           *
 *          A pointer to a 6D vector, the initial condition for RK4.          *
 *      h (float64):                                                          *
 *          The size of the step (usually dt or dt/2, pending factor).        *
 *      u1 (*Vec6):                                                           *
 *          A pointer to a 6D vector, the perturbing vector for u0.           *
 *      acc (Acceleration):                                                   *
 *          The function that describes the ODE. We are numerically solving   *
 *          p''(t) = acc(p(t)).                                               *
 *  Outputs:                                                                  *
 *      out (Vec6):                                                           *
 *          The Runge-Kutta factor.                                           *
 *  Method:                                                                   *
 *      We need to compute:                                                   *
 *                                                                            *
 *          p_out = v0 + h * v1                                               *
 *          v_out = acc(p0 + h * p1)                                          *
 *                                                                            *
 *      Where u0 = (p0, v0), u1 = (p1, v1), and out = (p_out, v_out).         *
 ******************************************************************************/
func RK4Factor(u0 *Vec6, h float64, u1 *Vec6, acc Acceleration) Vec6 {

    /*  Declare a variable for the output.                                    */
    var out Vec6

    /*  We can compute p0 + h*p1 in one step with the following.              */
    var p Vec3 = Vec3ScaledAdd(&u0.P, h, &u1.P)

    /*  Compute the first factor, p_out = v0 + h * v1.                        */
    out.P = Vec3ScaledAdd(&u0.V, h, &u1.V)

    /*  Compute acc(p0 + h * p1) using "p" from before.                       */
    out.V = acc(&p)
    return out
}
/*  End of RK4Factor.                                                         */

/******************************************************************************
 *  Function:                                                                 *
 *      RKPath                                                                *
 *  Purpose:                                                                  *
 *      Given a vector-valued acceleration a = acc(r), a starting position p, *
 *      an initial velocity v, and a stopping condition stop, perform the RK4 *
 *      method method to numerically solve the system of motion. The initial  *
 *      conditions (p, v) are given as the 6D vector u.                       *
 *  Arguments:                                                                *
 *      u (*Vec6):                                                            *
 *          A pointer to a 6D vector that represents the initial position and *
 *          velocity vectors of the particle.                                 *
 *      acc (Acceleration):                                                   *
 *          A function that describes the equation of motion for the particle.*
 *      stop (Stopper):                                                       *
 *          A stopper function that determines when to stop the RK4 method.   *
 *  Outputs:                                                                  *
 *      None.                                                                 *
 *  Method:                                                                   *
 *      Apply fourth order Runge-Kutta method. Given initial conditions       *
 *      (p0, v0) and time increment dt, we iteratively compute:               *
 *                                                                            *
 *          u_{n} = (p_{n}, v_{n})                                            *
 *          a_{n} = acc(p_{n})                                                *
 *          k1    = (v_{n}, a_{n})                                            *
 *          k2    = u_{n} + k1 * (dt/2)                                       *
 *          k3    = u_{n} + k2 * (dt/2)                                       *
 *          k4    = u_{n} + k2 * dt                                           *
 *                                                                            *
 *      These are the so-called RK factors. We update u_{n} via:              *
 *                                                                            *
 *          u_{n+1} = (dt/6) * (k1 + 2*k2 + 2*k3 + k4)                        *
 *                                                                            *
 *      Do this until the stopper function tells you to stop, or until you've *
 *      done to many iterations.                                              *
 ******************************************************************************/
func RKPath(u *Vec6, acc Acceleration, stop Stopper) {

    /*  Index for keeping track of the number of iterations performed.        */
    var n uint32

    /*  Constant multiples of dt used in the computation.                     */
    var dt float64 = RK_Time_Increment
    var h0 float64 = 0.5 * dt
    var h1 float64 = dt * 0.1666666666666667

    /*  Current acceleration vector given by the starting position.           */
    var a Vec3 = acc(&u.P)

    /*  Compute the initial Runge-Kutta factors.                              */
    var k1 Vec6 = Vec6{u.V, a}
    var k2 Vec6 = RK4Factor(u, h0, &k1, acc)
    var k3 Vec6 = RK4Factor(u, h0, &k2, acc)
    var k4 Vec6 = RK4Factor(u, dt, &k2, acc)

    /*  Iteratively performed RK4.                                            */
    for n = 0; n < RK_Max_Iters; n += 1 {

        /*  We numerically solve d^2/dt^2 p = F(p) in two steps. First we     *
         *  compute the velocity dp/dt, meaning we solve dv/dt = F(p). We     *
         *  solve numerically with the Runge-Kutta method. We use this v to   *
         *  compute p via dp/dt = v, solving numerically again. So long as dt *
         *  is small, the error should be small as well. Error is O(dt^4).    */
        u.P.X += h1 * (k1.P.X + 2.0*k2.P.X + 2.0*k3.P.X + k4.P.X)
        u.P.Y += h1 * (k1.P.Y + 2.0*k2.P.Y + 2.0*k3.P.Y + k4.P.Y)
        u.P.Z += h1 * (k1.P.Z + 2.0*k2.P.Z + 2.0*k3.P.Z + k4.P.Z)

        /*  Velocity component of the RK4 update.                             */
        u.V.X += h1 * (k1.V.X + 2.0*k2.V.X + 2.0*k3.V.X + k4.V.X)
        u.V.Y += h1 * (k1.V.Y + 2.0*k2.V.Y + 2.0*k3.V.Y + k4.V.Y)
        u.V.Z += h1 * (k1.V.Z + 2.0*k2.V.Z + 2.0*k3.V.Z + k4.V.Z)

        /*  Update the Runge-Kutta factors.                                   */
        a = acc(&u.P)
        k1 = Vec6{u.V, a}
        k2 = RK4Factor(u, h0, &k1, acc)
        k3 = RK4Factor(u, h0, &k2, acc)
        k4 = RK4Factor(u, dt, &k2, acc)

        /*  Check if we can stop.                                             */
        if (stop(&u.P)) {
            break
        }
    }
}
/*  End of RKPath function.                                                   */
