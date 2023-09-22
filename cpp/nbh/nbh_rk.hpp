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
 *      Provides functions for performing the Runge-Kutta method on ODEs.     *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/04/04                                                        *
 ******************************************************************************/

/*  Include guard to prevent including this file twice.                       */
#ifndef NBH_RK_HPP
#define NBH_RK_HPP

/*  Basic 3D vector struct given here.                                        */
#include "nbh_vec3.hpp"

/*  Basic 6D vector struct given here.                                        */
#include "nbh_vec6.hpp"

/*  Function typedefs are provided here.                                      */
#include "nbh_function_types.hpp"

/*  Namespace for the mini-project. "Newtonian Black Holes."                  */
namespace nbh {

    /*  To avoid name conflicts with Euler, use another namespace.            */
    namespace rk {

        /*  Time step used in the RK method. The user may change this. We may *
         *  safely set this to be 10x the size of the Euler increment since   *
         *  the global error in RK4 is O(h^4). The global error in Euler's    *
         *  method grows linearly.                                            */
        static double time_increment = 0.1;

        /*  The max number of iterations in the method.                       */
        static unsigned int max_iters = 65535U;

        /*  Function for resetting the max number of iterations allowed.      */
        inline void reset_max_iters(unsigned int n);

        /*  Function for performing the Runge-Kutta method, provided below.   */
        inline void path(nbh::vec6 &u, acceleration acc, stopper stop);
    }
    /*  End of rk namespace.                                                  */

    /**************************************************************************
     *  Function:                                                             *
     *      rk::path                                                          *
     *  Purpose:                                                              *
     *      Given a vector-valued acceleration a = acc(r), a starting         *
     *      position p, an initial velocity v, and a stopping condition stop, *
     *      perform the RK4 method to numerically solve the system of motion. *
     *      The initial conditions (p, v) are given by the 6D vector u.       *
     *  Arguments:                                                            *
     *      u (nbh::vec6 &):                                                  *
     *          A pointer to a 6D vector that represents the initial position *
     *          and velocity vectors of the particle.                         *
     *      acc (acceleration):                                               *
     *          A function that describes the equation of motion for the      *
     *          particle.                                                     *
     *      stop (stopper):                                                   *
     *          A function that determines when to stop the RK4 method.       *
     *  Outputs:                                                              *
     *      None (void).                                                      *
     *  Method:                                                               *
     *      Apply fourth order Runge-Kutta method. Given initial conditions   *
     *      (p0, v0) and time increment dt, we iteratively compute:           *
     *                                                                        *
     *          u_{n} = (p_{n}, v_{n})                                        *
     *          a_{n} = acc(p_{n})                                            *
     *          k1    = (v_{n}, a_{n})                                        *
     *          k2    = u_{n} + k1 * (dt/2)                                   *
     *          k3    = u_{n} + k2 * (dt/2)                                   *
     *          k4    = u_{n} + k2 * dt                                       *
     *                                                                        *
     *      These are the so-called RK factors. We update u_{n} via:          *
     *                                                                        *
     *          u_{n+1} = (dt/6) * (k1 + 2*k2 + 2*k3 + k4)                    *
     *                                                                        *
     *      Do this until the stopper function tells you to stop, or until    *
     *      you've done to many iterations.                                   *
     **************************************************************************/
    inline void rk::path(nbh::vec6 &u, acceleration acc, stopper stop)
    {
        /*  Variable for keeping track of the number of iterations.           */
        unsigned int n = 0U;

        /*  Multiples of the time increment used in the computation.          */
        const double dt = rk::time_increment;
        const double h0 = 0.5 * dt;
        const double h1 = dt * 0.1666666666666667;

        /*  Factor for Runge-Kutta method.                                    */
        nbh::vec6 k1 = nbh::vec6(u.v, acc(u.p));
        nbh::vec6 k2 = nbh::vec6(u.v + k1.v * h0, acc(u.p + k1.p * h0));
        nbh::vec6 k3 = nbh::vec6(u.v + k2.v * h0, acc(u.p + k2.p * h0));
        nbh::vec6 k4 = nbh::vec6(u.v + k2.v * dt, acc(u.p + k2.p * dt));

        /*  Iteratively perform the RK4 method.                               */
        while (!stop(u.p) && n < rk::max_iters)
        {
            /*  We numerically solve d^2/dt^2 p = F(p) by solving dp/dt = v   *
             *  and dv/dt = F(p). Using 6D "phase-space" vectors we can get   *
             *  the next iteration in the RK4 method in one line.             */
            u += (k1 + 2.0*k2 + 2.0*k3 + k4) * h1;

            /*  Update the Runge-Kutta factors.                               */
            k1 = nbh::vec6(u.v, acc(u.p));
            k2 = nbh::vec6(u.v + k1.v * h0, acc(u.p + k1.p * h0));
            k3 = nbh::vec6(u.v + k2.v * h0, acc(u.p + k2.p * h0));
            k4 = nbh::vec6(u.v + k2.v * dt, acc(u.p + k2.p * dt));

            /*  It is possible that a photon was captured into orbit, but not *
             *  absorbed into the black hole. To avoid an infinite loop,      *
             *  abort the computation once N gets to large.                   */
            ++n;
        }
    }
    /*  End of path function.                                                 */
}
/*  End of nbh namespace.                                                     */

#endif
/*  End of include guard.                                                     */
