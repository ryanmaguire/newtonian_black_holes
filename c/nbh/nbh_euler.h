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
 *      Provides functions for performing Euler's method on ODEs.             *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/04/04                                                        *
 ******************************************************************************/

/*  Include guard to prevent including this file twice.                       */
#ifndef NBH_EULER_H
#define NBH_EULER_H

/*  NBH_INLINE macro found here.                                              */
#include "nbh_inline.h"

/*  Basic 3D vector struct given here.                                        */
#include "nbh_vec3.h"

/*  Basic 6D vector struct given here.                                        */
#include "nbh_vec6.h"

/*  Acceleration and stopper function typedefs here.                          */
#include "nbh_function_types.h"

/*  Time step used in Euler's method. The user may change this.               */
static double nbh_euler_time_increment = 0.01;

/*  The max number of iterations in Euler's method.                           */
static unsigned int nbh_euler_max_iters = 65535U;

/*  Function for resetting the max number of iterations allowed.              */
NBH_INLINE void
nbh_euler_reset_max_iters(unsigned int n)
{
    nbh_euler_max_iters = n;
}

/*  Function for resetting the step size in Euler's method.                   */
NBH_INLINE void
nbh_euler_reset_time_increment(double dt)
{
    nbh_euler_time_increment = dt;
}

/******************************************************************************
 *  Function:                                                                 *
 *      nbh_euler_path                                                        *
 *  Purpose:                                                                  *
 *      Given a vector-valued acceleration a = acc(r), a starting position p, *
 *      an initial velocity v, and a stopping condition stop, perform Euler's *
 *      method method to numerically solve the system of motion. The initial  *
 *      conditions (p, v) are given as the 6D vector u.                       *
 *  Arguments:                                                                *
 *      u (struct nbh_vec6 *):                                                *
 *          A pointer to a 6D vector that represents the initial position and *
 *          velocity vectors of the particle.                                 *
 *      acc (acceleration):                                                   *
 *          A function that describes the equation of motion for the particle.*
 *      stop (stopper):                                                       *
 *          A stopper function that determines when to stop Euler's method.   *
 *  Outputs:                                                                  *
 *      None (void).                                                          *
 *  Method:                                                                   *
 *      Apply Euler's method. Given initial conditions (p0, v0) and time      *
 *      increment dt, we iteratively compute:                                 *
 *                                                                            *
 *          v_{n+1} = dt*acc(p_{n}) + v_{n}                                   *
 *          p_{n+1} = dt*v_{n} + p_{n}                                        *
 *                                                                            *
 *      Do this until the stopper function tells you to stop, or until you've *
 *      done to many iterations.                                              *
 ******************************************************************************/
NBH_INLINE void
nbh_euler_path(struct nbh_vec6 *u, acceleration acc, stopper stop)
{
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
    struct nbh_vec3 a;

    /*  In the main use of this function the black hole is of radius r at     *
     *  the origin and our detector is the plane z = z0. Our source of light  *
     *  is some plane z = z1. So the light is coming down and heading towards *
     *  our detector. We'll increment time using a small value dt, and we'll  *
     *  keep incrementing until the light either hits the detector or is      *
     *  absorbed by the black hole. dt is given by the time_increment value,  *
     *  and the stopping condition (hitting the detector, or being absorbed   *
     *  by a black hole) is determined by the "stop" function.                */
    unsigned int n = 0U;

    /*  Keep performing Euler's method until we hit the detector or perform   *
     *  too many iterations.                                                  */
    while (!stop(&u->p) && n < nbh_euler_max_iters)
    {
        /*  We numerically solve d^2/dt^2 p = F(p) in two steps. First we     *
         *  compute the velocity dp/dt, meaning we need to solve dv/dt = F(p).*
         *  We solve numerically with Euler's method. Then we use this v to   *
         *  compute p via dp/dt = v, again solving numerically with Euler's   *
         *  method. So long as dt is small, the error should be small as well.*/
        a = acc(&u->p);
        nbh_vec3_scaled_addto(&u->p, nbh_euler_time_increment, &u->v);
        nbh_vec3_scaled_addto(&u->v, nbh_euler_time_increment, &a);

        /*  It is possible that a photon was captured into orbit, but not     *
         *  absorbed into the black hole. To avoid an infinite loop           *
         *  abort the computation once n gets too large.                      */
        ++n;
    }
}
/*  End of nbh_euler_path.                                                    */

/******************************************************************************
 *  Function:                                                                 *
 *      nbh_euler_normalized_path                                             *
 *  Purpose:                                                                  *
 *      Perhaps the worst part of the previous "Newtonian approximation" is   *
 *      that the speed of light is not constant. It increases under the force *
 *      of gravity, and very quickly exceeds the speed of light. This idea    *
 *      halts that. The acceleration is only allowed to act in a direction    *
 *      orthogonal to the velocity vector. This keeps the speed constant.     *
 *      The derivation for this is straight-forward:                          *
 *                                                                            *
 *                        v . a = 0                                           *
 *                  => 2(v . a) = 0                                           *
 *             => v . a + a . v = 0                                           *
 *            => d / dt (v . v) = 0                                           *
 *          => d / dt (||v||^2) = 0                                           *
 *                   => ||v||^2 = constant                                    *
 *                     => ||v|| = constant                                    *
 *  Arguments:                                                                *
 *      u (struct nbh_vec6 *):                                                *
 *          A pointer to a 6D vector that represents the initial position and *
 *          velocity vectors of the particle.                                 *
 *      acc (acceleration):                                                   *
 *          A function that describes the equation of motion for the particle.*
 *      stop (stopper):                                                       *
 *          A stopper function that determines when to stop Euler's method.   *
 *  Outputs:                                                                  *
 *      None (void).                                                          *
 *  Method:                                                                   *
 *      Same as before, but at each step compute the orthogonal component of  *
 *      the acceleration a along the velocity vector v and use this.          *
 ******************************************************************************/
NBH_INLINE void
nbh_euler_normalized_path(struct nbh_vec6 *u, acceleration acc, stopper stop)
{
    /*  Index for keeping track of the number of iterations performed.        */
    unsigned int n = 0U;

    /*  Keep performing Euler's method until we hit the detector or perform   *
     *  too many iterations.                                                  */
    while (!stop(&u->p) && n < nbh_euler_max_iters)
    {
        /*  Compute the acceleration at the point p.                          */
        struct nbh_vec3 a = acc(&u->p);

        /*  To avoid changing the speed of light, get the orthogonal part of  *
         *  a along v. If a and v are orthogonal, ||v|| is constant.          */
        a = nbh_vec3_orthogonal_part(&a, &u->v);

        /*  Perform Euler's method in two steps for both v' = a and p' = v.   */
        nbh_vec3_scaled_addto(&u->p, nbh_euler_time_increment, &u->v);
        nbh_vec3_scaled_addto(&u->v, nbh_euler_time_increment, &a);

        /*  Just to be on the paranoid side, normalize v. As the dt factor    *
         *  tends to zero, the norm of v will be closer to a constant. But    *
         *  for any fixed positive dt there will be some deviation in the     *
         *  value of ||v||. Correct for this by normalizing.                  */
        nbh_vec3_normalizeself(&u->v);

        /*  Update the counter to avoid an infinite loop.                     */
        ++n;
    }
}
/*  End of nbh_euler_normalized_path.                                         */

#endif
/*  End of include guard.                                                     */
