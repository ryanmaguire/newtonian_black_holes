(******************************************************************************
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
 ******************************************************************************)
UNIT nbh_Euler;

INTERFACE

(*  Vectors are used throughout representing particles in phase-space.        *)
USES nbh_Vec3, nbh_Vec6, nbh_FunctionTypes;

CONST
    (*  Maximum number of iterations allowed in Euler's method.               *)
    EulerMaxIters: Integer = 32767;

    (*  Step-size used in Euler's method.                                     *)
    EulerTimeIncrement: Real = 0.01;

(*  Iteratively performs Euler's method for numerically solving ODEs.         *)
Procedure EulerPath(var U: Vec6; Acc: Acceleration; Stop: Stopper);

IMPLEMENTATION

(******************************************************************************
 *  Procedure:                                                                *
 *      EulerPath                                                             *
 *  Purpose:                                                                  *
 *      Performs Euler's method to numerically solve an ODE.                  *
 *  Arguments:                                                                *
 *      U (var Vec6):                                                         *
 *          The "phase-space" vector describing the initial position and      *
 *          velocity. The output is also stored here.                         *
 *      Acc (Acceleration):                                                   *
 *          A vector-valued function describing the equation of motion.       *
 *      Stop (Stopper):                                                       *
 *          A stopping condition for when to stop Euler's method.             *
 ******************************************************************************)
Procedure EulerPath(var U: Vec6; Acc: Acceleration; Stop: Stopper);
VAR
    (*  Integer for keeping track of the number of iterations performed.      *)
    Iters: Integer;

    (*  3D Vector for storing the acceleration at a given point.              *)
    A: Vec3;

BEGIN
    (*  Keep track of the number of iterations performed.                     *)
    Iters := 0;

    (*  Iteratively perform Euler's method to numerically solve the ODE.      *)
    WHILE (Iters <= EulerMaxIters) DO
    BEGIN

        (*  If the stopping condition is satisfied abort the computation.     *)
        IF (Stop(U)) THEN BREAK;

        (*  We are solving P''(t) = A(P(t)). This is done numerically in two  *
         *  steps using Euler's method. First, V'(t) = A, and then P'(t) = V. *)
        A := Acc(U[0]);

        (*  Perform the Euler step for position: P_{n+1} = V_{n}*dt + P_{n}.  *)
        Vec3ScaledAddTo(U[0], EulerTimeIncrement, U[1]);

        (*  And the step for the velocity: V_{n+1} = A*dt + V_{n}.            *)
        Vec3ScaledAddTo(U[1], EulerTimeIncrement, A);

        (*  Avoid an infinite loop. Keep track of the number of iterations    *
         *  that have been performed and abort once it gets too high.         *)
        Iters += 1;
    END;
END;

END.
