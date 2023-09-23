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
 *      Provides all tools needed to raytrace a Newtonian black hole.         *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/04/03                                                        *
 ******************************************************************************/
package main

/*  Only standard library packages are required.                              */
import (
    "fmt"   /*  Fprintf found here. */
    "sync"  /*  Parallel for-loop.  */
)

/******************************************************************************
 *                            Raytracing Routines                             *
 ******************************************************************************/

/******************************************************************************
 *  Function:                                                                 *
 *      Run                                                                   *
 *  Purpose:                                                                  *
 *      Runs the raytracing routines without parallelizing the computation.   *
 *  Arguments:                                                                *
 *      acc (Acceleration):                                                   *
 *          Function describing the equation of motion.                       *
 *      stop (Stopper):                                                       *
 *          Stopper function for determining when the photon stops moving.    *
 *      color (Colorer):                                                      *
 *          Color function for coloring the detector.                         *
 *      path (Raytracer):                                                     *
 *          The method of numerical raytracing. Options are Euler's method    *
 *          and fourth order Runge-Kutta.                                     *
 *      name (const char *):                                                  *
 *          The name of the output ppm file.                                  *
 ******************************************************************************/
func Run(acc Acceleration, stop Stopper,
         color Colorer, path Raytracer, name string) {

    /*  The vector v represents the initial velocity vector of a particle of  *
     *  light. Since our light rays are being directed downwards, this vector *
     *  should be (0, 0, -c), where c is the speed of light. We can take this *
     *  to be 1 for simplicity. Adjusting this value would be equivalent to   *
     *  adjusting the strength of gravity. Smaller values mean stronger       *
     *  gravity, and larger values mean weaker gravity.                       */
    var v_start Vec3 = Vec3{0.0, 0.0, -1.0}

    /*  The initial conditions of a particle of light.                        */
    var u Vec6

    /*  Variables for looping over the x and y coordinates of the detector.   */
    var x, y uint32

    /*  Factor used for printing a progress report.                           */
    var prog_factor float64 = 100.0 / float64(Y_Size)

    /*  Variable for the color.                                               */
    var c Color

    /*  Open the file and give it write permissions.                          */
    var ppm PPM

    ppm.Create(name)

    /*  If the constructor fails the FILE pointer will be NULL. Check this.   */
    if (ppm.Fp == nil) {
        return
    }

    /*  Otherwise initialize the ppm with default values in "setup".          */
    ppm.Init()

    /*  We can NOT do parallel processing with the creation of our PPM file   *
     *  since the order the values are computed is essential. If we wanted to *
     *  introduce parallel processing, we would need to store the colors in   *
     *  an array, and then create the PPM from that array. For the simplicity *
     *  of the code, this is not done.                                        */
    for y = 0; y < Y_Size; y += 1 {
        for x = 0; x < X_Size; x += 1 {

            /*  We're incrementing p across our detector.                     */
            u.P = PixelToPoint(x, y)

            /*  Set the starting velocity.                                    */
            u.V = v_start

            /*  Raytrace where the photon that hit p came from.               */
            path(&u, acc, stop)

            /*  Get the color for the current pixel.                          */
            c = color(&u)

            /*  Write the color to the PPM file.                              */
            c.WriteToPPM(&ppm)
        }
        /*  End of x for-loop.                                                */

        /*  Print a status update.                                            */
        if ((y % 20) == 0) {
            fmt.Printf("Progress: %.4f%%  \r", prog_factor*float64(y))
        }
    }
    /*  End of y for-loop.                                                    */

    /*  Print a final progress report.                                        */
    fmt.Printf("Progress: 100.0000%%\nDone\n")

    /*  Close the file and return.                                            */
    ppm.Close()
}
/*  End of Run.                                                               */

/******************************************************************************
 *  Function:                                                                 *
 *      PRun                                                                  *
 *  Purpose:                                                                  *
 *      Runs the raytracing routines and parallelizes the computation.        *
 *  Arguments:                                                                *
 *      acc (Acceleration):                                                   *
 *          Function describing the equation of motion.                       *
 *      stop (Stopper):                                                       *
 *          Stopper function for determining when the photon stops moving.    *
 *      color (Colorer):                                                      *
 *          Color function for coloring the detector.                         *
 *      path (Raytracer):                                                     *
 *          The method of numerical raytracing. Options are Euler's method    *
 *          and fourth order Runge-Kutta.                                     *
 *      name (const char *):                                                  *
 *          The name of the output ppm file.                                  *
 ******************************************************************************/
func PRun(acc Acceleration, stop Stopper,
          color Colorer, path Raytracer, name string) {

    /*  Total number of pixels in the drawing.                                */
    var size uint32 = X_Size * Y_Size

    /*  Variables for looping over the pixels.                                */
    var n uint32

    /*  Array for the colors.                                                 */
    var c []Color = make([]Color, size)

    /*  WaitGroup from the sync package for concurrency / parallelization.    */
    var wg sync.WaitGroup

    /*  Add the number of colors being written to the WaitGroup.              */
    wg.Add(int(size))

    /*  Open the file and give it write permissions.                          */
    var ppm PPM

    ppm.Create(name)

    /*  If the constructor fails the FILE pointer will be NULL. Check this.   */
    if (ppm.Fp == nil) {
        return
    }

    /*  Otherwise initialize the ppm with default values in "setup".          */
    ppm.Init()

    /*  Loop over each pixel of the ppm.                                      */
    for n = 0; n < size; n += 1 {

        /*  Go routine to parallelize the loop. This routine does not write   *
         *  to the ppm just yet since the order matters. Instead it stores    *
         *  the resulting colors in the color array "c".                      */
        go func(n uint32) {

            defer wg.Done()

            /*  Get the x and y components of the current pixel.              */
            var x uint32 = n % X_Size
            var y uint32 = n / Y_Size

            /*  The initial conditions of a particle of light.                */
            var u Vec6

            /*  We're incrementing p across our detector.                     */
            u.P = PixelToPoint(x, y)

            /*  Set the starting velocity.                                    */
            u.V =  Vec3{0.0, 0.0, -1.0}

            /*  Raytrace where the photon that hit p came from.               */
            path(&u, acc, stop)

            /*  Get the color for the current pixel.                          */
            c[n] = color(&u)
        }(n)
    }

    /*  Loop through each pixel and write the colors to the ppm in order.     */
    for n = 0; n < size; n += 1 {
        c[n].WriteToPPM(&ppm)
    }

    /*  Close the file and return.                                            */
    ppm.Close()
}
/*  End of PRun.                                                              */
