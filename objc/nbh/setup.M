#import "vec3.M"

/*  The z value for the source.                                               */
static const double zSrc = 10.0;

/*  The start and end parameters for the "source". The source is the          *
 *  square [start, end] x [start, end] at height z_src.                       */
static const double start = -10.0;
static const double end = 10.0;

/*  The height of the detector plane.                                         */
static const double zDetector = -10.0;

/*  The square of the height of the detector, used frequently.                */
static const double zDetectorSq = 100.0;

/*  The radius of the black hole.                                             */
static const double blackHoleRadius = 1.0;

/*  The square of the black hole radius, also used frequently.                */
static const double blackHoleRadiusSq = 1.0;

/*  Number of pixels in the x and y axes.                                     */
static const unsigned int xSize = 1024U;
static const unsigned int ySize = 1024U;

/*  Factor used for converting from pixels to points in space.                */
static const double pxFactor = 0.019550342130987292;
static const double pyFactor = 0.019550342130987292;

/*  Threshold for highlighting features (usually the origin).                 */
static const double highlightThreshold = 0.02;

/*  For two black holes, the centers lie on the x axis.                       */
static const double bhx1 = -3.0;
static const double bhx2 = +3.0;


/******************************************************************************
 *  Function:                                                                 *
 *      pixelToPoint                                                          *
 *  Purpose:                                                                  *
 *      Converts a pixel (x, y) on the PPM to a point in space.               *
 *  Arguments:                                                                *
 *      x (unsigned int):                                                     *
 *          The x coordinate of the pixel.                                    *
 *      y (unsigned int):                                                     *
 *          The y coordinate of the pixel.                                    *
 *  Outputs:                                                                  *
 *      p (Vec3 *):                                                           *
 *          The corresponding point in space to the given pixel.              *
 *  Method:                                                                   *
 *      The point on the detector lies on the z = setup::z_src plane.         *
 *      Use this and convert the (x, y) components of the pixel to the        *
 *      (x, y) component on the detector.                                     *
 ******************************************************************************/
static inline Vec3 * pixelToPoint(unsigned int x, unsigned int y)
{
    const double xPt = start + pxFactor * (double)x;
    const double yPt = start + pyFactor * (double)y;
    return [[Vec3 alloc] init: xPt Y: yPt Z: zSrc];
}

/******************************************************************************
 *  Function:                                                                 *
 *      stop                                                                  *
 *  Purpose:                                                                  *
 *      Determines if a photon is still in motion for one black hole.         *
 *  Arguments:                                                                *
 *      v (const Vec3 * const):                                               *
 *          The vector corresponding to the given photon.                     *
 *  Outputs:                                                                  *
 *      halt (BOOL):                                                          *
 *          Boolean for if the photon is still moving.                        *
 ******************************************************************************/
static inline BOOL stop(const Vec3 * const v)
{
    /*  Case 1: The photon has reached the detector.                      */
    if ([v z] < zDetector)
        return YES;

    /*  Case 2: The black hole swallowed the photon.                      */
    else if ([v normSq] < blackHoleRadiusSq)
        return YES;

    /*  Otherwise, the photon is still moving. Don't stop.                */
    return NO;
}

/******************************************************************************
 *  Function:                                                                 *
 *      gravity                                                               *
 *  Purpose:                                                                  *
 *      Computes the acceleration given by the inverse square law from        *
 *      Newton's universal law of gravitation.                                *
 *  Arguments:                                                                *
 *      p (const nbh::vec3 &):                                                *
 *          The position vector of the particle.                              *
 *  Outputs:                                                                  *
 *      a (nbh::vec3):                                                        *
 *          The acceleration of the particle.                                 *
 ******************************************************************************/
static inline Vec3 * gravity(const Vec3 * const p)
{
    /*  Given a vector p, Newton's universal law of gravitation says the      *
     *  acceleration is proportional to p/||p||^3 = p_hat/||p||^2, where      *
     *  p_hat is the unit vector for p. We can compute p/||p||^3 in terms     *
     *  of the norm of p and the square of the norm of p. We have:            */
    const double factor = 1.0 / ([p normSq] * [p norm]);

    /*  The acceleration is the minus of p times this factor. The reason      *
     *  it is minus p is because gravity pulls inward, so the                 *
     *  acceleration is towards the blacks hole.                              */
    return [p times: -factor];
}
