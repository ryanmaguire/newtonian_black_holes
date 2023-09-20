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
 *      Provides a struct for using colors.                                   *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2023/02/28                                                        *
 ******************************************************************************/

/* Include guard to prevent including this file twice.                        */
#ifndef NBH_COLOR_HPP
#define NBH_COLOR_HPP

/*  File data type found here.                                                */
#include <cstdio>

/*  6D vector struct given here.                                              */
#include "nbh_vec6.hpp"

/*  Struct for working with PPM files found here.                             */
#include "nbh_ppm.hpp"

/*  Multiples of pi provides here.                                            */
#include "nbh_pi.hpp"

/*  Namespace for the mini-project. "Newtonian Black Holes."                  */
namespace nbh {

    /*  Simple struct for working with colors in a PPM file.                  */
    struct color {

        /*  A color is given by its RGB values.                               */
        unsigned char red, green, blue;

        /*  Empty constructor.                                                */
        color(void);

        /**********************************************************************
         *  Constructor:                                                      *
         *      nbh::color                                                    *
         *  Purpose:                                                          *
         *      Creates a color struct from three unsigned values.            *
         *  Arguments:                                                        *
         *      r (unsigned char):                                            *
         *          The red component of the color.                           *
         *      g (unsigned char):                                            *
         *          The green component of the color.                         *
         *      b (unsigned char):                                            *
         *          The blue component of the color.                          *
         *  Outputs:                                                          *
         *      c (nbh::color):                                               *
         *          The color (r, g, b) in 24-bit RGB color space.            *
         **********************************************************************/
        color(unsigned char r, unsigned char g, unsigned char b);

        /**********************************************************************
         *  Method:                                                           *
         *      nbh::write                                                    *
         *  Purpose:                                                          *
         *      Writes a color to a FILE.                                     *
         *  Arguments:                                                        *
         *      fp (FILE *):                                                  *
         *          A pointer to a file, the file the color is written to.    *
         *  Outputs:                                                          *
         *      None (void).                                                  *
         **********************************************************************/
        inline void write(FILE *fp) const;

        /**********************************************************************
         *  Method:                                                           *
         *      nbh::write                                                    *
         *  Purpose:                                                          *
         *      Writes a color to a PPM file.                                 *
         *  Arguments:                                                        *
         *      PPM (nbh::ppm &):                                             *
         *          A reference to a PPM file that has been initialized.      *
         *  Outputs:                                                          *
         *      None (void).                                                  *
         **********************************************************************/
        inline void write(nbh::ppm &PPM) const;

        /**********************************************************************
         *  Operator:                                                         *
         *      *                                                             *
         *  Purpose:                                                          *
         *      Scales the intensity of a color by a real number.             *
         *  Arguments:                                                        *
         *      t (double):                                                   *
         *          A real number, usually between 0 an 1.                    *
         *  Outputs:                                                          *
         *      tc (nbh::color).                                              *
         *          The color *this* with each color channel scaled by t.     *
         **********************************************************************/
        inline color operator * (double t) const;

        /**********************************************************************
         *  Operator:                                                         *
         *      *=                                                            *
         *  Purpose:                                                          *
         *      Scales the intensity of a color by a real number.             *
         *  Arguments:                                                        *
         *      t (double):                                                   *
         *          A real number, usually between 0 an 1.                    *
         *  Outputs:                                                          *
         *      None (void).                                                  *
         **********************************************************************/
        inline void operator *= (double t);

        /**********************************************************************
         *  Operator:                                                         *
         *      +                                                             *
         *  Purpose:                                                          *
         *      Mixes two colors together by averaging over their components. *
         *  Arguments:                                                        *
         *      c (const nbh::color &c):                                      *
         *          A color.                                                  *
         *  Outputs:                                                          *
         *      mix (nbh::color).                                             *
         *          The average of the color c and *this*.                    *
         **********************************************************************/
        inline color operator + (const nbh::color &c) const;

        /**********************************************************************
         *  Operator:                                                         *
         *      +=                                                            *
         *  Purpose:                                                          *
         *      Mixes two colors together by averaging over their components. *
         *  Arguments:                                                        *
         *      c (const nbh::color &c):                                      *
         *          A color.                                                  *
         *  Outputs:                                                          *
         *      None (void).                                                  *
         **********************************************************************/
        inline void operator += (const nbh::color &c);
    };

    /*  Empty constructor, just return.                                       */
    color::color(void)
    {
        return;
    }

    /**************************************************************************
     *  Constructor:                                                          *
     *      nbh::color                                                        *
     *  Purpose:                                                              *
     *      Creates a color struct from three unsigned values.                *
     *  Arguments:                                                            *
     *      r (unsigned char):                                                *
     *          The red component of the color.                               *
     *      g (unsigned char):                                                *
     *          The green component of the color.                             *
     *      b (unsigned char):                                                *
     *          The blue component of the color.                              *
     *  Outputs:                                                              *
     *      c (nbh::color):                                                   *
     *          The color (r, g, b) in 24-bit RGB color space.                *
     **************************************************************************/
    color::color(unsigned char r, unsigned char g, unsigned char b)
    {
        red = r;
        green = g;
        blue = b;
    }

    /**************************************************************************
     *  Method:                                                               *
     *      nbh::write                                                        *
     *  Purpose:                                                              *
     *      Writes a color to a FILE.                                         *
     *  Arguments:                                                            *
     *      fp (FILE *):                                                      *
     *          A pointer to a file, the file the color is written to.        *
     *  Outputs:                                                              *
     *      None (void).                                                      *
     **************************************************************************/
    inline void color::write(FILE *fp) const
    {
        std::fputc(int(red), fp);
        std::fputc(int(green), fp);
        std::fputc(int(blue), fp);
    }

    /**************************************************************************
     *  Method:                                                               *
     *      nbh::write                                                        *
     *  Purpose:                                                              *
     *      Writes a color to a PPM file.                                     *
     *  Arguments:                                                            *
     *      PPM (nbh::ppm &):                                                 *
     *          A reference to a PPM file that has been initialized.          *
     *  Outputs:                                                              *
     *      None (void).                                                      *
     **************************************************************************/
    inline void color::write(nbh::ppm &PPM) const
    {
        write(PPM.fp);
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      *                                                                 *
     *  Purpose:                                                              *
     *      Scales the intensity of a color by a real number.                 *
     *  Arguments:                                                            *
     *      t (double):                                                       *
     *          A real number, usually between 0 an 1.                        *
     *  Outputs:                                                              *
     *      tc (nbh::color).                                                  *
     *          The color *this* with each color channel scaled by t.         *
     *  Method:                                                               *
     *      Scale each color channel by t and then cast to unsigned char.     *
     *  Notes:                                                                *
     *      No checks for overflow are performed. Large or negative values of *
     *      t may yield unexpected results.                                   *
     **************************************************************************/
    inline color color::operator * (double t) const
    {
        const unsigned char r = static_cast<unsigned char>(t*red);
        const unsigned char g = static_cast<unsigned char>(t*green);
        const unsigned char b = static_cast<unsigned char>(t*blue);
        return color(r, g, b);
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      *=                                                                *
     *  Purpose:                                                              *
     *      Scales the intensity of a color by a real number.                 *
     *  Arguments:                                                            *
     *      t (double):                                                       *
     *          A real number, usually between 0 an 1.                        *
     *  Outputs:                                                              *
     *      None (void).                                                      *
     *  Method:                                                               *
     *      Scale each color channel by t and then cast to unsigned char. The *
     *      end result is then stored in *this*.                              *
     *  Notes:                                                                *
     *      No checks for overflow are performed. Large or negative values of *
     *      t may yield unexpected results.                                   *
     **************************************************************************/
    inline void color::operator *= (double t)
    {
        red = static_cast<unsigned char>(t*red);
        green = static_cast<unsigned char>(t*green);
        blue = static_cast<unsigned char>(t*blue);
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      +                                                                 *
     *  Purpose:                                                              *
     *      Mixes two colors together by averaging over their components.     *
     *  Arguments:                                                            *
     *      c (const nbh::color &c):                                          *
     *          A color.                                                      *
     *  Outputs:                                                              *
     *      mix (nbh::color).                                                 *
     *          The average of the color c and *this*.                        *
     *  Method:                                                               *
     *      Cast each channel to double to avoid overflow. Then average       *
     *      the values together and cast back to unsigned char.               *
     **************************************************************************/
    inline color color::operator + (const nbh::color &c) const
    {
        /*  Cast the values to doubles and take the average, component-wise.  */
        const double x = 0.5 * (double(red) + double(c.red));
        const double y = 0.5 * (double(green) + double(c.green));
        const double z = 0.5 * (double(blue) + double(c.blue));

        /*  Cast the double back to unsigned char's and return.               */
        const unsigned char r = static_cast<unsigned char>(x);
        const unsigned char g = static_cast<unsigned char>(y);
        const unsigned char b = static_cast<unsigned char>(z);
        return color(r, g, b);
    }

    /**************************************************************************
     *  Operator:                                                             *
     *      +=                                                                *
     *  Purpose:                                                              *
     *      Mixes two colors together by averaging over their components.     *
     *  Arguments:                                                            *
     *      c (const nbh::color &c):                                          *
     *          A color.                                                      *
     *  Outputs:                                                              *
     *      None (void).                                                      *
     *  Method:                                                               *
     *      Average over each channel and store the result in *this*.         *
     **************************************************************************/
    inline void color::operator += (const nbh::color &c)
    {
        /*  Cast the values to doubles and take the average, component-wise.  */
        const double x = 0.5 * (double(red) + double(c.red));
        const double y = 0.5 * (double(green) + double(c.green));
        const double z = 0.5 * (double(blue) + double(c.blue));

        /*  Cast the double back to unsigned char's and return.               */
        red = static_cast<unsigned char>(x);
        green = static_cast<unsigned char>(y);
        blue = static_cast<unsigned char>(z);
    }

    /*  Constant colors that are worth having.                                */
    namespace colors {
        inline color white(void)
        {
            return color(0xFFU, 0xFFU, 0xFFU);
        }

        inline color black(void)
        {
            return color(0x00U, 0x00U, 0x00U);
        }

        inline color red(void)
        {
            return color(0xFFU, 0x00U, 0x00U);
        }

        inline color green(void)
        {
            return color(0x00U, 0xFFU, 0x00U);
        }

        inline color blue(void)
        {
            return color(0x00U, 0x00U, 0xFFU);
        }

        inline color yellow(void)
        {
            return color(0xFFU, 0xFFU, 0x00U);
        }
    }
    /*  End of namespace "colors".                                            */

    /**************************************************************************
     *  Function:                                                             *
     *      nbh::checker_board                                                *
     *  Purpose:                                                              *
     *      Creates a checker-board pattern on the detector.                  *
     *  Arguments:                                                            *
     *      u (const nbh::vec6 &):                                            *
     *          The position and velocity of the particle.                    *
     *  Outputs:                                                              *
     *      c (nbh::color):                                                   *
     *          The color given on the detector.                              *
     **************************************************************************/
    inline color checker_board(const nbh::vec6 &u)
    {
        /*  Factor for darkening the checker board.                           */
        const double cfact = nbh::setup::z_detector_sq / u.p.normsq();

        /*  If the photon didn't make it, color the pixel black.              */
        if (u.p.z > nbh::setup::z_detector)
            return nbh::colors::black();

        /*  Otherwise use a bit-wise trick to color the plane.                */
        else if (static_cast<unsigned>(std::ceil(u.p.x)+std::ceil(u.p.y)) & 1U)
            return colors::white() * cfact;

        return colors::red() * cfact;
    }

    /**************************************************************************
     *  Function:                                                             *
     *      nbh::bright_checker_board                                         *
     *  Purpose:                                                              *
     *      Creates a brighter checker-board pattern on the detector.         *
     *  Arguments:                                                            *
     *      u (const nbh::vec6 &):                                            *
     *          The position and velocity of the particle.                    *
     *  Outputs:                                                              *
     *      c (nbh::color):                                                   *
     *          The color given on the detector.                              *
     **************************************************************************/
    inline color bright_checker_board(const nbh::vec6 &u)
    {
        /*  Factor for darkening the checker board.                           */
        const double cfact = 0.5*(nbh::setup::z_detector_sq/u.p.normsq() + 1.0);

        /*  If the photon didn't make it, color the pixel black.              */
        if (u.p.z > nbh::setup::z_detector)
            return nbh::colors::black();

        /*  Otherwise use a bit-wise trick to color the plane.                */
        else if (static_cast<unsigned>(std::ceil(u.p.x)+std::ceil(u.p.y)) & 1U)
            return colors::white() * cfact;

        return colors::red() * cfact;
    }

    /**************************************************************************
     *  Function:                                                             *
     *      nbh::checker_board_four                                           *
     *  Purpose:                                                              *
     *      Creates a checker-board pattern on the detector with four colors. *
     *  Arguments:                                                            *
     *      u (const nbh::vec6 &):                                            *
     *          The position and velocity of the particle.                    *
     *  Outputs:                                                              *
     *      c (nbh::color):                                                   *
     *          The color given on the detector.                              *
     **************************************************************************/
    inline color checker_board_four(const nbh::vec6 &u)
    {
        /*  Factor for darkening the checker board.                           */
        const double cfact = nbh::setup::z_detector_sq / u.p.normsq();

        /*  Integers that determines the color.                               */
        const unsigned int nx = static_cast<unsigned>(std::ceil(u.p.x)) & 1U;
        const unsigned int ny = static_cast<unsigned>(std::ceil(u.p.y)) & 1U;
        const unsigned int n = nx + (ny << 1U);

        /*  If the photon didn't make it, color the pixel black.              */
        if (u.p.z > nbh::setup::z_detector)
            return nbh::colors::black();

        /*  Otherwise use a bit-wise trick to color the plane.                */
        switch (n)
        {
            case 0:
                return colors::white() * cfact;
            case 1:
                return colors::yellow() * cfact;
            case 2:
                return colors::green() * cfact;
            default:
                return colors::red() * cfact;
        }
    }

    /**************************************************************************
     *  Function:                                                             *
     *      nbh::checker_board_four_highlight                                 *
     *  Purpose:                                                              *
     *      Creates a checker-board pattern using four colors and             *
     *      highlights the origin blue.                                       *
     *  Arguments:                                                            *
     *      u (const nbh::vec6 &):                                            *
     *          The position and velocity of the particle.                    *
     *  Outputs:                                                              *
     *      c (nbh::color):                                                   *
     *          The color given on the detector.                              *
     **************************************************************************/
    inline color checker_board_four_highlight(const nbh::vec6 &u)
    {
        /*  Factor for darkening the checker board.                           */
        const double cfact = nbh::setup::z_detector_sq / u.p.normsq();

        /*  Integers that determines the color.                               */
        const unsigned int nx = static_cast<unsigned>(std::ceil(u.p.x)) & 1U;
        const unsigned int ny = static_cast<unsigned>(std::ceil(u.p.y)) & 1U;
        const unsigned int n = nx + (ny << 1U);

        /*  If the photon didn't make it, color the pixel black.              */
        if (u.p.z > nbh::setup::z_detector)
            return nbh::colors::black();

        /*  If the center of the plane was hit, color blue.                   */
        else if (u.p.rhosq() < nbh::setup::highlight_threshold)
            return nbh::colors::blue();

        /*  Otherwise use a bit-wise trick to color the plane.                */
        switch (n)
        {
            case 0:
                return colors::white() * cfact;
            case 1:
                return colors::yellow() * cfact;
            case 2:
                return colors::green() * cfact;
            default:
                return colors::red() * cfact;
        }
    }

    /**************************************************************************
     *  Function:                                                             *
     *      nbh::checker_board_highlight                                      *
     *  Purpose:                                                              *
     *      Creates a checker-board pattern and highlights the origin blue.   *
     *  Arguments:                                                            *
     *      u (const nbh::vec6 &):                                            *
     *          The position and velocity of the particle.                    *
     *  Outputs:                                                              *
     *      c (nbh::color):                                                   *
     *          The color given on the detector.                              *
     **************************************************************************/
    inline color checker_board_highlight(const nbh::vec6 &u)
    {
        /*  Factor for darkening the checker board.                           */
        const double color_factor = nbh::setup::z_detector_sq / u.p.normsq();

        /*  If the photon didn't make it, color the pixel black.              */
        if (u.p.z > nbh::setup::z_detector)
            return nbh::colors::black();

        /*  If the center of the plane was hit, color blue.                   */
        else if (u.p.rhosq() < nbh::setup::highlight_threshold)
            return nbh::colors::blue();

        /*  Otherwise use a bit-wise trick to color the plane.                */
        else if (static_cast<unsigned>(std::ceil(u.p.x)+std::ceil(u.p.y)) & 1U)
            return colors::white() * color_factor;

        return colors::red() * color_factor;
    }

    /**************************************************************************
     *  Function:                                                             *
     *      nbh::angle_gradient                                               *
     *  Purpose:                                                              *
     *      Creates a rainbow gradient of color based on the angle the        *
     *      velocity vector of the particle makes with the detector on impact.*
     *  Arguments:                                                            *
     *      u (const nbh::vec6 &):                                            *
     *          The position and velocity of the particle as it hit           *
     *          the detector.                                                 *
     *  Outputs:                                                              *
     *      c (nbh::color):                                                   *
     *          The color given on the detector.                              *
     **************************************************************************/
    inline color angle_gradient(const nbh::vec6 &u)
    {
        /*  Declare unsigned char's for computing the output color.           */
        unsigned char red, green, blue;

        /*  We want the zenith angle of the velocity vector. This can be      *
         *  computed using the cylindrical coordinates of the vector.         */
        const double angle = std::atan2(std::fabs(u.v.z), u.v.rho());

        /*  Scale the angle so that it falls between 0 and 255.               */
        const double scaled = 255.0 * angle / NBH_HALF_PI;

        /*  Use an RGB rainbow gradient to color the current pixel. We'll set *
         *  blue to correspond to the least value and red for the greatest,   *
         *  with a continuous gradient in between. First blue to cyan.        */
        if (scaled < 64.0)
        {
            red = 0x00U;
            green = static_cast<unsigned char>(4.0*scaled);
            blue = 0xFFU;
        }

        /*  Cyan to green.                                                    */
        else if (scaled < 128.0)
        {
            red = 0x00U;
            green = 0xFFU;
            blue = static_cast<unsigned char>(255.0 - 4.0*(scaled - 64.0));
        }

        /*  Green to yellow.                                                  */
        else if (scaled < 192.0)
        {
            red = static_cast<unsigned char>(4.0*(scaled - 128.0));
            green = 0xFFU;
            blue = 0x00U;
        }

        /*  Yellow to red.                                                    */
        else if (scaled < 255.0)
        {
            red = 0xFFU;
            green = static_cast<unsigned char>(255.0 - 4.0*(scaled - 192.0));
            blue = 0x00U;
        }

        /*  And, lastly, red.                                                 */
        else
        {
            red = 0xFFU;
            green = 0x00U;
            blue = 0x00U;
        }

        return color(red, green, blue);
    }

    /**************************************************************************
     *  Function:                                                             *
     *      nbh::color_gradient_checkerboard                                  *
     *  Purpose:                                                              *
     *      Creates a checker-board pattern on the detector and adds the      *
     *      rainbow gradient defined in nbh_angle_gradient.                   *
     *  Arguments:                                                            *
     *      u (const nbh::vec6 &):                                            *
     *          The position and velocity of the particle as it               *
     *          hit the detector.                                             *
     *  Outputs:                                                              *
     *      c (nbh::color):                                                   *
     *          The color given on the detector.                              *
     **************************************************************************/
    inline color color_gradient_checkerboard(const nbh::vec6 &u)
    {
        /*  If the photon didn't make it, color the pixel black.              */
        if (u.p.z > nbh::setup::z_detector)
            return nbh::colors::black();

        /*  Take the average of the checkerboard and the raindbow gradient.   */
        return bright_checker_board(u) + angle_gradient(u);
    }
}
/*  End of "nbh" namespace.                                                   */

#endif
/*  End of include guard.                                                     */
