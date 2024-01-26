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
 *      Provides a basic double-precision 3D vector class.                    *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2024/01/26                                                        *
 ******************************************************************************/
module nbh.vec3;

/*  Class for working with vectors in R^3. Uses double-precision values.      */
class Vec3 {

    private {
        /*  Following several C and C++ libraries, use a contiguous array for *
         *  the three Euclidean components.                                   */
        double[3] dat;
    }

    public {

        this() pure nothrow @safe @nogc
        {
            this.dat[0] = 0.0;
            this.dat[1] = 0.0;
            this.dat[2] = 0.0;
        }

        this(double x, double y, double z)
        pure nothrow @safe @nogc
        {
            this.dat[0] = x;
            this.dat[1] = y;
            this.dat[2] = z;
        }

        override string toString()
        pure @safe const
        {
            import std.string : format;
            return format("<%.4E, %.4E, %.4E>", dat[0], dat[1], dat[2]);
        }

        double x() pure nothrow @safe @nogc const
        {
            return this.dat[0];
        }

        double y() pure nothrow @safe @nogc const
        {
            return this.dat[1];
        }

        double z() pure nothrow @safe @nogc const
        {
            return this.dat[2];
        }

        double dot(const Vec3 rhs) pure nothrow @safe @nogc const
        {
            return this.dat[0]*rhs.dat[0] +
                   this.dat[1]*rhs.dat[1] +
                   this.dat[2]*rhs.dat[2];
        }

        double normSq() pure nothrow @safe @nogc const
        {
            return this.dot(this);
        }

        double norm() pure nothrow @safe @nogc const
        {
            import std.math : sqrt;
            return sqrt(this.normSq());
        }

        Vec3 dup() pure nothrow @safe const
        {
            Vec3 copy = new Vec3;
            copy.dat[0] = this.dat[0];
            copy.dat[1] = this.dat[1];
            copy.dat[2] = this.dat[2];
            return copy;
        }

        Vec3 opBinary(string op : "+")(const Vec3 rhs) pure nothrow @safe const
        {
            Vec3 sum = new Vec3;
            sum.dat[0] = this.dat[0] + rhs.dat[0];
            sum.dat[1] = this.dat[1] + rhs.dat[1];
            sum.dat[2] = this.dat[2] + rhs.dat[2];
            return sum;
        }

        Vec3 opBinary(string op : "-")(const Vec3 rhs) pure nothrow @safe const
        {
            Vec3 diff = new Vec3;
            diff.dat[0] = this.dat[0] - rhs.dat[0];
            diff.dat[1] = this.dat[1] - rhs.dat[1];
            diff.dat[2] = this.dat[2] - rhs.dat[2];
            return diff;
        }

        Vec3 opBinary(string op : "*")(double rhs) pure nothrow @safe const
        {
            Vec3 prod = new Vec3;
            prod.dat[0] = this.dat[0]*rhs;
            prod.dat[1] = this.dat[1]*rhs;
            prod.dat[2] = this.dat[2]*rhs;
            return prod;
        }

        Vec3 opBinaryRight(string op : "*")(double lhs) pure nothrow @safe const
        {
            Vec3 prod = new Vec3;
            prod.dat[0] = lhs*this.dat[0];
            prod.dat[1] = lhs*this.dat[1];
            prod.dat[2] = lhs*this.dat[2];
            return prod;
        }

        Vec3 opBinary(string op : "/")(double rhs) pure nothrow @safe const
        {
            Vec3 quot = new Vec3;
            immutable double rcpr = 1.0 / rhs;
            quot.dat[0] = this.dat[0]*rcpr;
            quot.dat[1] = this.dat[1]*rcpr;
            quot.dat[2] = this.dat[2]*rcpr;
            return quot;
        }

        void opOpAssign(string op : "+")(Vec3 rhs) pure nothrow @safe @nogc
        {
            this.dat[0] += rhs.dat[0];
            this.dat[1] += rhs.dat[1];
            this.dat[2] += rhs.dat[2];
        }

        void opOpAssign(string op : "-")(Vec3 rhs) pure nothrow @safe @nogc
        {
            this.dat[0] -= rhs.dat[0];
            this.dat[1] -= rhs.dat[1];
            this.dat[2] -= rhs.dat[2];
        }

        void opOpAssign(string op : "*")(double rhs) pure nothrow @safe @nogc
        {
            this.dat[0] *= rhs;
            this.dat[1] *= rhs;
            this.dat[2] *= rhs;
        }

        void opOpAssign(string op : "/")(double rhs) pure nothrow @safe @nogc
        {
            immutable double rcpr = 1.0 / rhs;
            this.dat[0] *= rcpr;
            this.dat[1] *= rcpr;
            this.dat[2] *= rcpr;
        }

        Vec3 cross(const Vec3 rhs) pure nothrow @safe const
        {
            Vec3 cross = new Vec3;
            cross.dat[0] = this.dat[1]*rhs.dat[2] - this.dat[2]*rhs.dat[1];
            cross.dat[1] = this.dat[2]*rhs.dat[0] - this.dat[0]*rhs.dat[2];
            cross.dat[2] = this.dat[0]*rhs.dat[1] - this.dat[1]*rhs.dat[0];
            return cross;
        }

        void crossWith(const Vec3 rhs) pure nothrow @safe @nogc
        {
            immutable double x = this.dat[0];
            immutable double y = this.dat[1];
            this.dat[0] = y*rhs.dat[2] - this.dat[2]*rhs.dat[1];
            this.dat[1] = this.dat[2]*rhs.dat[0] - x*rhs.dat[2];
            this.dat[2] = x*rhs.dat[1] - y*rhs.dat[0];
        }

        double rhoSq() pure nothrow @safe @nogc const
        {
            return this.dat[0]*this.dat[0] + this.dat[1]*this.dat[1];
        }

        double rho() pure nothrow @safe @nogc const
        {
            import std.math : sqrt;
            return sqrt(this.rhoSq);
        }

        Vec3 normal() pure nothrow @safe const
        {
            return this / this.norm;
        }

        void normalize() pure nothrow @safe @nogc
        {
            this /= this.norm;
        }

        Vec3 component(const Vec3 v) pure nothrow @safe const
        {
            immutable double factor = this.dot(v) / v.normSq;
            return v * factor;
        }

        Vec3 orthogonalComponent(const Vec3 v) pure nothrow @safe const
        {
            return this - this.component(v);
        }

        double project(const Vec3 v) pure nothrow @safe @nogc const
        {
            return this.dot(v) / v.norm;
        }

        double distSq(const Vec3 v) pure nothrow @safe @nogc const
        {
            immutable double dx = this.dat[0] - v.dat[0];
            immutable double dy = this.dat[1] - v.dat[1];
            immutable double dz = this.dat[2] - v.dat[2];
            return dx*dx + dy*dy + dz*dz;
        }

        double dist(const Vec3 v) pure nothrow @safe @nogc const
        {
            import std.math : sqrt;
            return sqrt(this.distSq(v));
        }
    }
}

/*  Vector addition unit test. Test for "u + v" for vectors u and v.          */
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 2, 3);
    Vec3 u = new Vec3(4, 5, 6);
    Vec3 sum = v + u;
    assert(sum.x == 5.0);
    assert(sum.y == 7.0);
    assert(sum.z == 9.0);
}

/*  Vector subtraction unit test. Test for "u - v" for vectors u and v.       */
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 2, 3);
    Vec3 u = new Vec3(4, 5, 6);
    Vec3 diff = u - v;
    assert(diff.x == 3.0);
    assert(diff.y == 3.0);
    assert(diff.z == 3.0);
}

/*  Scalar multiplication unit test. Test for "a * v" for vector v and real a.*/
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 2, 3);
    immutable double a = 2.0;
    Vec3 prod = a*v;
    assert(prod.x == 2.0);
    assert(prod.y == 4.0);
    assert(prod.z == 6.0);
}

/*  Scalar multiplication unit test. Test for "v * a" for vector v and real a.*/
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 2, 3);
    immutable double a = 2.0;
    Vec3 prod = v*a;
    assert(prod.x == 2.0);
    assert(prod.y == 4.0);
    assert(prod.z == 6.0);
}

/*  Scalar division unit test. Test for "v / a" for vector v and real a.      */
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(2, 4, 6);
    immutable double a = 2.0;
    Vec3 prod = v/a;
    assert(prod.x == 1.0);
    assert(prod.y == 2.0);
    assert(prod.z == 3.0);
}

/*  Tests for op assignment for vector addition. "v += u" for vectors v, u.   */
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 2, 3);
    Vec3 u = new Vec3(4, 5, 6);
    v += u;
    assert(v.x == 5.0);
    assert(v.y == 7.0);
    assert(v.z == 9.0);
}

/*  Tests for op assignment for vector subtraction. "u -= v" for vectors u, v.*/
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 2, 3);
    Vec3 u = new Vec3(4, 5, 6);
    u -= v;
    assert(u.x == 3.0);
    assert(u.y == 3.0);
    assert(u.z == 3.0);
}

/*  Tests for op assignment for scalar multiplication. "v *= a".              */
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 2, 3);
    immutable double a = 2.0;
    v *= a;
    assert(v.x == 2.0);
    assert(v.y == 4.0);
    assert(v.z == 6.0);
}

/*  Tests for op assignment for scalar division. "v /= a".                    */
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(2, 4, 6);
    immutable double a = 2.0;
    v /= a;
    assert(v.x == 1.0);
    assert(v.y == 2.0);
    assert(v.z == 3.0);
}

/*  Test for the projection functions x(), y(), and z().                      */
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 2, 3);
    assert(v.x == 1.0);
    assert(v.y == 2.0);
    assert(v.z == 3.0);
}

/*  Test for the Euclidean dot product. This is the "dot" member function.    */
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 2, 3);
    Vec3 w = new Vec3(4, 5, 6);
    immutable double dot = v.dot(w);
    immutable double ans = 32.0;
    assert(dot == ans);
}

/*  This tests the square of the Euclidean norm. Computes ||v||^2.            */
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 2, 3);
    immutable double nsq = v.normSq;
    immutable double ans = 14.0;
    assert(nsq == ans);
}

/*  Tests the Euclidean norm, ||v||, for a vector v.                          */
pure nothrow @safe unittest
{
    import std.math : fabs;
    Vec3 v = new Vec3(1, 2, 3);
    immutable double norm = v.norm;
    immutable double ans = 3.7416573867739413;
    immutable double err = fabs(norm - ans);

    /*  We probably won't get exact due to floating point round-off.          */
    immutable double eps = 2.0 * ans.epsilon;
    assert(err < eps);
}

/*  Test for the cross product. z = x cross y using the right hand rule.      */
pure nothrow @safe unittest
{
    Vec3 x = new Vec3(1, 0, 0);
    Vec3 y = new Vec3(0, 1, 0);
    Vec3 z = x.cross(y);
    assert(z.x == 0.0);
    assert(z.y == 0.0);
    assert(z.z == 1.0);
}

/*  Test for the cross product. z = x cross y using the right hand rule.      */
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 0, 0);
    Vec3 y = new Vec3(0, 1, 0);
    v.crossWith(y);
    assert(v.x == 0.0);
    assert(v.y == 0.0);
    assert(v.z == 1.0);
}

/*  Unit test for the azimuthal component of a vector.                        */
pure nothrow @safe unittest
{
    import std.math : fabs;
    Vec3 v = new Vec3(1, 2, 3);
    immutable double rho = v.rho;
    immutable double ans = 2.23606797749979;
    immutable double err = fabs(rho - ans);
    immutable double eps = 2.0 * ans.epsilon;
    assert(err < eps);
}

/*  Unit test for the square of the azimuthal component of a vector.          */
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 2, 3);
    immutable double rsq = v.rhoSq;
    immutable double ans = 5.0;
    assert(rsq == ans);
}

/*  Test for computing the unit normal of a vector. Magnitude should be 1.    */
pure nothrow @safe unittest
{
    import std.math : fabs;
    Vec3 v = new Vec3(1, 2, 3);
    Vec3 u = v.normal;
    immutable double ans = 1.0;
    immutable double err = fabs(u.norm - 1.0);
    immutable double eps = 2.0 * ans.epsilon;
    assert(err < eps);
}

/*  Test for normalizing a vector and storing the result in "this."           */
pure nothrow @safe unittest
{
    import std.math : fabs;
    Vec3 v = new Vec3(1, 2, 3);
    v.normalize;
    immutable double ans = 1.0;
    immutable double err = fabs(v.norm - 1.0);
    immutable double eps = 2.0 * ans.epsilon;
    assert(err < eps);
}

/*  Test for vector component function, or the vector projection function.    */
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 1, 1);
    Vec3 x = new Vec3(1, 0, 0);
    Vec3 proj = v.component(x);
    assert(proj.x == 1.0);
    assert(proj.y == 0.0);
    assert(proj.z == 0.0);
}

/*  Test for the square of the Euclidean distance between two vectors.        */
pure nothrow @safe unittest
{
    Vec3 v = new Vec3(1, 2, 3);
    Vec3 u = new Vec3(4, 5, 6);
    immutable double ans = 27.0;
    immutable double dsq = v.distSq(u);
    assert(dsq == ans);
}

/*  Test for the Euclidean distance between two vectors.                      */
pure nothrow @safe unittest
{
    import std.math : fabs;
    Vec3 v = new Vec3(1, 2, 3);
    Vec3 u = new Vec3(4, 5, 6);
    immutable double ans = 5.196152422706632;
    immutable double dst = v.dist(u);
    immutable double err = fabs(ans - dst);
    immutable double eps = 2.0 * ans.epsilon;
    assert(err < eps);
}

/*  Test for the scalar projection function.                                  */
pure nothrow @safe unittest
{
    import std.math : fabs;
    Vec3 v = new Vec3(1, 2, 3);
    Vec3 y = new Vec3(0, 1, 0);
    immutable double proj = v.project(y);
    immutable double ans = 2.0;
    immutable double err = fabs(proj - ans);
    immutable double eps = 2.0 * ans.epsilon;
    assert(err < eps);
}

/*  Test for the orthogonal projection function.                              */
pure nothrow @safe unittest
{
    import std.math : fabs;
    Vec3 v = new Vec3(1, 2, 3);
    Vec3 u = new Vec3(4, 5, 6);
    Vec3 comp = v.component(u);
    Vec3 orth = v.orthogonalComponent(u);
    Vec3 sum = comp + orth;
    immutable double eps = 2.0 * double.epsilon;
    immutable double dx = fabs(v.x - sum.x);
    immutable double dy = fabs(v.y - sum.y);
    immutable double dz = fabs(v.z - sum.z);
    immutable double err = fabs(orth.dot(u) / u.norm);
    assert(dx < eps);
    assert(dy < eps);
    assert(dz < eps);
    assert(err < eps);
}
