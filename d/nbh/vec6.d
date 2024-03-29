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
 *      Provides a basic double-precision 6D vector class.                    *
 ******************************************************************************
 *  Author: Ryan Maguire                                                      *
 *  Date:   2024/01/26                                                        *
 ******************************************************************************/
module nbh.vec6;
private import nbh.vec3 : Vec3;

/*  Class for working with vectors in R^3. Uses double-precision values.      */
struct Vec6 {

    private {
        /*  Using phase-space, a 6D vector is two 3D vectors.                 */
        double[6] dat;
    }

    public {

        this(const Vec3 pos, const Vec3 vel)
        pure nothrow @safe @nogc
        {
            this.dat[0] = pos.x;
            this.dat[1] = pos.y;
            this.dat[2] = pos.z;

            this.dat[3] = vel.x;
            this.dat[4] = vel.y;
            this.dat[5] = vel.z;
        }

        this(double px, double py, double pz, double vx, double vy, double vz)
        pure nothrow @safe @nogc
        {
            this.dat[0] = px;
            this.dat[1] = py;
            this.dat[2] = pz;

            this.dat[3] = vx;
            this.dat[4] = vy;
            this.dat[5] = vz;
        }

        string toString()
        pure @safe const
        {
            import std.string : format;
            return format("<%.4E, %.4E, %.4E, %.4E, %.4E, %.4E>",
                          dat[0], dat[1], dat[2], dat[3], dat[4], dat[5]);
        }

        void pos(const Vec3 p)
        pure nothrow @safe @nogc
        {
            this.dat[0] = p.x;
            this.dat[1] = p.y;
            this.dat[2] = p.z;
        }

        void vel(const Vec3 v)
        pure nothrow @safe @nogc
        {
            this.dat[3] = v.x;
            this.dat[4] = v.y;
            this.dat[5] = v.z;
        }

        double px()
        pure nothrow @safe @nogc const
        {
            return this.dat[0];
        }

        double py()
        pure nothrow @safe @nogc const
        {
            return this.dat[1];
        }

        double pz()
        pure nothrow @safe @nogc const
        {
            return this.dat[2];
        }

        double vx()
        pure nothrow @safe @nogc const
        {
            return this.dat[3];
        }

        double vy()
        pure nothrow @safe @nogc const
        {
            return this.dat[4];
        }

        double vz()
        pure nothrow @safe @nogc const
        {
            return this.dat[5];
        }

        void px(double x)
        pure nothrow @safe @nogc
        {
            this.dat[0] = x;
        }

        void py(double y)
        pure nothrow @safe @nogc
        {
            this.dat[1] = y;
        }

        void pz(double z)
        pure nothrow @safe @nogc
        {
            this.dat[2] = z;
        }

        void vx(double x)
        pure nothrow @safe @nogc
        {
            this.dat[3] = x;
        }

        void vy(double y)
        pure nothrow @safe @nogc
        {
            this.dat[4] = y;
        }

        void vz(double z)
        pure nothrow @safe @nogc
        {
            this.dat[5] = z;
        }

        Vec3 pos()
        pure nothrow @safe @nogc const
        {
            return Vec3(this.dat[0], this.dat[1], this.dat[2]);
        }

        Vec3 vel()
        pure nothrow @safe @nogc const
        {
            return Vec3(this.dat[3], this.dat[4], this.dat[5]);
        }

        double dot(const Vec6 u)
        pure nothrow @safe @nogc const
        {
            return this.dat[0]*u.dat[0] +
                   this.dat[1]*u.dat[1] +
                   this.dat[2]*u.dat[2] +
                   this.dat[3]*u.dat[3] +
                   this.dat[4]*u.dat[4] +
                   this.dat[5]*u.dat[5];
        }

        double normSq()
        pure nothrow @safe @nogc const
        {
            return this.dot(this);
        }

        double norm()
        pure nothrow @safe @nogc const
        {
            import std.math : sqrt;
            return sqrt(this.normSq);
        }

        Vec6 opBinary(string op : "+")(const Vec6 u)
        pure nothrow @safe @nogc const
        {
            Vec6 sum;
            sum.px = this.dat[0] + u.dat[0];
            sum.py = this.dat[1] + u.dat[1];
            sum.pz = this.dat[2] + u.dat[2];
            sum.vx = this.dat[3] + u.dat[3];
            sum.vy = this.dat[4] + u.dat[4];
            sum.vz = this.dat[5] + u.dat[5];
            return sum;
        }

        void opOpAssign(string op : "+")(const Vec6 u)
        pure nothrow @safe @nogc
        {
            this.dat[0] += u.dat[0];
            this.dat[1] += u.dat[1];
            this.dat[2] += u.dat[2];
            this.dat[3] += u.dat[3];
            this.dat[4] += u.dat[4];
            this.dat[5] += u.dat[5];
        }

        Vec6 opBinary(string op : "-")(const Vec6 u)
        pure nothrow @safe @nogc const
        {
            Vec6 diff;
            diff.px = this.dat[0] - u.dat[0];
            diff.py = this.dat[1] - u.dat[1];
            diff.pz = this.dat[2] - u.dat[2];
            diff.vx = this.dat[3] - u.dat[3];
            diff.vy = this.dat[4] - u.dat[4];
            diff.vz = this.dat[5] - u.dat[5];
            return diff;
        }

        void opOpAssign(string op : "-")(const Vec6 u)
        pure nothrow @safe @nogc
        {
            this.dat[0] -= u.dat[0];
            this.dat[1] -= u.dat[1];
            this.dat[2] -= u.dat[2];
            this.dat[3] -= u.dat[3];
            this.dat[4] -= u.dat[4];
            this.dat[5] -= u.dat[5];
        }

        Vec6 opBinary(string op : "*")(double rhs)
        pure nothrow @safe @nogc const
        {
            Vec6 prod;
            prod.px = this.dat[0] * rhs;
            prod.py = this.dat[1] * rhs;
            prod.pz = this.dat[2] * rhs;
            prod.vx = this.dat[3] * rhs;
            prod.vy = this.dat[4] * rhs;
            prod.vz = this.dat[5] * rhs;
            return prod;
        }

        Vec6 opBinaryRight(string op : "*")(double lhs)
        pure nothrow @safe @nogc const
        {
            Vec6 prod;
            prod.px = lhs * this.dat[0];
            prod.py = lhs * this.dat[1];
            prod.pz = lhs * this.dat[2];
            prod.vx = lhs * this.dat[3];
            prod.vy = lhs * this.dat[4];
            prod.vz = lhs * this.dat[5];
            return prod;
        }

        void opOpAssign(string op : "*")(double rhs)
        pure nothrow @safe @nogc
        {
            this.dat[0] *= rhs;
            this.dat[1] *= rhs;
            this.dat[2] *= rhs;
            this.dat[3] *= rhs;
            this.dat[4] *= rhs;
            this.dat[5] *= rhs;
        }

        Vec6 opBinary(string op : "/")(double rhs)
        pure nothrow @safe @nogc const
        {
            Vec6 quot;
            immutable double rcpr = 1.0 / rhs;
            quot.px = this.dat[0] * rcpr;
            quot.py = this.dat[1] * rcpr;
            quot.pz = this.dat[2] * rcpr;
            quot.vx = this.dat[3] * rcpr;
            quot.vy = this.dat[4] * rcpr;
            quot.vz = this.dat[5] * rcpr;
            return quot;
        }

        void opOpAssign(string op : "/")(double rhs)
        pure nothrow @safe @nogc
        {
            immutable double rcpr = 1.0 / rhs;
            this.dat[0] *= rcpr;
            this.dat[1] *= rcpr;
            this.dat[2] *= rcpr;
            this.dat[3] *= rcpr;
            this.dat[4] *= rcpr;
            this.dat[5] *= rcpr;
        }
    }
}

pure nothrow @safe unittest
{
    Vec3 p = Vec3(1, 2, 3);
    Vec3 v = Vec3(4, 5, 6);
    Vec6 u = Vec6(p, v);
    immutable double nsq = u.normSq;
    immutable double ans = 91.0;
    assert(nsq == ans);
}

pure nothrow @safe unittest
{
    import std.math : fabs;
    Vec3 p = Vec3(1, 2, 3);
    Vec3 v = Vec3(4, 5, 6);
    Vec6 u = Vec6(p, v);
    immutable double nrm = u.norm;
    immutable double ans = 9.539392014169456;
    immutable double err = fabs((ans - nrm) / ans);
    immutable double eps = 2.0 * ans.epsilon;
    assert(err < eps);
}

pure nothrow @safe unittest
{
    Vec3 p = Vec3(1, 2, 3);
    Vec3 v = Vec3(4, 5, 6);
    Vec6 u = Vec6(p, v);
    assert(u.px == p.x);
    assert(u.py == p.y);
    assert(u.pz == p.z);
    assert(u.vx == v.x);
    assert(u.vy == v.y);
    assert(u.vz == v.z);
}

pure nothrow @safe unittest
{
    Vec3 p = Vec3(1, 2, 3);
    Vec3 v = Vec3(4, 5, 6);
    Vec6 u = Vec6(p, v);
    Vec3 pos = u.pos;
    Vec3 vel = u.vel;
    assert(pos.x == p.x);
    assert(pos.y == p.y);
    assert(pos.z == p.z);
    assert(vel.x == v.x);
    assert(vel.y == v.y);
    assert(vel.z == v.z);
}

pure nothrow @safe unittest
{
    Vec6 u0 = Vec6(1, 2, 3, 4, 5, 6);
    Vec6 u1 = Vec6(2, 3, 4, 5, 6, 7);
    Vec6 sum = u0 + u1;
    assert(sum.px == 3);
    assert(sum.py == 5);
    assert(sum.pz == 7);
    assert(sum.vx == 9);
    assert(sum.vy == 11);
    assert(sum.vz == 13);
}

pure nothrow @safe unittest
{
    Vec6 u0 = Vec6(1, 2, 3, 4, 5, 6);
    Vec6 u1 = Vec6(2, 3, 4, 5, 6, 7);
    Vec6 diff = u1 - u0;
    assert(diff.px == 1);
    assert(diff.py == 1);
    assert(diff.pz == 1);
    assert(diff.vx == 1);
    assert(diff.vy == 1);
    assert(diff.vz == 1);
}

pure nothrow @safe unittest
{
    Vec6 u = Vec6(1, 2, 3, 4, 5, 6);
    Vec6 prod = 2.0 * u;
    assert(prod.px == 2);
    assert(prod.py == 4);
    assert(prod.pz == 6);
    assert(prod.vx == 8);
    assert(prod.vy == 10);
    assert(prod.vz == 12);
}

pure nothrow @safe unittest
{
    Vec6 u = Vec6(1, 2, 3, 4, 5, 6);
    Vec6 prod = u * 2.0;
    assert(prod.px == 2);
    assert(prod.py == 4);
    assert(prod.pz == 6);
    assert(prod.vx == 8);
    assert(prod.vy == 10);
    assert(prod.vz == 12);
}

pure nothrow @safe unittest
{
    Vec6 u0 = Vec6(1, 2, 3, 4, 5, 6);
    Vec6 u1 = Vec6(2, 3, 4, 5, 6, 7);
    u0 += u1;
    assert(u0.px == 3);
    assert(u0.py == 5);
    assert(u0.pz == 7);
    assert(u0.vx == 9);
    assert(u0.vy == 11);
    assert(u0.vz == 13);
}

pure nothrow @safe unittest
{
    Vec6 u0 = Vec6(1, 2, 3, 4, 5, 6);
    Vec6 u1 = Vec6(2, 3, 4, 5, 6, 7);
    u1 -= u0;
    assert(u1.px == 1);
    assert(u1.py == 1);
    assert(u1.pz == 1);
    assert(u1.vx == 1);
    assert(u1.vy == 1);
    assert(u1.vz == 1);
}

pure nothrow @safe unittest
{
    Vec6 prod = Vec6(1, 2, 3, 4, 5, 6);
    prod *= 2.0;
    assert(prod.px == 2);
    assert(prod.py == 4);
    assert(prod.pz == 6);
    assert(prod.vx == 8);
    assert(prod.vy == 10);
    assert(prod.vz == 12);
}

pure nothrow @safe unittest
{
    Vec6 u = Vec6(2, 4, 6, 8, 10, 12);
    Vec6 quot = u / 2.0;
    assert(quot.px == 1);
    assert(quot.py == 2);
    assert(quot.pz == 3);
    assert(quot.vx == 4);
    assert(quot.vy == 5);
    assert(quot.vz == 6);
}

pure nothrow @safe unittest
{
    Vec6 quot = Vec6(2, 4, 6, 8, 10, 12);
    quot /= 2.0;
    assert(quot.px == 1);
    assert(quot.py == 2);
    assert(quot.pz == 3);
    assert(quot.vx == 4);
    assert(quot.vy == 5);
    assert(quot.vz == 6);
}
