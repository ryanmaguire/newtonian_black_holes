module nbh.color;
private import nbh.ppm : PPM;
private import nbh.vec3 : Vec3;

class Color {
    private {
        ubyte[3] dat;
    }

    public {
        this(ubyte red, ubyte green, ubyte blue)
        pure nothrow @safe @nogc
        {
            this.dat[0] = red;
            this.dat[1] = green;
            this.dat[2] = blue;
        }

        ubyte red()
        pure nothrow @safe @nogc const
        {
            return this.dat[0];
        }

        ubyte green()
        pure nothrow @safe @nogc const
        {
            return this.dat[1];
        }

        ubyte blue()
        pure nothrow @safe @nogc const
        {
            return this.dat[2];
        }

        void write(PPM ppm)
        @safe
        {
            ppm.file.rawWrite(dat);
        }

        Color opBinary(string op : "*")(double a)
        pure nothrow @safe const
        {
            immutable double r = a * cast(double)this.dat[0];
            immutable double g = a * cast(double)this.dat[1];
            immutable double b = a * cast(double)this.dat[2];

            immutable ubyte red = cast(ubyte)r;
            immutable ubyte green = cast(ubyte)g;
            immutable ubyte blue = cast(ubyte)b;
            return new Color(red, green, blue);
        }

        void opOpAssign(string op : "*")(double a)
        pure nothrow @safe @nogc
        {
            immutable double r = a * cast(double)this.dat[0];
            immutable double g = a * cast(double)this.dat[1];
            immutable double b = a * cast(double)this.dat[2];
            this.dat[0] = cast(ubyte)r;
            this.dat[1] = cast(ubyte)g;
            this.dat[2] = cast(ubyte)b;
        }
    }
};

Color red(double val)
pure nothrow @safe
{
    ubyte c = cast(ubyte)(255.0 * val);
    return new Color(c, 0x00, 0x00);
}

Color white(double val)
pure nothrow @safe
{
    ubyte c = cast(ubyte)(255.0 * val);
    return new Color(c, c, c);
}

Color black()
pure nothrow @safe
{
    return new Color(0x00, 0x00, 0x00);
}

Color checkerBoard(const Vec3 p)
pure nothrow @safe
{
    import nbh.setup : zDetectorSq, zDetector;
    import std.math : ceil;

    /*  Factor for darkening the checker board.                               */
    double cfact = zDetectorSq / p.normSq;

    /*  If the photon didn't make it, color the pixel black.                  */
    if (p.z > zDetector)
        return black;

    /*  Otherwise use a bit-wise trick to color the plane.                    */
    if (cast(uint)(ceil(p.x) + ceil(p.y)) & 1U)
        return red(cfact);

    return white(cfact);
}
