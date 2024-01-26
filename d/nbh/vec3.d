module nbh.vec3;

class Vec3 {
    private {
        double x;
        double y;
        double z;
    }

    public {

        this(double x, double y, double z) @safe @nogc
        {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        double X() @safe @nogc
        {
            return this.x;
        }

        double Y() @safe @nogc
        {
            return this.y;
        }

        double Z() @safe @nogc
        {
            return this.z;
        }

        double normSq() @safe @nogc
        {
            return x*x + y*y + z*z;
        }

        double norm() @safe @nogc
        {
            import std.math : sqrt;
            return sqrt(this.normSq());
        }
    }
}
