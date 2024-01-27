module nbh.tools;
private import nbh.vec3 : Vec3;

Vec3 pixelToPoint(uint x, uint y)
pure nothrow @safe
{
    import nbh.setup : pxFactor, pyFactor, start, zSrc;
    immutable double xval = cast(double)x;
    immutable double yval = cast(double)y;
    immutable double xSrc = start + pxFactor * xval;
    immutable double ySrc = start + pyFactor * yval;
    return new Vec3(xSrc, ySrc, zSrc);
}

bool stop(const Vec3 p)
pure nothrow @safe @nogc
{
    import nbh.setup : blackHoleRadiusSq, zDetector;

    if (p.z < zDetector)
        return true;

    if (p.normSq < blackHoleRadiusSq)
        return true;

    return false;
}

Vec3 gravity(const Vec3 p)
pure nothrow @safe
{
    return p / (-p.norm * p.normSq);
}
