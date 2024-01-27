module nbh.euler;
private import nbh.functions;
private import nbh.setup;
private import nbh.vec3 : Vec3;
private import nbh.vec6 : Vec6;
immutable uint maxIters = 65535U;
immutable double eulerIncrement = 0.01;

void path(ref Vec6 u, acceleration acc, stopper stop)
@safe @nogc
{
    uint n = 0U;
    Vec3 p = u.pos;
    Vec3 v = u.vel;
    Vec3 a;

    while (!stop(p) && n < maxIters)
    {
        a = acc(p);
        p += eulerIncrement * v;
        v += eulerIncrement * a;
        ++n;
    }

    u.pos = p;
    u.vel = v;
}
