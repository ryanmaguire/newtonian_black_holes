import std.stdio : writeln;
import nbh;

void main()
{
    nbh.vec3.Vec3 v = new nbh.vec3.Vec3(1, 1, 1);
    writeln(v.norm());
}
