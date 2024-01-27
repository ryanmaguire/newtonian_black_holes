module nbh.functions;
private import nbh.vec3 : Vec3;
private import nbh.vec6 : Vec6;

alias stopper = bool function(const Vec3) pure nothrow @safe @nogc;
alias acceleration = Vec3 function(const Vec3) pure nothrow @safe @nogc;
