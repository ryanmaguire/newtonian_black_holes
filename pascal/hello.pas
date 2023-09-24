PROGRAM Hello;
USES
    nbh_Vec3;

VAR
    V : Vec3;

BEGIN
    V := Vec3Rect(1, 2, 3);
    Vec3Print(V);
    WriteLn('Hello, World!');
END.
