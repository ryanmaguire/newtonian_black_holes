PROGRAM Hello;
USES
    nbh_Vec3,
    nbh_Vec6,
    nbh_Euler;

VAR
    P, V: Vec3;
    U: Vec6;
    R: Real;

BEGIN
    P := Vec3Rect(1.0, 0.0, 1.0);
    V := Vec3Rect(1.0, 2.0, 3.0);
    U := Vec6FromVectors(P, V);
    R := Vec6Norm(U);
    WriteLn('Hello, World!');
    WriteLn('The Norm is: ', R);
END.
