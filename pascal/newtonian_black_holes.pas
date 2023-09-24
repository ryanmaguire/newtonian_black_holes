PROGRAM Hello;
USES
    nbh_Color,
    nbh_Euler,
    nbh_FunctionTypes,
    nbh_Setup,
    nbh_Vec3,
    nbh_Vec6;

CONST
    InitialVelocity: Vec3 = (0.0, 0.0, 1.0);
    PPM_FILE_NAME = 'newtonian_black_holes.ppm';

VAR
    P, V: Vec3;
    U: Vec6;
    X, Y: Integer;
    C: Color;
    PPM: Text;
    ProgFactor: Real;

BEGIN
    ProgFactor := 100.0 / YSIZE;
    Assign(PPM, PPM_FILE_NAME);
    Rewrite(PPM);
    WriteLn(PPM, 'P3');
    WriteLn(PPM, XSIZE:4, YSIZE:5);
    WriteLn(PPM, '255');

    FOR Y:= 0 TO YSIZE-1 DO
    BEGIN
        FOR X:= 0 TO XSIZE-1 DO
        BEGIN
            P := PixelToPoint(X, Y);
            V := InitialVelocity;
            U := Vec6FromVectors(P, V);
            nbh_Euler.EulerPath(U, @Gravity, @Stop);
            C := CheckerBoard(U);
            ColorWrite(C, PPM);
        END;
        IF (Y MOD 20 = 0) THEN WriteLn('Progress: ', ProgFactor*Y:2:4, '%');
    END;
    WriteLn('Done');
    Close(PPM);
END.
