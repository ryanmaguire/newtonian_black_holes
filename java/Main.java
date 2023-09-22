import nbh.Vec3;
import nbh.Vec6;
import nbh.PPM;
import nbh.Color;
import nbh.Setup;
import nbh.Gravity;
import nbh.Stop;
import nbh.Euler;

public class Main {
    public static void main(String[] args) {
        int x, y;
        Color c;

        double prog_factor = 100.0 / (double)(Setup.y_size);
        Gravity gravity = new Gravity();
        Stop stop = new Stop();

        PPM ppm = new PPM("newtonian_black_holes.ppm");
        ppm.init();

        for (y = 0; y < Setup.y_size; ++y) {
            for (x = 0; x < Setup.x_size; ++x) {
                Vec3 p = Setup.pixelToPoint(x, y);
                Vec3 v = new Vec3(0.0, 0.0, 1.0);
                Vec6 u = new Vec6(p, v);

                Euler.path(u, gravity, stop);

                c = Color.checkerBoard(u);
                c.write(ppm);
            }

            if (y % 20 == 0) {
              System.out.printf("Progress: %f%%     \r", prog_factor*(double)y);
            }
        }
        System.out.println("Progress: 100%         ");
        System.out.println("Done.");
        ppm.close();
    }
}
