package nbh;

public class Euler {
    static int max_iterations = 65535;
    static double time_increment = 0.01;

    public static void path(Vec6 u, Acceleration acc, Stopper stopper) {
        int iters = 0;
        Vec3 a = new Vec3(0.0, 0.0, 0.0);

        while (!stopper.stop(u) && iters < max_iterations) {
            a = acc.acceleration(u.p);
            u.p.scaledAddTo(time_increment, u.v);
            u.v.scaledAddTo(time_increment, a);
            iters += 1;
        }
    }
}
