package nbh;

public class Setup {
    public static double z_src = -10.0;
    public static double z_detector = 10.0;
    public static double z_detector_squared = z_detector*z_detector;
    public static double start = -10.0;
    public static double end = 10.0;
    public static int x_size = 1024;
    public static int y_size = 1024;
    public static double px_factor = (end - start) / (double)(x_size - 1);
    public static double py_factor = (end - start) / (double)(y_size - 1);
    public static double black_hole_radius = 1.0;
    public static double black_hole_radius_squared
         = black_hole_radius*black_hole_radius;

}
