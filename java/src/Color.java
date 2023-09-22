package nbh;

public class Color {
    public int red, green, blue;

    public Color(int r, int g, int b) {
        red = r;
        green = g;
        blue = b;
    }

    public void write(PPM ppm) {
        ppm.write(String.format("%d %d %d%n", red, green, blue));
    }

    public Color scale(double t) {
        int r = (int)(t * red);
        int g = (int)(t * green);
        int b = (int)(t * blue);
        return new Color(r, g, b);
    }

    public void scaleBy(double t) {
        red = (int)(t * red);
        green = (int)(t * green);
        blue = (int)(t * blue);
    }

    public static Color checkerBoard(Vec6 u) {
        int cx, cy, use_white;
        double color_factor = Setup.z_detector_squared / u.p.normSquared();
        int val = (int)(255.0 * color_factor);

        if (u.p.z < Setup.z_detector) {
            return new Color(0, 0, 0);
        }

        cx = (int)(Math.ceil(u.p.x));
        cy = (int)(Math.ceil(u.p.y));
        use_white = (cx + cy) & 1;

        if (use_white != 0) {
            return new Color(val, val, val);
        } else {
            return new Color(val, 0, 0);
        }
    }
}
