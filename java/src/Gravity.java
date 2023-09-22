package nbh;

public class Gravity implements Acceleration {
    public Vec3 acceleration(Vec3 p) {
        double norm = p.norm();
        double norm_squared = norm*norm;
        double factor = -1.0 / (norm * norm_squared);
        return p.multiply(factor);
    }
}
