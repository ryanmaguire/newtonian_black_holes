package nbh;

public class Stop implements Stopper {
    public boolean stop(Vec6 u) {
        if (u.p.z >= Setup.z_detector) {
            return true;
        } else if (u.p.normSquared() <= Setup.black_hole_radius_squared) {
            return true;
        } else {
            return false;
        }
    }
}
