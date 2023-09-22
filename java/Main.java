import nbh.Vec3;

public class Main {

    public static void print(Vec3 v) {
        System.out.printf("(%f, %f, %f)%n", v.x, v.y, v.z);
    }

    public static void main(String[] args) {
        Vec3 v = new Vec3(1, 2, 3);
        print(v);
    }
}
