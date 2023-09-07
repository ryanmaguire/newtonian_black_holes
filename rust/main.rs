mod nbh;

fn main() {
    let p = nbh::vec3::Vec3{x: 1.0, y: 2.0, z: 3.0};
    let q = nbh::vec3::Vec3{x: 4.0, y: 5.0, z: 6.0};
    let mut sum = p + q;
    let cross = p.cross(sum);
    sum += q;

    println!("<{}, {}, {}>", cross.x, cross.y, cross.z);
    println!("<{}, {}, {}>", sum.x, sum.y, sum.z);
    println!("{}", cross.dot(p));
    println!("{}", p.norm());
    println!("{}", p.norm_sq());
    println!("{}", p.rho());
    println!("{}", p.rho_sq());
}
