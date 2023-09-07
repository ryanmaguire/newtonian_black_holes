extern crate nbh;

fn main() {
    nbh::raytrace::run(nbh::setup::gravity,
                       nbh::setup::stop,
                       nbh::color::checker_board,
                       nbh::euler::path,
                       "newtonian_black_hole.ppm");
}
