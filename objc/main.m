#include "nbh/vec3.M"
#include "nbh/vec6.M"
#include "nbh/ppm.M"
#include "nbh/color.M"
#include "nbh/function_types.M"

void destroy(id *val)
{
    [*val release];
    *val = nil;
}

int main(void)
{
    Vec3 *p = [[Vec3 alloc] init: 1.0 Y: 2.0 Z: 3.0];
    Vec3 *v = [[Vec3 alloc] init: 4.0 Y: 5.0 Z: 6.0];
    Vec6 *u = [[Vec6 alloc] init: p Vel: v];
    Vec6 *w = [u copy];
    Vec6 *sum = [u plus: w];

    destroy(&p);
    destroy(&v);
    destroy(&u);
    destroy(&w);

    printf("%E\n", [sum norm]);
    destroy(&sum);

    return 0;
}
