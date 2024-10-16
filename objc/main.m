#include "nbh/vec3.M"

int main(void)
{
    Vec3 *v = [[Vec3 alloc] init: 1.0 Y:2.0 Z:3.0];
    Vec3 *u = [[Vec3 alloc] init: 4.0 Y:5.0 Z:6.0];
    printf("||v|| = %E\n", [v norm]);
    printf("v . u = %E\n", [v dot: u]);
    [v release];
    [u release];
    return 0;
}
