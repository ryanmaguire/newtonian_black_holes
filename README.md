# Table of Contents
1. [Newtonian Black Holes](#nbh)
2. [Benchmarks](#benchmarks)
3. [License](#license)

# Newtonian Black Holes <a name="nbh"></a>
This is a very small project to experiment with ray tracing and
black holes using (and slightly abusing) Newtonian mechanics.

Newtonian mechanics predicts the existence of black holes.
Take the escape velocity formula and apply it to the speed of light.
The result is the Schwarzschild radius, which is the same
formula one derives if they were to perform to full general
relativistic calculation.
$$R=\frac{2GM}{c^{2}}$$
where $M$ is the mass of the object, $c$ is the speed of light,
and $G$ is the universal gravitational constant. Next we abuse Newtonian
mechanics slightly. Newton's second law says
$$\mathbf{F}(t)=m\mathbf{a}(t)=m\ddot{\mathbf{r}}(t)$$
where $\mathbf{F}$ is the force on an object of mass $m$, $\mathbf{a}$ is
the acceleration vector, $\mathbf{r}$ is the position vector, and double-dots
denote the second derivative with respect to time. For gravity we have
$$m\ddot{\mathbf{r}}(t)=\frac{GMm}{||\mathbf{r}(t)||^{3}}\mathbf{r}(t)$$
where $||\mathbf{r}(t)||$ denotes the *norm* or Euclidean length of the
vector $\mathbf{r}(t)$ and $M$ is the mass of the gravitating object.
Dividing by $m$ we obtain the following equation of motion:
$$\ddot{\mathbf{r}}(t)=\frac{GM}{||\mathbf{r}(t)||^{3}}\mathbf{r}(t)$$
This is all fine and dandy so long as $m\ne{0}$, otherwise we have an
illegal division. Let us suppose that this final equation is true even for
massless particles (like light). Or pretend that light has a mass $m$ that
is so stupidly small (but positive) that it will never be measurable
(The reciprocal of [Graham's number](https://en.wikipedia.org/wiki/Graham%27s_number), if you'd like, in kilograms).
We may then take this equation and use it to approximate how light may
behave under the force of gravity of some object (such as a black hole).
We could then apply raytracing methods and draw a nice picture. This
directory contains files to do exactly this.

**This is not physically realistic**, obviously, but the resulting images
do look like what one might expect a black hole to be.
![Newtonian Black Hole](https://math.dartmouth.edu/~rmaguire/projects/newtonian_black_holes/newtonian_black_hole.png "Newtonian Black Hole")

# Benchmarks
| Language               | Implementation | Time (s) | Flags                      | Version                                  |
| ---------------------- | -------------- | -------- | -------------------------- | ---------------------------------------- |
| C (w/OpenMP)           | gcc            |    1.322 | -O3 -flto -fopenmp         | gcc (Debian 10.2.1-6) 10.2.1 20210110    |
| C (w/OpenMP)           | clang          |    1.704 | -O3 -flto -fopenmp         | Debian clang version 11.0.1-2            |
| C++ (w/OpenMP)         | clang++        |    1.995 | -O3 -flto -fopenmp         | Debian clang version 11.0.1-2            |
| C++ (w/OpenMP)         | g++            |    2.234 | -O3 -flto -fopenmp         | g++ (Debian 10.2.1-6) 10.2.1 20210110    |
| go (w/Parallelization) | golang         |    2.646 |                            | go 1.15.15                               |
| go (w/Parallelization) | gccgo          |    2.921 | -O3 -flto                  | gccgo (Debian 10.2.1-6) 10.2.1 20210110  |
| C                      | gcc            |   15.067 | -O3 -flto                  | gcc (Debian 10.2.1-6) 10.2.1 20210110    |
| C                      | clang          |   16.440 | -O3 -flto                  | Debian clang version 11.0.1-2            |
| Python                 | Pypy           |   18.324 |                            | PyPy 7.3.5 with GCC 10.2.1 20210110      |
| Rust                   | rustc          |   21.160 | -C opt-level=3 -C lto=true | rustc 1.71.1 (eb26296b5 2023-08-03)      |
| Go                     | golang         |   23.883 |                            | go 1.15.15                               |
| Go                     | gccgo          |   27.201 | -O3 -flto                  | gccgo (Debian 10.2.1-6) 10.2.1 20210110  |
| C++                    | g++            |   27.566 | -O3 -flto                  | g++ (Debian 10.2.1-6) 10.2.1 20210110    |
| C++                    | clang++        |   29.547 | -O3 -flto                  | Debian clang version 11.0.1-2            |
| C                      | pcc            |   52.616 | -O2                        | Portable C Compiler 1.2.0.DEVEL 20200630 |
| C                      | tcc            |   80.480 |                            | tcc version 0.9.27                       |
| Python                 | CPython        | 5552.407 |                            | Python 3.9.2                             |

Pleasantly surpised to see go can beat C++ in certain benchmarks.
Utterly surprised that third place (without parallelizing) is Pypy!

A little puzzled by Rust. When running the benchmark several times to get a
good average, the numbers vary like crazy. The first time is ~20 seconds, the
next is nearly a minute! Not sure what's going on there. The time listed in the
above table is the average of the "good" times. The worst was 85 seconds, but
I'm guessing something weird was going on behind the scenes.

Next up is Java and swift.

# License
    newtonian_black_holes is free software: you can redistribute it and/or
    modify it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    newtonian_black_holes is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with newtonian_black_holes.  If not, see <https://www.gnu.org/licenses/>.
