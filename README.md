# Table of Contents
1. [Newtonian Black Holes](#nbh)
2. [Benchmarks](#benchmarks)
3. [Benchmarks (w/Parallelization)](#pbenchmarks)
4. [License](#license)

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
The following used a Ryzen 9 3900x (12-core) on Debian 11.

| Language | Implementation | Time (s) | Version                                  |
| -------- | -------------- | -------- | ---------------------------------------- |
| C        | gcc            |   15.067 | gcc (Debian 10.2.1-6) 10.2.1 20210110    |
| Java*    | openJDK        |   16.022 | openjdk 17.0.7 2023-04-18                |
| Fortran  | gfortran       |   16.250 | GNU Fortran (Debian 10.2.1-6) 20210110   |
| C        | clang          |   16.440 | Debian clang version 11.0.1-2            |
| Python   | Pypy           |   18.324 | PyPy 7.3.5 with GCC 10.2.1 20210110      |
| Swift*   | swiftc         |   19.004 | Swift version 5.8.1 (swift-5.8.1-RELEASE)|
| Rust*    | rustc          |   21.160 | rustc 1.71.1 (eb26296b5 2023-08-03)      |
| Pascal   | fpc            |   22.992 | Free Pascal Compiler 3.2.0 20210125      |
| Go       | golang         |   23.883 | go 1.15.15                               |
| Go       | gccgo          |   27.201 | gccgo (Debian 10.2.1-6) 10.2.1 20210110  |
| C++      | g++            |   27.566 | g++ (Debian 10.2.1-6) 10.2.1 20210110    |
| C++      | clang++        |   29.547 | Debian clang version 11.0.1-2            |
| C        | pcc            |   52.616 | Portable C Compiler 1.2.0.DEVEL 20200630 |
| C        | tcc            |   80.480 | tcc version 0.9.27                       |
| Python   | CPython        | 5552.407 | Python 3.9.2                             |
| IDL      | GDL            | 9554.826 | GNU Data Language, Version 0.9.9         |

Compilers marked with a * were release about two years after the others
on the list. Hence this may be comparing apples and oranges. The newer
benchmarks below use up-to-date implementations and are more
suitable for comparisons.

These newer benchmarks used a Ryzen 9 7950x on Debian 12.

| Language | Implementation | Time (s) | Version                                  |
| -------- | -------------- | -------- | ---------------------------------------- |
| C        | gcc            |   11.396 | gcc (Debian 12.2.0-14) 12.2.0            |
| C++      | g++            |   11.840 | g++ (Debian 12.2.0-14) 12.2.0            |
| D        | gdc            |   12.186 | gdc (Debian 12.2.0-14) 12.2.0            |
| C        | clang          |   12.199 | Debian clang version 14.0.6              |
| D        | ldc            |   12.204 | LLVM D Compiler (1.30.0)                 |
| Swift    | swiftc         |   12.216 | Swift version 5.9 (swift-5.9-RELEASE)    |
| C++      | clang++        |   12.361 | Debian clang version 14.0.6              |
| Fortran  | gfortran       |   12.633 | GNU Fortran (Debian 12.2.0-14) 12.2.0    |
| Rust     | rustc          |   12.681 | rustc 1.72.0 (5680fa18f 2023-08-23)      |
| Java     | openJDK        |   12.910 | openjdk 17.0.8 2023-07-18                |
| Python   | Pypy           |   15.481 | PyPy 7.3.11 with GCC 12.2.0              |
| Go       | golang         |   18.118 | go 1.19.8                                |
| Pascal   | fpc            |   18.755 | Free Pascal Compiler 3.2.2 20230330      |
| Go       | gccgo          |   18.932 | gccgo (Debian 12.2.0-14) 12.2.0          |
| D        | dmd            |   35.832 | DMD64 D Compiler v2.107.0                |
| C        | pcc            |   62.625 | Portable C Compiler 1.2.0.DEVEL 20220331 |
| C        | tcc            |   70.031 | tcc version 0.9.27                       |
| Python   | CPython        | 1876.293 | Python 3.11.2                            |
| IDL      | GDL            | 6677.710 | GNU Data Language, Version 1.0.1         |

# Benchmarks (w/Parallelization) <a name="pbenchmarks"></a>
I haven't made parellel versions in every language, but here are the benchmarks
for those that have been done.

Ryzen 9 3900x (12-core) on Debian 11.

| Language | Implementation | Time (s) | Version                                  |
| -------- | -------------- | -------- | ---------------------------------------- |
| C        | gcc            |    1.322 | gcc (Debian 10.2.1-6) 10.2.1 20210110    |
| C        | clang          |    1.704 | Debian clang version 11.0.1-2            |
| C++      | clang++        |    1.995 | Debian clang version 11.0.1-2            |
| C++      | g++            |    2.234 | g++ (Debian 10.2.1-6) 10.2.1 20210110    |
| go       | golang         |    2.646 | go 1.15.15                               |
| go       | gccgo          |    2.921 | gccgo (Debian 10.2.1-6) 10.2.1 20210110  |

Ryzen 9 7950x (16-core) on Debian 12.

| Language | Implementation | Time (s) | Version                                  |
| -------- | -------------- | -------- | ---------------------------------------- |
| C        | gcc            |    0.665 | gcc (Debian 12.2.0-14) 12.2.0            |
| C++      | g++            |    0.666 | g++ (Debian 12.2.0-14) 12.2.0            |
| C++      | clang++        |    0.961 | Debian clang version 14.0.6              |
| C        | clang          |    0.971 | Debian clang version 14.0.6              |
| Go       | golang         |    1.334 | go 1.19.8                                |
| Go       | gccgo          |    1.684 | gccgo (Debian 12.2.0-14) 12.2.0          |

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
