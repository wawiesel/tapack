THIS PROJECT IS UNSUPPORTED WITH A COMPLETELY OPEN LICENSE FOR THE COMPONENTS
DEVELOPED BELOW IN THE HOPES SOME OF THE ALGORITHMS COULD BE USEFUL.

## Transport Algorithms Package (TAPACK) v2.5.0 (2012)

This repository represents the final state of the TAPACK software used in 
this journal paper. 

```
William A. Wieselquist, Dmitriy Y. Anistratov, Jim E. Morel,
A cell-local finite difference discretization of the low-order 
quasidiffusion equations for neutral particle transport on unstructured 
quadrilateral meshes,
Journal of Computational Physics,
Volume 273,
2014,
Pages 343-357,
ISSN 0021-9991,
https://doi.org/10.1016/j.jcp.2014.05.011.
(http://www.sciencedirect.com/science/article/pii/S002199911400357X)
Abstract: We present a quasidiffusion (QD) method for solving neutral 
particle transport problems in Cartesian XY geometry on unstructured 
quadrilateral meshes, including local refinement capability. Neutral 
particle transport problems are central to many applications including 
nuclear reactor design, radiation safety, astrophysics, medical imaging, 
radiotherapy, nuclear fuel transport/storage, shielding design, and oil 
well-logging. The primary development is a new discretization of the 
low-order QD (LOQD) equations based on cell-local finite differences. 
The accuracy of the LOQD equations depends on proper calculation of 
special non-linear QD (Eddington) factors from a transport solution. 
In order to completely define the new QD method, a proper discretization 
of the transport problem is also presented. The transport equation is 
discretized by a conservative method of short characteristics with a 
novel linear approximation of the scattering source term and monotonic, 
parabolic representation of the angular flux on incoming faces. Analytic 
and numerical tests are used to test the accuracy and spatial convergence 
of the non-linear method. All tests exhibit O(h2) convergence of the 
scalar flux on orthogonal, random, and multi-level meshes.

Keywords: Discretization; Particle transport equation; Unstructured 
quadrilateral meshes; Linear source representation
```

## BUILD and TEST

To build, execute the following:

```
source script/config.sh gfortran
cd build
make -j4
```
This will place the tap.exe executable in the `bin` directory.

All tests may be run with
```
source script/test.sh
```

## LAST KNOWN WORKING

This software was last compiled on UBUNTU 12 with the GFORTRAN 4.4.3 
in 2012. It requires `-std=gnu` and a C-preprocessor flag. 
That bin and build directory are included in the `releases` subdirectory.
You should be able to use the `releases/v2.5.0/bin/tap.exe` on an older linux 
system and run all tests. 

Currently, there are build errors when trying to compile with GCC 4.9 which 
when resolved still have some issues running the code. It seems some 
assumptions with pointers initial status or recursive behavior has changed
from the compilers used in the last working version. 
 
