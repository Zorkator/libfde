
Fortran Development Extensions (libfde)
=======================================

.. contents::

The extension of legacy code often presents developers with big challenges.
Particularly in case of Fortran code, the implementation of newer software concepts requires a great effort, very good understanding
  of language concepts (fixed format, f77, implicit declaration, etc.) and the legacy code, which is often way beyond the scope of many projects.
For this reason, new software projects often try to integrate old, thoroughly tested but yet unmaintained code, as-is.
However, even this approach turns out to be difficult enough in practice.
Aside from other issues, it's likely that signatures of routines have to be changed to make them externally available to languages that follow
  the C-conventions, what is often not as straight forward as it seems.
Such simple modifications allow calling the code, but they alredy hold certain danger of introducing bugs!
Allowing even extended data exchange or advanced control over code execution require a lot more effort, care and testing.

The "lessons learned" from a number of Fortran-based projects of recent years have been combined into this library, with the aim of facilitating
  the integration and control of legacy Fortran codes.
The included techniques make it possible to use such codes virtually unchanged, but still make it accessible for data exchange and process control.
As a result, software techniques such as using generic and heterogeneous data structures, event-based control, exception handling, and dynamic reload
  of code parts (aka plugins) are possible even within the narrow confines of Fortran.
The library furthermore provides a python package (ctypes-based) that allows to load and use Fortran codes compiled as shared libraries within python.


Naming
------

The library development started with the idea of providing abstract data types in Fortran, what resulted in naming the library libadt.
Meanwhile this focus shifted and it contains a lot more, so it really should be renamed to libfde (Fortran Development Extensions) to reflect better
  it's purpose and contained functionality.
While the repository already got renamed to libfde, the prefix used internally is _still_ *adt*.
This prefix will change in one of the next releases, but it should not have a great impact on codes using the libraries.


Basic principles and state of development
-----------------------------------------

There are other, well-established approaches of making Fortran codes available in Python (e.g. F2PY http://www.f2py.com/).
However, aside from using Fortran codes in Python the mayor goals of libfde comprise a lot more:

 - call, control and access data of Fortran code with only minimal invasive changes.
     So using the library and preparing the legacy code should be as simple and modest as possible.
 - it should not put restrictions on the toolchain or require a certain Fortran compiler, although in practice **there is** the need of
     using at least a Fortran compiler that provides a reasonable C-preprocessor and f90/f2003-support!
     At least ifort (>= 14.0) and gfortran (>= 4.8) should work fine, without regard to buggy compiler versions
     (Basically the flood of bugs in Fortran compilers is the reason why libfde does not make use of newer Fortran concepts, e.g. classes).
 - the exported interfaces are kept C-compatible - however, *not really* ISO Bind C, because this would put far too many restrictions
     on code that just passes strings or structured types to routines.


The library reached a quite stable state and it's used in productive code.
Nevertheless, **a lot** of things can be improved and some aspects need further development.
 
 - mainly, the documentation of interfaces and examples!
   (still looking for a good tool for generating it from Fortran-code!!)
 - the python interface is not complete
 - some constructs should be revised to improve memory efficiency
 - some data structures might be way better off being re-implemented in the C/C++ core (libfortres)
 - ...

