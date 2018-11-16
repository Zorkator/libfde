
# Fortran Development Extensions (libfde)

The extension of legacy code often presents developers with big challanges.
Particularly in case of Fortran code, the implementation of newer software concepts often requires a great effort, very good understanding
  of language concepts (fixed format, f77, implicit declaration, etc.) and the lagacy code, which is beyond the scope of many projects.
For this reason, new software projects often try to integrate old, thoroughly tested but yet unmaintained code, as-is.
However, even this approach turns out to be difficult enough in practice.
It's likely that signatures of routines have to be changed to make them externally available to languages that follow the C-conventions, what
  is often not as straight forward as it seems.
While such modifications allow calling the code, extended data exchange or advanced control over code execution require a lot more.

The "lessons learned" from a number of Fortran-based projects of recent years have been combined into this library, with the aim of facilitating
  the integration and control of legacy Fortran codes.
The included techniques make it possible to use suche codes virtually unchanged, but still make it accessible for data exchange and process control.
As a result, software techniques such as using generic and heterogenous data structures, event-based control, exception handling, and dynamic reload
  of code parts (aka plugins) are possible even within the narrow confines of Fortran.
The library furthermore provides a python package (ctypes-based) that allows to load and use Fortran codes compiled as shared libraries within python.


## Naming

The library development started with the idea of providing abstract data types in Fortran, what resulted in naming the library libadt.
Meanwhile this focus shifted and it contains a lot more, so it really should be renamed to libfde (Fortran Development Extensions) to reflect better
  it's porpose and contained functionality.
While the repository already got renamed to libfde, the prefix used internally is _still_ *adt*.
This prefix will change in one of the next releases, but it should not have a great inpact on codes using the libraries.


## Basic priciples and state of development

There are other, well-established approaches of making Fortran codes available in Python (e.g. F2PY http://www.f2py.com/).
However, the mayor goals of libfde comprise a lot more:

 - call, control and access data of Fortran code with only minimal invasive changes.
     So using the library to prepare the code should be as simple as modest as possible.
 - it should not put restrictions on the toolchain or require a certain Fortran compiler, although in practice **there is** the need of
     using at least a Fortran compiler that provides a reasonable C-preprocessor and f90/f2003-support ** !
     At least ifort (>= 14.0) and gfortran (>= 4.8) should work fine.
     Basically the flood of compiler bugs are the reason why libfde-code does not make use of newer Fortran concepts (e.g. classes).
 - the exported interfaces are kept C-compatible - however, *not really* ISO Bind C, because this would put far too many restrictions
     on code that just passes strings or structured types to routines.


The library reached a quite stable state and it's used in productive code.
Nevertheless, **a lot** of things needs to be improved and some aspects need further development.
 
 - mainly, the documentation of interfaces and examples!
   (still looking for a good tool for generating it from code!!)
 - some constructs need to be revised
 - the python interface is not complete
 - some data structures might be way better off being reimplemented in the C/C++ core (libfortres)
 - ...

