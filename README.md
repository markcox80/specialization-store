The specialization store system provides a new kind of function,
called a store function, whose behavior is determined by the *types*
of objects given as arguments to the store function.

Associated with each store function is a set of *specialization*
objects. Each specialization encapsulates behavior and the domain for
which the behavior is defined.

A premise of specialization store is that all specializations should
perform the same task. Specializations should only differ in how the
task is performed. This premise resolves ambiguities that arise when
using types, rather than classes, to select the most specific
specialization to apply.

Specialization store is a major component of a related system,
the
[template-function system](https://github.com/markcox80/template-function/). A
wiki page in this project
outlines
[one of the problems](https://github.com/markcox80/template-function/wiki/Motivating-the-Template-Function-System) the
specialization store system was designed to address.

The following list summarizes the major differences between a store
function and common lisp's
[generic function](http://www.lispworks.com/documentation/HyperSpec/Body/07_f.htm):
- Types rather than classes are used to select behavior.
- Optional, rest and/or keyword arguments are considered when
  determining behavior.
- Initialization forms for optional and keyword arguments are part of
  the state of a store function.
- Support for compile time optimizations.
- No support for method/specialization combination.
- No support for argument precedence.
- No support for class precedence.

The specialization store system includes a meta object protocol to
allow users to change the default implementation.

Documentation for this project is available in
the
[project wiki](https://github.com/markcox80/specialization-store/wiki).

The specialization store system has been tested
using [SBCL](http://www.sbcl.org)
and [Clozure Common Lisp](https://ccl.clozure.com).

Great effort has been spent on implementing this system such that it
is portable to all implementations. The compile time dispatch
functionality makes extensive use of compiler macros and the
[CLTL2 environment API](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node102.html#SECTION001250000000000000000).

# Installation (ASDF)

The specialization store system
uses [ASDF](https://common-lisp.net/project/asdf/) to the manage
compilation and loading of source files.

The specialization store system depends on the following systems:
1. [Alexandria](https://common-lisp.net/project/alexandria/)
2. [Introspect Environment](https://github.com/Bike/introspect-environment)
3. [FiveAM](https://common-lisp.net/project/fiveam/) (test framework)

The specialization store system can be loaded in to your lisp
environment by evaluating the following forms

```lisp
(asdf:load-system "specialization-store")
```

The specialization store system includes a large suite of tests. These
tests can be executed by evaluating the following forms

```lisp
(asdf:test-system "specialization-store")
```
