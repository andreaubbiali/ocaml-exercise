# PAY ATTENTION ON MODULES

In OCaml, every piece of code is wrapped into a module. Optionally, a module itself can be a submodule of another module, pretty much like directories in a file system - but we don't do this very often.

When you write a program let's say using two files `amodule.ml` and `bmodule.ml`, each of these files automatically defines a module named `Amodule` and a module named `Bmodule` that provide whatever you put into the files.

In fact you can have:

`amodule.ml`
<code>
    let hello () = print_endline "Hello"
</code>

`bmodule.ml`
<code>
    open Amodule;;

    let () = hello ();;
</code>

And compiling them the bmodule will open the amodule and call its function hello().

# HOW TO COMPILE MODULES

* ocamlc -c Interface.mli
* ocamlc -c ImplementationN.ml
* ocamlc -c Functor.ml

In the functor.ml to use the interface you can do:
<code>
    open interface

    module FunctorName( AliasInterface: InterfaceModuleName)
    ...
</code>


* ocamlc module1.cmo module2.cmo ... main.ml

(
    moduleN are files that contains modules used by the main
    For example ImplementationN.cmo and Functor.cmo
)

In the main file on the head you need to specify:
<code>
    open ImplementationN;;
    open Functor;;

    module example = Functor(ImplementationN)
</code>

## Capital letter

Pay attention that sometimes return compilation error because modules names must be in capital letter.