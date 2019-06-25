How to install this project ?

You will need to use opam to install dune and menhir.
You can use the following commands :
        opam install dune
        opam install menhir
/!\ You have to autorize opam to install all the necessary packages for both installations


How to use this project ?
First you have to build the folder, execute the following command in the Parser_v1 folder :
        dune utop
When the folder is successfully build you have to import the library corresponding to the programming language :
        open MyML;;
You will now be able to compile any examples by executing the following command :
        Main.main "<path of the file you wanted to compile>";;
For example :
        Main.main "examples/example-00.mml";;        

