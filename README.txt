How to install this project ?
You will need to use opam to install dune, menhir, utop and the test library.
You can use the following commands:
        opam install dune
        opam install menhir
        opam install utop
        opam install ppx_expect
/!\ You have to autorize opam to install all the necessary packages for all installations


How to use this project?
First you have to build the folder, execute the following command in the src folder:
        dune utop
When the folder is successfully build you have to import the library corresponding to the programming language:
        open MyML;;
You will now be able to compile any examples by executing the following command:
        Main.main <verbosity> <maxstep> ~seed:<nb> "<path of the file you want to compile>";;
- verbosity: between 0 and 3
- maxstep: positive integer
- nb: positive integer
For example:
        Main.main 1 10000 ~seed:25 "examples/examples/example-00.mml";;
The seed is optional thus the compilation is possible without one
For example:
        Main.main 1 10000 "examples/examples/example-00.mml";;
To exit dune use:
        exit 0;;
        
How to run the automated tests?
Simply execute the following command:
        dune runtest
If no message appears, all the tests have been successful.
You will find in the text file src/Tests.txt a short description of each test

How to add an automated test?
Add the following line to the Testing.ml file:
        let%expect_test _ = print_string (main "<path of the test>"); [%expect{| <string corresponding to the output> |}]
