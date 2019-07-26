How to install this project ?

You will need to use opam to install dune and menhir.
You can use the following commands:
        opam install dune
        opam install menhir
        opam install utop
        opam install ppx_expect
/!\ You have to autorize opam to install all the necessary packages for both installations


How to use this project?
First you have to build the folder, execute the following command in the src folder:
        dune utop
When the folder is successfully build you have to import the library corresponding to the programming language:
        open MyML;;
You will now be able to compile any examples by executing the following command:
        Main.main "<path of the file you want to compile>";;
For example :
        Main.main "examples/examples/example-00.mml";;
        
How to run the automated tests?
Simply execute the following command:
        dune runtest
If no message appears, all the tests have been successful.
You will find in the text file src/Tests.txt a short description of each test

How to add an automated test?
Add the following line to the Testing.ml file:
        let%expect_test _ = print_string (main "<path of the test>"); [%expect{| <string corresponding to the output> |}]
