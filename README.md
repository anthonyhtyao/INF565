Iterpreter function is in newsyntax.ml
Compiler function is in compile.ml
Typage function is in typage.ml

How to test:

  make test
  ./test filename

This will echo something likes below:

Test expression:
  Here is to verify lexer and parser works
New syntex expression :
  Convert to new syntax
After calculating De Bruijn index :
  Echo instance with its De Bruijn index
Intrepretation:
  Result for intrepretor
Typage:
  Check type
Compiling ...
  Echo result of compile

For example, if we find error in section typage, its means that type's problem for the given file
