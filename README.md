# WebAssembly Language
This project was born out of my quest to understand and build compilers.

## Technical spec for byte code
There is a PDF in the root directory named `WebAssembly.pdf`. The implementation of the compiler is based on this guide. If you have not already you can go through it to understand the ABI.

## Grammar
The grammar for the language set is defined in `ast/grammar.pest`. I am using Parsing Expression Grammars (PEG) to parse the source into a token stream. The library being used is [pest.rs](http://pest.rs/)

## Compilation process
An AST is created from the token stream. This AST is optimized by a simple algorithm; if the value of the two children of a binary operation can be derived at compile time then execute the binary tree at compile time and return the resultant node. This saves time spent in running unnecessary instructions at runtime. Then using the ABI, this AST is compiled down to WASM byte code.

## Where to find what?
The source code for the parser, optimizer and ABI implementation is on `src/parser.rs`. There are some helper functions in `src/functions.rs` that help create abstractions over the webassembly implementations. A sample script showing an example of a working program written using the defined grammar is stored in `source/index.wasl`.