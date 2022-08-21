
## What is lambdaQ?

- A functional domain-specific language (DSL) for programming quantum computations, which uses Haskell as the host language.
- In its initial incarnation, it is based on quantum lambda calculus by Selinger and Valiron: https://www.mscs.dal.ca/~selinger/papers/papers/qlambdabook.pdf.
- While the syntax of lambdaQ resembles the Haskell syntax, lambdaQ is intended to be an external DSL, generally built on top of Haskell syntax, since it offer more flexibility in terms of syntax and execution.
- While details remain to be determined, the result of interpreting and/or compiling lambdaQ code will be some combination of the following:
  - quantum circuits specified in an yaml format.
  - quantum circuits specified in OpenQASM3 format.
  - classic code written in Haskell.
  - LLVM intermediate representation code like *Microsoft Quantum Intermediate Representation (QIR)*.
- Initially, the code will be interpreted and optionally run on a quantum simulator (https://github.com/sorin-bolos/moara).
- Eventually, the code will be able to compile the code and execute it on real quantum devices.

## Short-term objectives:

- Quantum lambda calculus with classical control. On quantum side the QRAM model is used (https://www.osti.gov/servlets/purl/366453-CZpmV6/webviewable/).
- BNFC Converter (https://bnfc.digitalgrammars.com/) used for lexing and parsing using a LBNF grammar (https://bnfc.readthedocs.io/en/latest/lbnf.html).
- Types: need to capture the no-cloning property of quantum values. Implement either one of the following:
  - Linear types 
  > a linear value can be used at most once, and cannot be duplicated or discarded.
  - Affine types 
  > an affine value can be used at most once, cannot be duplicated but can be discarded.
- Implement a static type checker.
- Operational Semantics - Interpreter implementing call by value lambda calculus.

## Medium-term objectives:

- Haskell and lambdaQ code will live side by side be able to interop.
- Execute the code on real quantum devices (IBMQ, IONQ, Rigetti, AQT, PASQAL)
- Compiler instead of Interpreter.

## Longer-term objectives:

- Implement type inference.
- Ad support for generic types.
- Add support for dependent types.