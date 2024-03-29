### Introduction

lambdaQ - A new functional language for quantum computing based on an extension of classical lambda calculus [1] named quantum lambda calculus, based on the work of Peter Selinger and Benoit Valiron [2, 3]. The language is built as an external DSL for Haskell. Possibly later, it will be reconfigured as a Haskell Language Extension [4].

What lambdaQ will bring to the table differently from other languages like QML [13], QCL [25], Quipper [9], QWire [14], Silq [10] funQ [15, 27] or Q [29] is a new semantics. The language will be practical rather than another theoretical language designed for the study of formal semantics of quantum computation. Programs will be compiled into the following two quantum intermediate representations code formats:

- The LLVM Quantum Intermediate Representation (QIR) [5] which is supported by Microsoft, Quantum Circuits Inc., Quantinuum, Rigetti and others.
- OpenQASM3 [11] supported by IBM which is regarded both as a high and a low-level Intermediate Representation (IR).  

Note: it looks like IBM will also use LLVM in their software stack but details have not been yet made public.

###  Guiding Principles 

This document describes a rather heterogeneous set of features and requirements. The purpose of a functional language should be to provide a higher level of programming experience for a quantum developer. For example, an interesting question is whether a functional language can be more expressive than plain circuit lanuguages when coding quantum algorithms.  

Our intention is to make lambdaQ programs more expressive than circuit description languages or jupyter notebooks for quantum programs in general. It remains to be seen during development which are those constructs that will prove suitable for creating a useful high-level quantum language. However, from the beginning, several features can be considered in this regard:

- strive for increased readability, abstraction, and code reuse. 
- quantum conditionals, express phase kick-back using language constructs.
- mid-circuit measurements and mid-circuit resets.
- measurement-dependent branches (quantum data with classical control). 
- quantum data types with nontrivial semantics.
- high-order functions and recursive functions.
- recursive types and lists are used to represent repetitive circuit elements.
- representation for families of circuits in data (e.g. parametrized circuits).
- operations to be applied at circuit level over entire circuit blocks: control, reverse, power, exponentiate. 
- facilities to help with handling uncomputation and qubit borrowing.
- reversible circuit synthesis from classical functions.


### Planned Features and Priorities for Implementing

#### Must Have Features - Immediate Objectives

- Using the QRAM [16] paradigm for quantum computation, where the quantum computer acts as a coprocessor. Lambda terms encode the control structure of the program and are implemented on a classical device but the data upon which lambda terms act can be quantum and thus stored on a QRAM quantum device. Since functions are considered classical data there cannot be a superposition of different functions at some point in the program. On the other hand the question of whether a lambda term is 'quantum' or not, meaning that it cannot or can be duplicated, is a different one and the answer does not depend on the type of its input/output variables alone but also on the type of its free variables. As a side note, an alternative to the approach of functions as classical data is advanced in [30], see also [31, 32, 33, 34], but it is unclear how to implement such ideas using a circuit model.  
- Stand-alone language with specified syntax, type checker, and a compiler. The syntax is similar to Haskell syntax.
- The language is statically typed, types must be explicitly declared.  
- Affine (linear) types will be used to represent quantum resources like qubits and operations on qubits.
- Two basic data types: bits and qubits.
- Support for standard classical data types: integer, floats, booleans, strings.
- Angle data type = 2π * fraction as bit string, or perhaps 4π * fraction, similar to OpenQASM3 data type with the same name.
- Call-by-value operational semantics.
- The compiler outputs some form of IR code, in principle both OpenQASM3 and the LLVM QIR will be used.
- An interpreter + REPL will probably be useful for development. For this, the simulator created for the Uranium Platform [8] will be used to simulate circuits.
- The language is built as an external DSL written in Haskell (Haskell is used as the host language).
- Haskell and lambdaQ code will live side by side and be able to interop. LambdaQ should accommodate some classical data types and classical programming constructs. However, instead of replicating all features of Haskell, perhaps calling Haskell functions from lambdaQ should be supported instead.
- Classical, non-linear fragments of lambdaQ code could in principle be cast to Haskell code or can be compiled directly into LLVM Haskell bindings.
- Quantum operations to be supported: new, ms (measure), reset, a fixed set of Unitary gates.
- Quantum operations and gates are implemented as constant terms in a quantum lambda calculus.
- Implement most common quantum gates: X, Y, Z, Pauli Root Gates, Hadamard, Id, RX, RY, RZ, S(S†), T(T†), V(V†), h(h†), U1, U2, U3, P, SWAP, SQRT-SWAP, ISWAP, FSWAP, SWAP-ROOT
- BNFC converter tool [6] to be used to generate a compiler frontend from an LBNF  grammar [7]:  
 	LBNF Grammar -> Lexer -> Abstract Syntax Tree -> Parser
- A type checker will ensure the correctness of programs. 
- A gate can be assigned any number of controls, except for the Identity gate. A CTRL term to be added to the grammar.
- Controls can be specified in standard Z basis ('0', '1').
- Support for mid-circuit measurements and mid-circuit resets. 
- Classically controlled quantum gates (if statements) and more general classically conditioned quantum operations. Using assignments to a classical variable and resets or measurements inside a conditioned code is posssible only if the control variable is classical.
- A program is defined as a list of functions that includes a function named 'main'.
- Support for tuples
- Support for recursive functions
 
#### Must Have Features but not Necessarily in the Initial Implementation
- Support for declaring and importing modules.
- Support for lists and recursion: map & fold.
- Support for custom data types and inductive (recursive) data types.
#### Nice to Have Features - Medium Timeframe Objectives

- Quantum conditionals (quantum branching) [18, 21, 25, 28, 31]. A very simple example of quantum branching is the CNOT gate but implementing quantum branching is nontrivial: 
    - Two programs that differ by a global phase have the same observable behavior, but when subjected to a conditional execution may behave differently.  
    - Measurements and reset do not seem to have a natural meaning when subjected to a quantum condition. Care should be taken when designing program semantics than encompass both irreversible operations and quantum branching.  
- Express phase kick-back and amplitude amplification using language constructs.  
- Support for declaring and using quantum and classical registers, see [25] for example and [26] section 2.1. 
    - Constant registers (qconst): an operator having a constant register as input may not modify it. 
    - Void register (qvoid) is guaranteed to be empty at the beginning of execution, useful when implementing Oracles as classical functions.
    - Scratch register (qscratch) is assumed to be empty at the beginning of an operation and must be left empty after the operation has finished. 
   - Declaring quantum data type with advanced semantics as exemplified above may require facilities for automatic uncomputation and qubit borrowing [23]. 
- Functions can be used as subroutines. Add modules and the ability to import code. Add a standard library.
- Circuits can be encapsulated as classical functions - parametrized circuits. See for example boxing (and unboxing) operation where a circuit describing a linear operation is promoted as classical data and thus be reused multiple times as parts of a larger circuit (Quipper [9] and QWIRE [14]).
- When circuits are transpiled to a lower level representation, adding input parameters, for example with an input/output modifier can result in a transpiled circuit encapsulating input/output parameters abstractly. Exact values for parameters will be supplied at run time and compilation only happens once for a given parametrized circuit. 
- Dynamic circuits (also known as dynamic lifting): quantum processor has the ability to preserve a qubit state between successive invocations from a classical device such that a classical device sends the continuation of some computation in real-time. Quantum operations and concurrent real-time classical computation are supported by OpenQASM3 for example [24].
- New gates can be encapsulated as functions (implement custom gates).
- Circuit level operators (control, reverse, power, compose, reorder input/output qubits, exponentiate).
- Multiple controlled gates can be decomposed in a simple gate basis set (e.g: CX, ID, RZ, SX, X for OpenQasm3).
- Rather than being an external DSL, the language will be packed as a Haskell Language Extension [4].
- Output code can be executed on real quantum devices (IBMQ, IONQ, Rigetti, AQT).
- Besides Z basis, controls can be specified also in X basis ('+', '-') and Y basis ('+i', '-i').
- Libraries (e.g. QFT, amplitude amplification, phase estimation, quantum arithmetic).
- Teleportation as a primitive.
- Barrier statement.
- Interpreter connects to a quantum cloud service like IBMQ [17] and sends the quantum program to be executed.

#### Other Features - Currently Out of Scope

- Implement type inference.
- Add support for generic types using a Hindley-Milner type system.
- Add support for dependent types in order to model families of circuits and model type-safe uncomputation of garbage qubits, see for example [22].
- Synthesis of reversible quantum circuits from classical functions, see [9] and [12].
- Quantum resource estimation - gate counts, no qubits, circuit depth, and width.
- Implement optimization and error-correcting techniques.
- Concurrent execution of independent code segments like for example extended circuits defined by the OpenQASM execution model.
- Implement all quantum gates supported by the Uranium platform [8].

#### Open Issues
- A quantum device has the ability to execute commuting operations to a quantum state in parallel. The same applies to measurements made on nonoverlapping memory addresses. It is expected that a quantum hardware controller has the ability to schedule gates in a manner that is optimal and this includes scheduling commuting operations. A language is typically written in a serial fashion but the ordering of instructions may vary when the program is executed in a parallel fashion. It is unclear if instruction parallelism semantics should be introduced at the lambdaQ language level. As a side note, OpenQASM3 has a very rich feature set for platform-dependent tuning of quantum instructions at the physical level including timing and optimization of operations, synchronization among different operations, and calibrating quantum instructions at the pulse level.

### Execution Model for Quantum Computation
In order to be able to make some rough plans regarding how a practical quantum computing language should be implemented it is useful to have a more precise image of how a real quantum device is expected to work, more precisely, which are the different phases for code execution. To this date, one of the most detailed descriptions was provided by IBM: [11, 19]. The LLVM QIR also supports integration and arbitrary interactions between quantum and classical computing resources which is intended to work with both the current limited quantum devices and future, more sophisticated, quantum computing resources.
 
- Compilation takes place on a classical computer where the lambdaQ source code will be processed into some combination of classical and quantum code expressed in a high-level IR. Two IR formats are being considered: OpenQasm3 and LLVM quantum IR on the one hand and classical code IR generated using Haskell LLVM bindings on the other. At this phase, specific problem parameters may not be known.

- Further circuit generation can be executed on a classical computer, in an environment where concrete problem parameters are known and some interaction with the quantum device is in place. This is an online phase where runtime problem parameters are known. Possibly new circuits will be generated here. The output is a collection of quantum circuits, classical control instructions (e.g classically controlled gates), and classical object code.

- Real-time code execution will be handled by a high-level controller that translates input if received as circuits and other run-time parameters expressed in some high-level IR format (e.g. OpenQasm3) into lower-level instructions (e.g. pules) to be executed by a low-level controller. After circuit execution, data in form of measurement results are provided back to the high-level controller. In general, the high-level controller can execute both classical control instructions from the IR code and external object code.

- A final post-processing step is expected to take offline on a classical computer after real-time processing is complete. This will take a collection of measurement results from the high-level controller as input and either output the final results or process the intermediate results for further circuit generation if the computation is not yet finished. 

### Interaction Between Classical and Quantum Domains
Fully general classical computation cannot be executed within the limited coherence time of a quantum device. Two time-scales for quantum-classical interaction can be identified:

- real-time: classical computation that must be performed within the coherence time of the qubits and possibly take advantage of feedback and feedforward. This domain in general is constrained by available memory or reduced clock speeds. Concurrency between classical and quantum and parallelism are essential here in order to be able to implement features like quantum error correction.

- near-time: computation that assumes more generic computing resources including access to various libraries and runtimes. This domain has less stringent constraints on memory and clock speed restrictions. Near-time computation should have low latency access to quantum hardware.

Following OpenQASM3 [11] a quantum program is defined as a model of computation where a classical computer interacts with a Quantum Processing Unit (QPU) used as a coprocessor. An extended quantum circuit should be understood as a sub-unit of a full quantum program and is a computational routine consisting of an ordered sequence of quantum operations (gates, measurements, and resets) on quantum data (qubits) and concurrent real-time classical operations. Data flow back and forth between quantum operations and real-time classical operations can depend on measurement results and quantum operations may involve or be conditioned on data generated by real-time classical computations. An extended quantum circuit can be completely specified in a quantum program IR and does not contain code to be executed in the near-time computation domain.

### Bibliography and Relevant Resources
[1] [Lecture Notes on the Lambda Calculus, Peter Selinger.](https://arxiv.org/pdf/0804.3434.pdf)   
[2] [A lambda calculus for quantum computation with classical control, Peter Selinger and Benoit Valiron.](https://arxiv.org/pdf/cs/0404056.pdf)    
[3] [Quantum Lambda Calculus, Peter Selinger and Benoit Valiron.](https://www.mscs.dal.ca/~selinger/papers/qlambdabook.pdf)  
[4] https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts.html  
[5] [QIR Alliance](https://www.qir-alliance.org/) and [QIR Alliance GitHub.](https://github.com/qir-alliance)  
[6] [BNFC converter.](http://bnfc.digitalgrammars.com/)  
[7] [Labeled BNF grammar.](https://bnfc.readthedocs.io/en/latest/lbnf.html#introduction)  
[8] [Uranium, a visual quantum computing platform.](https://uranium.transilvania-quantum.org/)  
[9] [Quipper website.](https://www.mathstat.dal.ca/~selinger/quipper/)  
[10] [Silq website.](https://silq.ethz.ch/)  
[11] [OpenQASM3 specifications.](https://arxiv.org/pdf/2104.14722.pdf)  
[12] [Automatic Generation of Grover Quantum Oracles for Arbitrary Data Structures.](https://arxiv.org/abs/2110.07545)  
[13] [A functional quantum programming language, Thorsten Altenkirch, and Jonathan Grattage.](http://www.cs.nott.ac.uk/~psztxa/publ/qml.pdf)  
[14] [QWIRE: a core language for quantum circuits, Jennifer Paykin, Robert Rand, Steve Zdancewic.](https://dl.acm.org/doi/10.1145/3093333.3009894)  
[15] https://github.com/NicklasBoto/funQ    
[16] [Conventions for quantum pseudocode, E. Knill.](https://www.osti.gov/servlets/purl/366453-CZpmV6/webviewable/)  
[17] https://quantum-computing.ibm.com/  
[18] [Quantum conditional operations, Alessandro Bisio, Paolo Perinotti.](https://arxiv.org/pdf/1509.01062.pdf)  
[19] [Open Quantum Assembly Language, Andrew W. Cross, Lev S. Bishop, John A. Smolin, Jay M. Gambetta.](https://arxiv.org/abs/1707.03429)  
[20] [Quipper: A Scalable Quantum Programming Language, Alexander S. Green, Peter LeFanu Lumsdaine, Neil J. Ross, Peter Selinger, Benoît Valiron.](https://arxiv.org/abs/1304.3390)  
[21] [Compiling Quantum Conditionals in a Functional Language, Nicklas Boto Fabian Forslund.](https://odr.chalmers.se/bitstream/20.500.12380/304420/1/Bot_Forslund_Compiling_quantum_conditionals_in_a_functional_langauge_Signe.pdf)  
[22] [A tutorial introduction to quantum circuit programming in dependently typed Proto-Quipper, Peng Fu, Kohei Kishida, Neil J. Ross, Peter Selinger.](https://arxiv.org/pdf/2005.08396.pdf)  
[23] [Elementary gates for quantum computation, Barenco et all.](https://arxiv.org/pdf/quant-ph/9503016.pdf)  
[24] https://www.ibm.com/blogs/research/2021/02/quantum-phase-estimation  
[25] [QCL Language, Bernhard Omer, PhD Thesis.](http://tph.tuwien.ac.at/~oemer/doc/structquprog.pdf)  
[26] [Quantum Programming Languages, Dominique Unruh.](https://kodu.ut.ee/~unruh/publications/quantum-programming-languages.pdf)   
[27] [A Functional Quantum Programming Language, Matilda Blomqvist, Nicklas Botö, Beata Burreau, Fabian Forslund, Marcus Jörgensson, Joel Rudsberg.](https://odr.chalmers.se/handle/20.500.12380/304169)  
[28] [An Overview of QML With a Concrete Implementation in Haskell, J. Grattage.](https://arxiv.org/abs/0806.2735)  
[29] [On Quantum Lambda Calculi: a Foundational Perspective, Margherita Zorzi.](https://www.academia.edu/2370968/On_Quantum_Lambda_Calculi_a_Foundational_Perspective)  
[30] [Linear-algebraic Lambda-calculus: higher-order, encodings and confluence, Pablo Arrighi, Gilles Dowek.](https://arxiv.org/abs/quant-ph/0612199v1)  
[31] [From Symmetric Pattern-Matching to Quantum Control, Amr Sabry, Benoıt Valiron, Juliana Kaizer Vizzotto.](https://arxiv.org/pdf/1804.00952.pdf)  
[32] Foundations of Quantum Programming, Mingsheng Ying  
[33] [The Vectorial λ-Calculus, Pablo Arrighi, Alejandro Dıaz-Carob, Benoıt Valiron.](https://arxiv.org/pdf/1308.1138.pdf)  
[34] [The Algebraic Lambda-Calculus, Linoel Vaux.](https://www.i2m.univ-amu.fr/perso/lionel.vaux/pub/alglam.pdf)  





 
 
