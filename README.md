
[![Unitary Fund](https://img.shields.io/badge/Supported%20By-UNITARY%20FUND-brightgreen.svg?style=for-the-badge)](https://unitary.fund)

##  lambdaQ - a functional quantum programming language
A higher-order, functional quantum programming language whose starting point is the [quantum lambda calculus](https://www.mscs.dal.ca/~selinger/papers/qlambdabook.pdf) created by Peter Selinger and Benoît Valiron. This is an ongoing project, very much in the early stages of implementation. The purpose of this document is to answer some general questions I had when I started working on this project. Some additional details regarding the plans made for development can be found in a somewhat older document here: [doc/development-plan.md](https://github.com/radumarg/lambdaQ/blob/main/doc/development-plan.md).


### Why a new programming language?
In the future, with the introduction of error-corrected quantum computers, there will be a need for expressive, high-level quantum programming languages which can then be used to write more readable and less error-prone quantum code. By a high-level quantum language, I understand a language where the code is written preferably, but not exclusively, at an abstraction level above the simple quantum circuit description types of languages that we have all become accustomed to in the NISQ era. Achieving this may be easier said than done, however, here is a set of features that such a language might support:

- start with implementing the [QRAM](https://www.osti.gov/biblio/366453) model of quantum computation, namely the "quantum data and classical control" paradigm.
- add quantum data types with nontrivial semantics using linear and dependent types.
- the type system and implicitly the compiler will prevent a programmer from attempting to make unphysical operations.
- predefined quantum algorithms primitives, for example, phase kick-back, amplitude amplification, or quantum Fourier sampling. 
- non-unitary operations like teleportation as a primitive.
- high-level facilities for constructing and handling circuits.
- parametrized circuits in the form of compiled circuits whose input parameters can be specified at runtime.
- automatic synthesis of boolean oracles.
- automatic uncomputation.
- possibly describe quantum computations using other primitives than quantum gates. [QML](https://arxiv.org/pdf/quant-ph/0409065.pdf) is an example of such language.
- quantum conditionals and possibly quantum recursion if this notion turns out to make sense in a practical setting.
- support for simple classical computing routines to allow for fast pre-processing of circuits and post-processing of measurement results at a time scale comparable to the coherence time of qubits.

### What is the programming model for lambdaQ?
 In the QRAM model of quantum computation classical computers send commands to QPU in the form of circuits and QPU returns measurement results. Creating complicated quantum programs from a classical host language is easy because this is typically a complex, high-level, general-purpose classical language. Proving that the classical computer produces well-formed programs is more difficult for the same reason. A static strong type system can be used to verify the safety of quantum programs during the compilation phase. More precisely a statically typed language has a type system that is checked at compile time with some additional checks to be executed at code generation. Strongly typing means one cannot easily cast one type into another in order to work around the type system. For example, Haskell is strongly typed while C is not since in C any pointer can be cast to another pointer type. Soundness means ensuring that a program can't get into invalid states. A sound type system guarantees that when executing the program, it will not get into a state where an expression evaluates to a value that doesn’t match the expression’s static type. One can enforce soundness using a combination of static checking via compile time errors and possibly additional runtime checks.

### What will be the semantics for lambdaQ?
- the operational semantics of a programming language describes how instructions are executed and the state of the program evolves. LambdaQ has operational semantics, in terms of quantum circuits to be run on quantum processors and classical computational routines running on classical processors like for example FPGAs for an actual quantum computer. Classical and quantum code interact with each other because measurements in a QPU can be used by classical routines generate circuits dynamically and steer the program execution. For the evaluation of lambda terms, a call-by-value strategy is implemented.
- the denotational semantics of a quantum programming language can be used to ensure that a program written in this language is compatible with quantum physics. This is a complicated mathematical problem, and we will ignore it further on.

### A basic example of a higher-level quantum program:
- take some given algebraic function and implement it as a boolean circuit.
- translate the boolean circuit to an oracle implemented as a reversible quantum circuit.
- apply the Grover search algorithm to the output of this oracle.
- copy the result to a scratch register. Copying is possible as long as the function implemented by the oracle is classical.
- uncompute in order to free the ancilla qubits needed to implement the oracle for further reuse.

### What kind of operations can be executed by a quantum computer?
- initialize quantum and classical registers.
- measure and reset quantum registers.
- reuse qubits after reset.
- one should keep in mind that when a quantum variable is discarded, the qubits it points to are implicitly measured which may have an effect on the quantum computation
- reset classical registers.
- create a duplicate of a quantum register (in general duplicates are allowed: |0⟩ + |1⟩ -> |0⟩⊗|0⟩ + |1⟩⊗|1⟩ cloning: |ψ⟩ -> |ψ⟩⊗|ψ⟩ is not).
- apply quantum gates and classically controlled quantum gates.
- mid-circuit measurements should allow a subset of qubits in a quantum device to be measured in order to steer the execution of a quantum program.
- perform relatively simple, fast, classical computation routines on classical registers concurrent with executing quantum gates on QPU.

### What are some high-level facilities for working with circuits?
- automatically handle the raising to a power, exponentiation, or applying controls for any quantum function representing a circuit.
- automatically handle the inverse (adjoint) of a circuit
- type system should complain if we try to take the inverse of a non unitary operation like a circuit containing measurements.
- support for parametrized circuit families.
- automatic synthesis of boolean oracles.
- automatic uncomputation.
- ancilla management: allocate, initialize, deallocate.
- deallocation is necessary either for reusing the ancilla or to avoid doing costly error correction operations on qubits that no longer matter in a computation.
- transform existing quantum circuits into related circuits.
- transpile and optimize quantum circuits.
- circuits can be constructed and modified dynamically based on mid-circuit measurement results.

### What is meant by unphysical operations?
- prevent the user from attempting to make a copy of a variable representing quantum data, an operation which is forbidden by the no-cloning theorem. Usually, this is implemented in a programming language using linear types.
- dependent types are usually used to add to a programming language types representing families of circuits that are indexed by classical parameters.
- a different use of dependent types is using existential dependent quantum data types to hide garbage qubits while guaranteeing that they will be uncomputed.

### What are ancilla qubits?
- ancilla qubits may be used in order to reduce the size or depth of the resulting circuit when decomposing a multiqubit unitary in one and two-qubit physical gates.
- ancilla qubits may also be used as scratch qubits needed to implement boolean oracles.
- ancilla qubits are also needed to implement error correction.

### What is an oracle?
In computer science in general and quantum computing in particular an oracle provides an answer to some problem which is treated as a black-box function. A boolean function is a function that takes one or more binary inputs and produces a boolean output. A boolean oracle is a classical function: f:Bool\*\*n -> Bool\*\*m. Such an oracle can be used to encode algebraic functions like sin or cos for example. Quantum oracles are not limited to boolean functions. In the case of the Grover algorithm, where the task is to search an unsorted database, the oracle may be used to mark a given quantum state we want to identify in a superposition of quantum states, a pattern which is named in literature a phase oracle since we add a phase to the sought-after state. In the case of quantum phase estimation, an oracle is needed to perform a controlled unitary transformation U on a set of qubits whose eigenvalue we want to compute. For implementing Shor's algorithm, an oracle is needed to identify periodicity in quantum Fourier transforms. An oracle is a theoretical construct. However, in the context of a quantum programming language, its implementation usually involves quantum circuits. Other technologies could be used in principle for implementing an oracle but we will not expand on such possibilities here. So, barring the comment made previously, in order to be able to run on a quantum computer a boolean oracle must be implemented as a reversible circuit. One way to do this is by implementing the oracle as f':Bool\*\*(n+m) -> Bool\*\*(n+m) as f'(x,y) = (x, y⊕f(x)) via O(|x⟩⊗|y⟩) = |x⟩⊗|y⊕f(x)⟩. This formulation first ensures that the size of the input register is the same as the output register which opens up the possibility that there could exist a unitary transformation O implementing this operation. Second it ensures that f'(x1,y) != f'(x2,y) in case x1 != x2 which means that the transformation O is invertible which is mandatory, otherwise it could not be implemented by a unitary operation. Moreover, O is unitary by construction and by defining how it acts on basis states we have specified how it acts on a general state. 

### What uncomputation means?
During a quantum program, we often need ancilla qubits to perform different operations, for example in order to implement a boolean oracle. These ancilla qubits typically need to be reused several times throughout a longer quantum program. Discarding a quantum register during computation is physically equivalent to measuring it because implies that a reset operation will be executed on those qubits. This implicit measurement needed during reset may impact the other qubits used to execute the quantum program if their state is entangled with ancilla qubits. This is why dropping temporary quantum data values from the program state requires explicitly applying quantum operations that un-compute those values (note that due to the principle of deferred measurement, keeping those qubits and discarding them at the end of the computation may still impact the final computation result). To be more precise, uncomputing means cleaning up temporary effects on ancilla qubits. Uncomputation is possible in principle because quantum circuits are reversible and consequently a computation can be run in reverse. A useful feature for a quantum programming language would be safe (possibly automatic) uncomputation which means that a temporary quantum variable can be discarded from a program like we can discard without consequences any classical variable. In a general quantum program, not any temporary quantum data can be uncomputed since it may be entangled with multiple registers. Uncomputation can be done safely if the initial evaluation of data to be uncomputed can be described classically, meaning that it does not produce a superposition state out of an input basis state, and also if the input quantum data used for computation is still around unaltered. For example in the [Silq](https://files.sri.inf.ethz.ch/website/papers/pldi20-silq.pdf) quantum programming language, the type system provides necessary support such that the program will fail to compile when attempting to perform an unsafe uncomputation. Picking up the time of uncomputation may be optimized and could be the job of the compiler.
 
### What are the phases of execution of a quantum program?
- quantum program compilation: takes place on a classical computer, offline, meaning no interaction with the QPU is assumed. The output is a combination of classical and quantum code expressed in a high-level intermediate representation (IR) language.
- circuit generation: takes place on a classical computer, online, where all problem parameters are known, and some interaction with the quantum computer may occur. The output is a collection of quantum circuits transpiled for a specific quantum device in a low-level quantum IR and possibly some classical control instructions compiled as object code. Some measurements may be performed, control flow conditions are evaluated, and new circuits may be generated dynamically in hybrid quantum-classical computations like VQE for example.
- circuit compilation and execution: occurs on the quantum computer in real-time while the quantum computer is active. Input is a set of extended quantum circuits (gates, measurements, resets) and concurrent classical control statements. These are sent to various controllers which translate the code to a stream of real-time instruction to be executed by QPU in the case of quantum circuits and to be executed by the FPGAs the case of classical code. Quantum computation is expected to run interactively with classical computation and there may be a data flow back and forth between the quantum and classical domain.

For example when defining the OpenQASM standard, a distinction is made between 'real time' computation which must be executed within the coherence time of qubits (like error correction), and 'near time' computation which has more relaxed timing requirements and can be run during the circuit generation phase. The [Quipper](https://arxiv.org/pdf/1304.3390.pdf) family of programming languages makes the distinction between parameters which are values known at circuit generation times, and states which are values known at circuit run time. States can be either linear (quantum) or non-linear (classical) while parameters are always classical.
 
### Which will be the compilation targets for lambdaQ?
LambdaQ will be compiled to different intermediate representations of quantum programs like [OpenQASM](https://openqasm.com/) and [Quantum Intermediate Representation](https://www.qir-alliance.org/), enabling:
- hybrid classical/quantum programs that support features like mid-circuit measurements and simple classical computation routines concurrent with quantum circuit execution.
- compatibility with multiple quantum hardware providers.

### Taking a peek beyond
LambdaQ is a functional quantum programming language. What is meant by this is that it runs by executing a version of lambda calculus, usually referred to as quantum lambda calculus. It turns out that the set of lambda terms that describe the computation can be given a vector space structure. This means that a quantum program can have as input not only an algorithm but a superposition of algorithms. This falls under a new paradigm: "quantum data and quantum control" which should be contrasted with the "quantum data and classical control" paradigm of the QRAM model. It is not yet clear how to implement this on the existing quantum computers, but at least at a mathematical level, it makes sense.
 
 
 
