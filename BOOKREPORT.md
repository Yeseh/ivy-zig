# Notes for bookreport XKE

## General Learning Goals
- Get more comfortable with Zig
- Learn about compilers, optimalization/benchmarking, and low-level programming in general

### Implementation Goals
- Have a custom programming/scripting language that can be useful to myself 
    - Should include a basic standard library that can manipulate files/dirs for example
    - Maybe a web(socket) server for APIs
    - Doesn't have to do anything special/unique, not meant to be used by other people
- Build a 'Language playground' website that can run the interpreter/compiler in a browser
- Implement the Language Server Protocol
    - Perhaps publish a VSCode extension 
- Implement a WebAssembly compiler backend (binaryen?)

## Implementation proces
1. Read the book chapter, think about the challenges, generally don't implement them (this is for when the book is done and I'm personalizing the language)
2. Implement in C-style zig to learn the concepts
3. Unit test chapter material 
4. Reflect and try to find a more 'ziggy' implementation
    - This might lead to some slightly less performant solutions due to skill issues;
    - Have not really benchmarked yet, will do this at the end of the book;
5. Refactor to this more idiomatic solution or mark a TODO for later if I don't feel like redoing at that moment.
6. Fix/verify unit tests still work
7. Extend and fix end-to-end tests where needed

## Important learnings
To summarize what I've learnt in this project:

- Object-oriented programming is fundamentally incompatible with performance at the CPU level
- Getting more intimately familiar with low level semantics, pointers, manual memory management
- How object inheritance works
- How hashtables/dictionaries work
- More proof-based programming
    - The algorithms and data structures used have very well-defined problem spaces
    - Up front (or at least early), and intensive testing is very important in projects like these

## Biggest challenges 
- String / Object implementation took a while, really ran into a lot of segfaults/mem leaks in this part.
    - Mostly came down to using a different allocator, ArenaAllocator doesn't work like malloc/free.
    - Null termination also works very differently in Zig compared to C. It is baked into the type system, so getting this right was very difficult.
    - Not reading over the primitive garbage collector implemented in the strings chapter would also have helped a lot.
- It is idiomatic in Zig to use way fewer pointers than C. This leads to some translation issues every now and then.

## Quick primer on zig

### General
- A modern take on C, IE not 50 years old
    - Very similar level of abstraction from the machine
    - Translating C to zig works pretty well with a bit of work, but it is not really idiomatic to write zig that way.
    - For example: you can use many-item pointers but this is not really idiomatic, I've done this in a couple of places to be closer to the book implementation while I was figuring things out.
    - Table.zig is a good example of a more idiomatic zig solution (afaik) using unions

- Tries to fix the issues C has
    - More restrictions that are opt-in
        - Some examples are many-item pointers vs slices, and a type system that is way more restrictive. In C you can cast pointers to w/e you feel like, not so much in zig. It doesn't really get in your way though.
        - This is in contrast to Rust where you HAVE to do what the borrow checker says is safe, unless you opt-out with an unsafe block

### Errorhandling 
- Try syntax
- Error sets

### Memory handling
- Low level AF
- Different pointer types
- No hidden allocations
- Explicit passing of allocators

### Comptime
- Its beautiful, I love it.
    - Very elegant, you can just write normal Zig code without a lot of extra syntax needed
    - Type reflection is *chefskiss*
- Generics
- Compile time evaluation of return type expressions (see std/mem/Allocator)
    - Kind of nuts, not really sure if I should be disgusted or amazed (perhaps both)

### Build system
- Did not get much use out of this but love the fact that I can just write Zig to build an app.

## Musings 
Some random, slightly unrelated thoughts/subjects that I explored during this project:

- Memory encapsulation within systems (IE table.zig)
    - The system/data structure owns the memory it uses
    - Pointers into a system's internal memory are pretty unsafe, IE ptrs to array elements become invalid when resizing
    - C++ smart pointers or Rust's borrowing/lifetimes are a solution to this
        - Zig doesn't have something similar, and doesn't need to, so extra care is needed
    - The system can instead opt to give out safe 'handles' to callers
        - Handles are just indexes into the internal array, and therefore safe(er) to use
        - Handles can be converted to pointers, but then we lose safety, this can be mitigated in other ways (TODO)
        - Handles can be smaller than actual pointers, pointers are BIG on 64-bit systems, so u32 (or smaller) can be used instead
    - OOP doing the right thing for once, but people forget why this can be useful 
        - "Encapuslation is just what you do"
        - Private by default is preemptively putting the gutter blockers in the bowling alley

- Programming with assumptions
    - If you can prove that a function is never called with an invalid state beforehand, you don't have to validate the state inside the function
    - Functions in the tokenizer for example expect the program to be in a certain state or it just won't work. But the state is never validated. The functions 'assume' that the tokenizer is in a state they can act upon, because the functions themselves represent the state the tokenizer is in. They should never be called when the functions can't act.
    - Naively handling every possible error case in every function is cumbersome and can lead to performance overhead and lots of useless LoC.
    - This is not as easy when the program relies on fickle user input
        - In this case the program states should also be able to reflect faulty input
    - Testing is important for this!

- Understanding that low-level programming is not necessarily harder than 'business' programming
    - Just a different way of thinking that requires time and practice
    - Very different problem spaces that have different types of complexity
    - 'Business' code is probably easier to write, but the complexity comes from the business domain and all the interconnected external systems (both human and technical). Whether that complexity is necessary is another question...
    - Low-level code is more about numbers, but it mostly is addition and multiplication
        - Everyone that has can apply bullshit OO design patterns or, god forbid, CSS, could do it.

- I'm watching GitHub copilot get better at Zig right before my eyes. 


## References
- https://floooh.github.io/2018/06/17/handles-vs-pointers.html