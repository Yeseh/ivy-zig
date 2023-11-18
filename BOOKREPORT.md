# Notes for bookreport XKE

## Learning Goals
I have the following general learning goals
- Buid a non-trivial project in a systems-level language
- Get more comfortable with Zig

### Project goals
- Have a custom programming/scripting language that can do usefull stuff on the OS
    - Should include a basic standard library that can manipulate files/dirs
    - Maybe a web(socket) server for APIs
- Build a 'Language playground' website that can run the interpreter/compiler in a browser
- Implement the Language Server Protocol
    - Perhaps publish a VSCode extension 
- Implement a WebAssembly compiler backend (binaryen?)

## Quick primer on zig

### General
- Slices
- 


### Errorhandling 
- Try syntax
- Error sets

### Memory handling
- Low level AF
- Different pointer types
- No hidden allocations
- Explicit passing of allocators

## Musings 
- Memory encapsulation within systems (IE table.zig)
    - The system owns the memory it uses
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
    - Naively handling every possible error case in every function is cumbersome and can lead to performance overhead and lot of useless LoC.
    - This is not as easy when the program relies on user input
        - In this case the program states should also be able to refl
    - Testing is important for this!

- I'm training Copilot to translate C to Zig, it got better as I went along


## References
- https://floooh.github.io/2018/06/17/handles-vs-pointers.html