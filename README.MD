# mexirun

My Rust implementation of a [mexico](https://www.github.com/maride/mexico) interpreter. The parser is not great, it tends to fail silently. The program first converts the text to an intermediate representation and then executes that. It is way faster than the python implementations that were written at the Labor :)

It was created to solve the Labortage CTF challenges mexico-2 and mexico-3. It was then used to create [mexico-4](https://www.github.com/noobtracker/mexico-4).

Usage: `cargo run (--release) [script.mxc]`

`fuxxor.mxc` is generated every time a program is executed, it is a randomly obfuscated version of the input. Obfuscating already obfuscated code breaks it ^^
