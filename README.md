# SICP

This repository contains exercises, notes, and experiments I created to help me
learn the ideas in Abelsson and Sussman's fantastic _Structure and
Interpretation of Computer Programs_.

## Powerful Ideas
Here I list some powerful ideas I learnet while reading the book.

- You can loop with recursion only. No 'for' loops needed
- As much as it is a barrier for others to learn about your code, expressing
    actions in increasing levels of abstraction, as they occur in your program
    and not invented prematurely, is very expressive
- Special forms are needed when the order of evaluation needs to be changed.
- There can be recursive programs programmed in iterative ways (each iteration
    is done after calling the next)
- Dispatch procedures can serve as objects (although I would prefer to use
    proper objects for that)
- One way to structure increasing levels of abstraction is to think about
    abstraction barriers
- Linked lists are more useful than I thought, but just as slow
- Two functions can recursively call each other, which gives a new,
    labyrinthine way to think about recursion. It can be useful sometimes.
- You can separate (abstract away) representation from other operations.
    For exambles, see A Picture Language (section 2.2.4) and
    Symbolic Algebra (section 2.5.4)
- Huffman created a very efficient way of encoding strings of characters.
- The substitution model is simple but unfit for dealing with assignments.
    That's when the environment model comes in.
- Sometimes the wires in a circuit are the objects we decide to model. Objects
    don't always need to correlate to boxy things.
- Concurrency and mutation are a disastrous and often necessary combination.
- Streams are a purely functional way of dealing with time. It works by
    modelling it explicitly, as sequences, intead of sets of states. This
    forces a change in perspective: we can model by thinking about moments in
    time and boundaries of state, or we can model transformations on streams
    of data, by looking at the whole picture and losing the sense of time.
    Neither strategy works for every case.
