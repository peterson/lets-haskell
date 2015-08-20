# Let's Haskell

### Written by David Peterson

### Introduction

### Getting Help

There are two mailing lists for asking questions. All questions are welcome,
however, your first post might be moderated. This is simply to prevent spam.

1. \#haskell-beginners [on Freenode](irc://irc.freenode.net/#haskell-beginners)
   is an IRC channel that is operated by others who are keen to share ideas
   relating to functional programming in Haskell. They are in various timezones
   and share a passion for functional programming, so may be able to provide
   relatively quick assistance with questions.


### Getting Started

1. Install the Glasgow Haskell Compiler (GHC). Version 7.8 or higher is recommended.

2. Change to the directory containing this document.

3. Execute the command `ghci`, which will compile and load all the source code.
   You may need to set permissions on a file, `chmod 600 .ghci`.

4. Inspect the modules to get a feel for Haskell's syntax, then move on to the
   exercises starting with `Lets.Data.Basics`. The [Progression](#progression)
   section of this document lists the recommended order in which to attempt the
   exercises.

5. Edit a source file to a proposed solution to an exercise. At the `ghci`
   prompt, issue the command `:reload`. This will compile your solution and
   reload it in the GHC interpreter. You may use `:r` for short.


### Progression

It is recommended to perform some exercises before others. The following
progression is recommended:

Day 1:

* `Lets.Go` - ops, basic types, to/from digits, div, mod, min, max ... etc.
* `Lets.Function`
* `Lets.List`
* `Lets.Data`
* (more) coming soon ...

Day 2:

* coming soon ...

Day 3:

* coming soon ...

Answers to the exercises can be found on the answers branch, here:
[https://github.com/peterson/lets-data/tree/answers](https://github.com/peterson/lets-data/tree/answers)

### Acknowledgements

Thanks to Tony Morris (and co-contributors) for the NICTA course, which provides both
the template and the motivation for this course.

### References

* [CIS 194 Spring '13](https://www.seas.upenn.edu/~cis194/spring13/) - Brent Yorgey's excellent introductory Haskell course, which covers many of the same topics in this tutorial in greater detail.

* [NICTA Course](https://github.com/nicta/course) - A foundational course on functional programming concepts using Haskell.

* [Learn You a Haskell](http://learnyouahaskell.com/) - An approachable textbook covering the basics of Haskell.

* [Real World Haskell](http://http://book.realworldhaskell.org/) - Textbook covering both beginner and advanced topics in Haskell.

* [Glasgow Haskell Compiler](http://haskell.org/ghc)
