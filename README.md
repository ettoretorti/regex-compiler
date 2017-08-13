# regex-compiler
A project that compiles regular expressions to C recognizers. Requires ghc and Parsec to build.

The compiler works by generating a DFA from a regular expression through the use of derivatives (https://dl.acm.org/citation.cfm?id=321249), and then mapping the DFA directly to C code. An advantage of this approach is that it becomes easy to support the complement and intersection of regular expressions.

There is no guarantee that the DFA for a regular expression will be minimal, but it tends to be minimal in practice, and modern C compilers tend to do a fairly good job at removing useless states.

### Regex Syntax
```
Regex = character literal
      | '$'               # matches the empty string
      | Regex '|' Regex   # matches if either of the two match
      | Regex '&' Regex   # matches if both of the two match
      | Regex Regex       # matches if both match in succession
      | Regex '*'         # matches zero or more occurences
      | Regex '+'         # matches one or more occurences
      | Regex '?'         # matches zero or one occurences
      | '!' Regex         # matches the complement
      | '(' Regex ')'     # parenthesis for grouping
```
The special characters |&!()*?+$\ have to be escaped by preceding them with a '\\' if they are to be used as literals. The operators are listed in ascending order of precedence i.e. '|' has a lower precedence than '&'. Use parenthesis to provide explicit groupings that go against operator precedence.

### Usage
```
./compile.sh
./RegexCompiler "(a|b|c)&!c" > recognizer.c
```
