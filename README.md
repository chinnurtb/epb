# epb [![Build Status](https://travis-ci.org/seancribbs/epb.png)](https://travis-ci.org/seancribbs/epb)

Erlang protocol buffers library. This is born out of rage against
[erlang_protobuffs](//github.com/basho/erlang_protobuffs), which uses a (working
but brain-dead) compilation strategy that is brittle and hard to
evolve. The project also has lots of technical debt from lack of
vision and inconsistency of multiple authors adding new features.

This is a work-in-progress, use at your own peril.

## Goals

1. Use classic compiler techniques to turn Protocol Buffers
   definitions into Erlang source. We'll likely make use of the
   `syntax_tools` application to accomplish this. This will also
   simplify usage of other tools like Dialyzer because the library
   user will be able to compile the source directly, instead of the
   library manipulating abstract code directly. Also, no more Pokemon!
2. Be a drop-in replacement for `erlang_protobuffs` in most
   circumstances. Some function names may change or move around, but
   it should be just as easy to use.
3. Aggressively optimize the encoding and decoding of messages,
   avoiding unnecessary copying, list-traversals, etc that have
   plagued the `erlang_protobuffs` codebase in the past. Reduce the
   overall footprint of in-memory message representations, perhaps
   taking advantage of zero-copy binaries where possible.
4. Comply with all aspects of the Protocol Buffers specification at
   its current version.
5. Avoid making me puke everytime I have to read or modify the
   codebase.
