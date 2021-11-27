
Rhombus Reader.

Rhombus is based on [REBOL](http://www.rebol.com/rebol3/),
[Red](https://www.red-lang.org/),
and [Ren](https://pointillistic.com/ren/),
which will further be referred to as Rebol.

Rebol is both a data format and a scripting language.
A rich set of data types is provided which includes the usual programming language types and adds multiple types of blocks,
quoting,
binary strings,
date and time,
money,
percentages,
and internet protocol endpoints.
"Blocks" of Rebol are read in to a structured document of rich symbols.
These symbols can then be interrogated to represent a specialized data format (called dialects) or run through an interpreter to form the scripting language itself.

Dialecting is a Rebol term for using the same data format but changing how the symbols are interpreted.
For example a simple configuration format can be implemented by simply interpreting pairs of `set-word!` followed by a value as an assigment of some values.
A GUI layout can be specified as a mini-language suited just to laying out how a GUI laid out or how data is bound to the application.
Scripting involves passing the blocks instead to an interpreter which uses it as the instructions to be run.

Rhombus is based heavily on existing Rebol implementations but offers no guarantees of compatibility between them.

The goal is that throughout Iceworks projects the only data format is Rhombus with different dialects to handle specific concerns.

# Usage

```nim
import rhombusreader

let source = slurp("halibut.r")
let block = rhombusreader.read(source)
# visit elements of block ...
```

# Restrictions
 - Only 2, 16 and 64 bit radices are allowed for binary strings.
   They map to reading strings of 0's and 1's,
   hexadecimals,
   and base64 encoded strings respectively.

# Todo
 - Security pass such as making sure over/underflow checks work,
   protecting how many depth levels are allowed,
   making sure binary string decoding is not griefable.
 - Possibly writing a parser generator that consumes Rhombus syntax spec as a Rhombus document and then creates a loader from that.
 - Possibly benchmarking.
   There are advances in SIMD programming where parsers can be made to run exceptionally fast on modern systems.
 - Possibly making sure streaming works;
   we currently assume the whole document is in memory at parse time.

# License
 - MPL 2.0

