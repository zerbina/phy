## Implements types and procedures for representing and working with the
## source-language types.

# XXX: the idea of ``SemType`` is misguided. Instead of using a custom IR, it
#      would be simpler *and* more efficient to use the ouput ``PackedTree`` as
#      the type IR. There's no ``SemType``-to-tree translation step then, and
#      instead of copying around ``SemType``s (which is costly), only a
#      ``NodeIndex`` (referring to the type) would have to be copied around.
#
#      If types are de-duplicated on creation, this would also reduce testing
#      types for equality to an integer comparison

import
  vm/[
    utils
  ]

type
  TypeKind* = enum
    tkError
    tkVoid
    tkUnit
    tkBool
    tkInt
    tkFloat
    tkTuple

  SemType* = object
    ## Represents a source-language type. The "Sem" prefix is there to prevent
    ## name conflicts with other types named `Type`.
    case kind*: TypeKind
    of tkError, tkVoid, tkUnit, tkBool, tkInt, tkFloat:
      discard
    of tkTuple:
      elems*: seq[SemType]

const
  ComplexTypes* = {tkTuple}
    ## types that can currently not be used as procedure return or parameter
    ## types in the target IL

proc errorType*(): SemType {.inline.} =
  SemType(kind: tkError)

proc prim*(kind: TypeKind): SemType {.inline.} =
  ## Returns the primitive type with the given kind.
  SemType(kind: kind)

proc `==`*(a, b: SemType): bool =
  ## Compares two types for equality.
  if a.kind != b.kind:
    return false

  case a.kind
  of tkError, tkVoid, tkUnit, tkBool, tkInt, tkFloat:
    result = true
  of tkTuple:
    result = a.elems == b.elems

proc size*(t: SemType): int =
  ## Computes the size-in-bytes that an instance of `t` occupies in memory.
  case t.kind
  of tkError, tkVoid: unreachable()
  of tkUnit, tkBool: 1
  of tkInt, tkFloat: 8
  of tkTuple:
    var s = 0
    for it in t.elems.items:
      s += size(it)
    s
