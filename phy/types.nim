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
  std/[
    algorithm
  ],
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
    tkTuple ## an anonymous product type
    tkUnion ## an anonymous sum type

  SemType* = object
    ## Represents a source-language type. The "Sem" prefix is there to prevent
    ## name conflicts with other types named `Type`.
    case kind*: TypeKind
    of tkError, tkVoid, tkUnit, tkBool, tkInt, tkFloat:
      discard
    of tkTuple, tkUnion:
      elems*: seq[SemType]

const
  ComplexTypes* = {tkTuple, tkUnion}
    ## types that can currently not be used as procedure return or parameter
    ## types in the target IL

proc cmp*(a, b: SemType): int =
  result = ord(a.kind) - ord(b.kind)
  if result != 0:
    return

  # same kind, compare operands
  case a.kind
  of tkError, tkVoid, tkUnit, tkBool, tkInt, tkFloat:
    result = 0 # equal
  of tkTuple, tkUnion:
    result = a.elems.len - b.elems.len
    if result != 0:
      return

    for i in 0..<a.elems.len:
      result = cmp(a.elems[i], b.elems[i])
      if result != 0:
        return

    result = 0 # the types are equal

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
  of tkTuple, tkUnion:
    result = a.elems == b.elems

proc union*(types: varargs[SemType]): SemType =
  ## Constructs a type that is the sum of all `types`.
  var elems: seq[SemType]
  for it in types.items:
    # for union types, the elements need to be sorted, so that
    # ``union(int, float)`` and ``union(float, int)`` are the same type
    let i = lowerBound(elems, it, cmp)
    if i >= elems.len or elems[i] != it:
      elems.insert(it, i)

  if elems.len == 1:
    # no union type necessary
    # XXX: I'm not sure whether this is a good idea...
    result = elems[0]
  else:
    result = SemType(kind: tkUnion, elems: elems)

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
  of tkUnion:
    var s = high(int)
    for it in t.elems.items:
      s = min(s, size(it))
    s + 8 # +8 for the tag
