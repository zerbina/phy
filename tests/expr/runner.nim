## Test runner for source language expression tests.

import
  std/[
    os,
    streams,
    strutils
  ],
  experimental/[
    sexp
  ],
  passes/[
    changesets,
    pass0,
    pass1,
    pass3,
    pass4,
    pass10,
    source2il,
    spec_source,
    trees
  ],
  common/[
    vmexec
  ],
  vm/[
    vmenv,
    vmvalidation
  ]

let
  args = getExecArgs()
  s = openFileStream(args[^1], fmRead)

# skip the test specification:
if s.readLine() == "discard \"\"\"":
  while not s.readLine().endsWith("\"\"\""):
    discard
else:
  s.setPosition(0)

var ctx = source2il.open()
# parse the S-expression and translate the source language to the L1:
let typ = ctx.exprToIL(fromSexp[NodeKind](parseSexp(readAll(s))))
# don't continue if there was an error:
if typ == tkError:
  echo "exprToIL failed"
  quit(1)

var tree = close(ctx)
# lower to the L0 language:
tree = tree.apply(pass10.lower(tree))
tree = tree.apply(pass4.lower(tree))
tree = tree.apply(pass3.lower(tree, 8))
tree = tree.apply(pass1.lower(tree, 8))

# generate the bytecode:
var env = initVm(1024, 1024 * 1024)
translate(tree, env)

# make sure the environment is correct:
let errors = validate(env)
if errors.len > 0:
  for it in errors.items:
    echo it
  echo "validation failure"
  quit(1)

# run the code and echo the result:
stdout.write run(env, env.procs.high.ProcIndex, typ)
