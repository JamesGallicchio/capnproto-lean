import CapnProtoLean.CapnpText
import CapnProtoLean.Schema

namespace CapnProtoLean

open Lean Elab Command in
run_cmd do
  let filename := "CapnProtoLeanTest/reflect/schema.text"
  let text ← IO.FS.readFile filename
  let syn : TSyntax `capnptext ←
    match
      Parser.runParserCategory
        (← get).env
        `capnptext
        text
        filename
    with
    | .ok s => pure (TSyntax.mk s : TSyntax `capnptext)
    | .error e => throwError e
  elabCommand (← `(
    def $(mkIdent `capnpSchemaText) : $(mkIdent ``CapnpText) := (capnptext| $syn)
  ))

#check capnpSchemaText

#eval show IO Unit from do
  match CodeGeneratorRequest.fromText capnpSchemaText with
  | .ok e => IO.println (repr e)
  | .error e => IO.println e
