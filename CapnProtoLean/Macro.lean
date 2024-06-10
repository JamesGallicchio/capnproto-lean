import Batteries
import Qq

import CapnProtoLean.Schema

namespace CapnProtoLean

open Lean CodeAction Elab Command Meta Qq

def genTopLevel (c : CodeGeneratorRequest) : CommandElabM (TSyntaxArray `command) := do
  let {nodes,capnpVersion:=_, sourceInfo:=_, requestedFiles:=_} := c
  let mut types : TSyntaxArray `command := #[]
  let mut decoders : TSyntaxArray `command := #[]
  for n in nodes do
    match n with
    | .mk id displayName displayNamePrefixLength scopeId parameters isGeneric nestedNodes annotations body =>
    logInfo displayName

  let mutualTypes ← `(command|
    mutual
    $types:command*
    end
  )
  let mutualDecs ← `(command|
    mutual
    $decoders:command*
    end
  )
  return #[mutualTypes, mutualDecs]

def readAndGen (files : Term) : CommandElabM (TSyntaxArray `command) := do
/-
  let files ← liftTermElabM <| do
    let expr ← Term.elabTerm files (some q(Array System.FilePath))
    unsafe evalExpr (Array System.FilePath) (q(Array System.FilePath)) expr

  for p in files do
    if !(← p.pathExists) then
      throwError "path {p} does not exist"
    if ← p.isDir then
      throwError "path {p} is a directory"

  let capnpc ← IO.Process.spawn {
    cmd := "capnpc"
    args := files.map (·.toString) ++ #["-o-"]
    stdout := .piped
    stderr := .piped
  }

  let out : IO.FS.Handle := capnpc.stdout

  let msg ← try
    let msg : Message ← liftM (m := IO) <| Message.fromHandle out

    logInfo m!"successfully parsed message: {msg.segments.map (·.data.size)}"
    let remaining ← out.readBinToEnd
    logInfo s!"{remaining.size} bytes left in stream"
    pure msg
  catch e =>
    logError m!"Error parsing standard out: {e.toMessageData}"
    let stderr ← capnpc.stderr.readToEnd
    logError m!"Standard error output from capnc: {stderr}"
    logError m!"capnpc returned {← capnpc.wait}"
    throw e

  let req : CodeGeneratorRequest ←
    match decode CodeGeneratorRequest.decoder msg with
    | .ok (some req) => pure req
    | .ok none => throwError "root was none?"
    | .error e =>
      throwError s!"decode error: {e}"
-/
  let req ← liftTermElabM <| do
    let expr ← Term.elabTerm files (some q(CodeGeneratorRequest))
    unsafe evalExpr (CodeGeneratorRequest) (q(CodeGeneratorRequest)) expr


  genTopLevel req


syntax (name := generate_capnproto)
  "#generate_capnproto " term (" {{ " command* "\n}}" )? : command

deriving instance TypeName for String

@[command_elab generate_capnproto]
def genCapnProtoHandler : CommandElab := fun stx => do
  match stx with
  | `(command| #generate_capnproto $files:term ) =>
    let res ← readAndGen files
    let fmt ← format files res
    pushInfoLeaf (.ofCustomInfo {
        stx := ← getRef,
        value := Dynamic.mk fmt.pretty })
    for c in res do elabCommand c
  | `(command| #generate_capnproto $files:term {{ $c:command* }} ) =>
    let res ← readAndGen files
    if res != c then
      let fmt ← format files res
      pushInfoLeaf (.ofCustomInfo {
        stx := ← getRef,
        value := Dynamic.mk fmt.pretty })
    for c in c do elabCommand c
  | _ =>
    throwUnsupportedSyntax
where format (files : Term) (cmds : TSyntaxArray `command) : CommandElabM Format := do
  let syn ← `(command|
    #generate_capnproto $files {{ $cmds:command* }}
  )
  return ← liftCoreM <| Lean.PrettyPrinter.formatCommand syn

open Server RequestM in
@[command_code_action generate_capnproto]
def guardMsgsCodeAction : CommandCodeAction := fun _ _ _ node => do
  let .node _ ts := node | return #[]
  let res := ts.findSome? fun
    | .node (.ofCustomInfo { stx, value }) _ => return (stx, (← value.get? String))
    | _ => none
  let some (stx, newText) := res | return #[]
  let eager := {
    title := "Update #generate_capnproto with correct output"
    kind? := "quickfix"
    isPreferred? := true
  }
  let doc ← readDoc
  pure #[{
    eager
    lazy? := some do
      let some start := stx.getPos? true | return eager
      let some tail := stx.getTailPos? true | return eager
      pure {
        eager with
        edit? := some <| .ofTextEdit doc.versionedIdentifier {
          range := doc.meta.text.utf8RangeToLspRange ⟨start, tail⟩
          newText
        }
      }
  }]
