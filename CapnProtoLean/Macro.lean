import Batteries
import Qq

import CapnProtoLean.Schema

namespace CapnProtoLean.Macro

open Lean CodeAction Elab Command Meta Qq

structure State where
  nameMap : Batteries.HashMap Id Name

def typeToLean (t : «Type») (state : State) : TermElabM Term := do
  match t with
  | .mk body =>
  match body with
  | .void    => return mkIdent ``Unit
  | .bool    => return mkIdent ``Bool
  | .int8    => return mkIdent ``Int8
  | .int16   => return mkIdent ``Int16
  | .int32   => return mkIdent ``Int32
  | .int64   => return mkIdent ``Int64
  | .uint8   => return mkIdent ``UInt8
  | .uint16  => return mkIdent ``UInt16
  | .uint32  => return mkIdent ``UInt32
  | .uint64  => return mkIdent ``UInt64
  | .float32 => return mkIdent ``Float32
  | .float64 => return mkIdent ``Float64
  | .text    => return mkIdent ``Text
  | .data    => return mkIdent ``Data
  | .list elementType  => return Syntax.mkApp (mkIdent ``List.P) #[← typeToLean elementType state]
  | .enum typeId brand =>
    let some name := state.nameMap.find? typeId
        | throwError "typeToLean called on unrecognized enum {typeId}"
    `($(mkIdent name))
  | .struct typeId brand =>
    let some name := state.nameMap.find? typeId
        | throwError "typeToLean called on unrecognized struct {typeId}"
    `($(mkIdent name))
  | .interface typeId brand =>
    throwError "typeToLean called on interface (not yet supported)"
  | .anyPointer a =>
    throwError "typeToLean called on anypointer (unsure where this comes up!)"

def genTopLevel (c : CodeGeneratorRequest) : CommandElabM (TSyntaxArray `command) := do
  let {nodes,capnpVersion:=_, sourceInfo:=_, requestedFiles:=_} := c
  let state : State := Id.run do
    let mut res := .empty
    for n in nodes do
      match n with
      | .mk (id := id) (displayName := displayName) (body := body) .. =>
      match body with
      | .struct .. =>
        let tyName : Name := .mkSimple displayName
        res := res.insert id tyName
      | _ => pure ()
    return {nameMap := res}
  -- Generate declarations to declare all the type aliases we need
  let typeDefs : Syntax.TSepArray `command "\n" ← (do
    let mut res := #[]
    for n in nodes do
      match n with
      | .mk (id := id) (body := .struct dataWordCount ptrCount ..) .. =>
        let name := state.nameMap.find! id
        res := res.push (← `(
          def $(mkIdent name) : Type := $(mkIdent ``Struct)
        ))
        res := res.push (← `(
          instance : $(mkIdent ``Struct.IsStruct) $(mkIdent name) where
            $(mkIdent `fromStruct) := $(mkIdent ``id)
            $(mkIdent `expectedDataWords) := $(Syntax.mkNumLit <| toString dataWordCount)
            $(mkIdent `expectedPtrWords) := $(Syntax.mkNumLit <| toString ptrCount)
        ))
      | _ => continue
    return res)
  let methodDefs : TSyntaxArray `command ← (do
    let mut res := #[]
    for n in nodes do
      match n with
      | .mk (id := id) (body := body) .. =>
      match body with
      | .struct dataWordCount pointerCount _preferredListEncoding _isGroup
          discriminantCount discriminantOffset
          fields =>
        let tyName := state.nameMap.find! id
        for f in fields do
          match f with
          | .mk (name := name) (body := body) .. =>
          match body with
          | .slot offset type .. =>
            let type : Term :=
              Syntax.mkApp (mkIdent ``DecodeM) #[← liftTermElabM <| typeToLean type state]
            let impl : Term :=
              Syntax.mkApp (mkIdent ``Struct.HasStructAccessor.get)
                #[mkIdent `self, Syntax.mkNumLit <| toString offset]
            res := res.push (← `(
              def $(mkIdent <| tyName.str name) ($(mkIdent `self) : $(mkIdent tyName)) : $type := $impl
            ))
          | .group fieldTypeId =>
            pure ()
      | _ => pure ()
    return res)

  /-for n in nodes do
    match n with
    | .mk id displayName displayNamePrefixLength scopeId
        parameters isGeneric nestedNodes annotations body =>
    logInfo m!"Processing {displayName}"
    match body with
    | .file => continue
    | .const type value =>
      logError "const not yet supported"
    | .enum enumerants =>
      logError "enum not yet supported"
    | .struct dataWordCount pointerCount preferredListEncoding isGroup
        discriminantCount discriminantOffset
        fields =>
      let tyName : Name := .mkSimple displayName
      typeDefs := typeDefs.push (← `(
        def $(mkIdent tyName) : Type := Struct
      ))
      let fields : TSyntaxArray `Lean.Parser.Term.bracketedBinder :=
        fields.map (fun f =>
          match f with
          | .mk name codeOrder annotations discriminantValue body ordinal =>
          match body with
          | .slot offset type defaultValue hadExplicitDefault =>
            logError "const not yet supported"
          | .group typeId =>
            sorry
        )
      typeDefs := typeDefs.push (← `(command|
        inductive $(mkIdent tyName) where
        | mk $[$fields]*
      ))
    | .annotation type .. =>
      sorry
    | .interface methods superclasses =>
      sorry
  -/
  return typeDefs ++ methodDefs

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
  "#generate_capnproto " term (" {{" (ppLine command)* ppDedent(ppLine "}}") )? : command

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
    #generate_capnproto $files {{
      $cmds:command*
    }}
  )
  let parenthesized ← liftCoreM <| Lean.PrettyPrinter.parenthesizeCommand syn
  return ← liftCoreM <| Lean.PrettyPrinter.formatCommand parenthesized

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
