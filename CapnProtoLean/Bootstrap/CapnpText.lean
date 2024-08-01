import Batteries

import CapnProtoLean.Schema

namespace CapnProtoLean

deriving instance Repr for Batteries.AssocList

inductive CapnpText
| struct : Batteries.AssocList String CapnpText → CapnpText
| arr : Array CapnpText → CapnpText
| str : String → CapnpText
| num : Nat → CapnpText
| id : String → CapnpText
| opaquePtr : CapnpText
| default : CapnpText
deriving Repr

declare_syntax_cat capnptext

syntax "( " (ident " = " capnptext),* " )" : capnptext
syntax "[ " capnptext,* " ]" : capnptext
syntax str : capnptext
syntax num : capnptext
syntax ident : capnptext
syntax "<opaque pointer>" : capnptext

syntax "(capnptext| " capnptext " )" : term

macro_rules
| `( (capnptext| ( $[$i:ident = $c:capnptext],* ) ) ) => do
  let map ← (i.zip c).foldrM
    (fun (i,c) acc => ``(
      Batteries.AssocList.cons
        $(Lean.Syntax.mkStrLit i.getId.toString)
        (capnptext| $c )
        $acc
    ))
    (← ``(Batteries.AssocList.nil))
  ``( CapnpText.struct $(map))
| `( (capnptext| [ $c:capnptext,* ] ) ) => do
  ``( CapnpText.arr #[ $[ (capnptext| $c ) ],* ] )
| `( (capnptext| $s:str ) ) => do
  ``( CapnpText.str $s )
| `( (capnptext| $s:num ) ) => do
  ``( CapnpText.num $s )
| `( (capnptext| $s:ident ) ) => do
  ``( CapnpText.id $(Lean.Syntax.mkStrLit s.getId.toString) )
| `( (capnptext| <opaque pointer> ) ) => do
  ``( CapnpText.opaquePtr )

namespace CapnpText

def Struct := Batteries.AssocList String CapnpText
deriving Inhabited, Repr

def struct? (name : String) (c : CapnpText) : Except String Struct :=
  match c with
  | .struct map => pure map
  | _ =>
    throw s!"struct {name} expected:\n{repr c}"

namespace Struct

def field (fieldName : String) (map : Struct) : CapnpText :=
  match map.find? fieldName with
  | some c' => c'
  | none => .default

def field? (fieldName : String) (map : Struct) : Except String CapnpText := do
  match map.find? fieldName with
  | some c' => pure c'
  | none => throw s!"field {fieldName} missing:\n{repr (struct map)}"

end Struct

def arr? (c : CapnpText) : Except String (Array CapnpText) := do
  match c with
  | .arr a =>
    pure a
  | .default => pure #[]
  | _ =>
    throw s!"array expected:\n{repr c}"

def str? (c : CapnpText) : Except String String := do
  match c with
  | .str s =>
    pure s
  | .default => pure ""
  | _ =>
    throw s!"string expected:\n{repr c}"

def num? (c : CapnpText) : Except String Nat := do
  match c with
  | .num a =>
    pure a
  | .default => pure 0
  | _ =>
    throw s!"number expected:\n{repr c}"

def id? (c : CapnpText) : Except String String := do
  match c with
  | .id s =>
    pure s
  | _ =>
    throw s!"id expected:\n{repr c}"

end CapnpText

def CodeGeneratorRequest.fromText (t : CapnpText) : Except String CodeGeneratorRequest := do
  let t ← t.struct? "codegeneratorrequest"
  let capnpVersion ← (do
    let t ← t.field "capnpVersion" |>.struct? "capnpversion"
    let major := (← t.field "major" |>.num?).toUInt16
    let minor := (← t.field "minor" |>.num?).toUInt8
    let micro := (← t.field "micro" |>.num?).toUInt8
    return some { major, minor, micro })
  let nodes ← (do
    let ts ← t.field "nodes" |>.arr?
    ts.mapM (fun t => do
      let t ← t.struct? "node"
      let id := (← (t.field "id").num?).toUInt64
      let displayName ← (t.field "displayName").str?
      let displayNamePrefixLength := (← (t.field "displayNamePrefixLength").num?).toUInt32
      let scopeId := (← (t.field "scopeId").num?).toUInt64
      let parameters ← (do
        let ts ← t.field "parameters" |>.arr?
        ts.mapM fun t => do
          let t ← t.struct? "parameter"
          let name ← (t.field "name").str?
          return .mk name)
      let isGeneric ←
        match (← (t.field "isGeneric").id?) with
        | "true" => pure true
        | "false" => pure false
        | s => throw s!"expected bool, got {s}"
      let nestedNodes ← (do
        let t ← (t.field "nestedNodes").arr?
        return #[])
      let annotations ← (do
        let t ← (t.field "annotations").arr?
        return #[])
      let body ← (do
        try
          let t ← t.field? "file"
          return .file
        catch _ =>
        try
          let t ← t.field? "struct"
          let t ← t.struct? "node struct group"
          let dataWordCount := (← t.field "dataWordCount" |>.num?).toUInt16
          let pointerCount := (← t.field "pointerCount" |>.num?).toUInt16
          let preferredListEncoding := default
          let isGroup := default
          let discriminantCount := (← t.field "discriminantCount" |>.num?).toUInt16
          let discriminantOffset := (← t.field "discriminantOffset" |>.num?).toUInt32
          let fields := default
          return Node.Body.struct dataWordCount pointerCount preferredListEncoding
            isGroup discriminantCount discriminantOffset fields
        catch _ =>
        try
          let t ← t.field? "enum"
          let enumerants := default
          return .enum enumerants
        catch _ =>
        try
          let t ← t.field? "interface"
          let methods := default
          let superclasses := default
          return .interface methods superclasses
        catch _ =>
        try
          let t ← t.field? "const"
          let t ← t.struct? "node const group"
          let type := «Type».mk «Type».Body.text
          let value := Value.mk («Value».Body.text "")
          return .const type value
        catch _ =>
        try
          let t ← t.field? "annotation"
          let type := «Type».mk «Type».Body.text
          let targetsFile := false
          let targetsConst := false
          let targetsEnum := false
          let targetsEnumerant := false
          let targetsStruct := false
          let targetsField := false
          let targetsUnion := false
          let targetsGroup := false
          let targetsInterface := false
          let targetsMethod := false
          let targetsParam := false
          let targetsAnnotation := false
          return .annotation type
            targetsFile targetsConst targetsEnum targetsEnumerant targetsStruct
            targetsField targetsUnion targetsGroup targetsInterface targetsMethod
            targetsParam targetsAnnotation
        catch _ =>
          throw s!"struct union had no field selected:\n{repr t}")
      return .mk
        id displayName displayNamePrefixLength scopeId parameters
        isGeneric nestedNodes annotations body
      ))
  let sourceInfo ← (do
    let ts ← t.field "sourceInfo" |>.arr?
    ts.mapM (fun t => do
      let t ← t.struct? "sourceinfo"
      let id := (← (t.field "id").num?).toUInt64
      let docComment := (← (t.field "docComment").str?)
      let members ← (do
        let ts ← t.field "members" |>.arr?
        ts.mapM (fun t => do
          let t ← t.struct? "member"
          let docComment := (← t.field "docComment" |>.str?)
          return .mk
            docComment))
      return .mk
        id docComment members))
  let requestedFiles ← (do
    let ts ← t.field "requestedFiles" |>.arr?
    ts.mapM (fun t => do
      let t ← t.struct? "requestedfile"
      let id := (← (t.field "id").num?).toUInt64
      let filename := (← (t.field "filename").str?)
      let imports ← (do
        let ts ← (t.field "imports").arr?
        ts.mapM (fun t => do
          let t ← t.struct? "import"
          let id := (← (t.field "id").num?).toUInt64
          let name := (← (t.field "name").str?)
          return .mk
            id name))
      return .mk
        id filename imports))
  return {
    capnpVersion
    nodes
    sourceInfo
    requestedFiles
  }
