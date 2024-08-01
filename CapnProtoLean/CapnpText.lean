import Batteries

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
