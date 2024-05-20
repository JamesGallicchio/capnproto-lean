import Batteries

namespace CapnProtoLean.Meta

open Lean Elab Command

scoped elab "declare_nonempty_type" id:ident : command => do
  let ptd : Ident := mkIdent <| id.getId.str "Pointed"
  elabCommand <| ← `(
    opaque $ptd : NonemptyType.{0}
    def $id : Type := ($ptd).type
    instance : Nonempty $id := ($ptd).property
  )

inductive hi
| what

scoped elab "declare_view" id:ident as:ident : command => do
  elabCommand <| ← `(
    def $(mkIdent <| id.getId.str "view") (x : $id) : $as := unsafe unsafeCast x
    def $(mkIdent <| id.getId.str "ofView") (x : $as) : $id := unsafe unsafeCast x
  )


namespace Example

declare_nonempty_type Hi

structure Hi.View where
  blah : Int
deriving Inhabited, Repr

declare_view Hi Hi.View

/-- info: { blah := 0 } -/
#guard_msgs in
#eval Hi.ofView ⟨0⟩ |>.view

end Example
