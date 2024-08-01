import Batteries
import Qq

namespace CapnProtoLean.Utils

open Lean Elab Command

scoped elab "declare_nonempty_type" id:ident : command => do
  let ptd : Ident := mkIdent <| id.getId.str "Pointed"
  elabCommand <| ← `(
    opaque $ptd : NonemptyType.{0}
    def $id : Type := ($ptd).type
    instance : Nonempty $id := ($ptd).property
  )

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

end Example

scoped syntax (name := run_elab_cached)
  "#run_elab_cached " term:max " {{" (ppLine command)* ppDedent(ppLine "}}") : command

deriving instance TypeName for String

open Qq Meta

@[command_elab run_elab_cached]
def runElabCachedHandler : CommandElab := fun stx => do
  match stx with
  | `(command| #run_elab_cached $e:term {{ $c:command* }} ) =>
    let cmds ← termToCmds e
    if cmds != c then
      logWarning "The macro output differs from the syntax between {{ }}"
      let fmt ← format e cmds
      pushInfoLeaf (.ofCustomInfo {
        stx := ← getRef,
        value := Dynamic.mk fmt.pretty })
    for c in c do elabCommand c
  | _ =>
    throwUnsupportedSyntax
where
  termToCmds (e : Term) : CommandElabM (TSyntaxArray `command) := do
    let elabM : CommandElabM (TSyntaxArray `command) ← liftTermElabM <| do
        let expr ← Term.elabTerm e (some q(CommandElabM (TSyntaxArray `command)))
        unsafe evalExpr (CommandElabM (TSyntaxArray `command))
                      (q(CommandElabM (TSyntaxArray `command)))
                      expr
    elabM
  format (e : Term) (cmds : TSyntaxArray `command) : CommandElabM Format := do
    let syn ← `(command|
      #run_elab_cached $e {{
        $cmds:command*
      }}
    )
    let parenthesized ← liftCoreM <| Lean.PrettyPrinter.parenthesizeCommand syn
    return ← liftCoreM <| Lean.PrettyPrinter.formatCommand parenthesized

open Server RequestM CodeAction in
@[command_code_action run_elab_cached]
def runElabCachedCodeAction : CommandCodeAction := fun _ _ _ node => do
  let .node _ ts := node | return #[]
  let res := ts.findSome? fun
    | .node (.ofCustomInfo { stx, value }) _ => return (stx, (← value.get? String))
    | _ => none
  let some (stx, newText) := res | return #[]
  let eager := {
    title := "Update #run_elab_cached with correct output"
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

namespace Example

def hiCmd : CommandElabM (TSyntaxArray `command) := do
  return #[
    ← `(command|
      def $(mkIdent `hi) : $(mkIdent `String) := "hello world"
    )
  ]

#run_elab_cached hiCmd {{
  def hi : String :=
    "hello world"
}}

end Example
