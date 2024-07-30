import Lean

open Lean Elab Command

syntax (name := generate_capnproto)
  "#generate_capnproto " term (" {{ " command* " }}" )? : command

@[command_elab generate_capnproto]
def genCapnProtoHandler : CommandElab := fun stx => do
  logInfo (
    ← liftCoreM <| Lean.PrettyPrinter.ppCommand (.mk stx)
  )

#generate_capnproto "hi" {{
  def what := 1
  def who := 2
}}
