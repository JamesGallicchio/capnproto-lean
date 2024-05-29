import Batteries
import Qq

import CapnProtoLean.Schema

namespace CapnProtoLean

open Lean Elab Command Meta Qq

scoped elab "generate_capnproto" e:term : command => do
  let files ← liftTermElabM <| do
    let expr ← Term.elabTerm e (some q(Array System.FilePath))
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
  let msg : Message ← liftM (m := IO) <| Message.fromHandle out

  let _ ← dbgTrace s!"successfully parsed message: {msg.segments.map (·.data.size)}" fun () => pure ()
  let remaining ← out.readBinToEnd
  let _ ← dbgTrace s!"{remaining.size} bytes left in stream" fun () => pure ()

  if (← capnpc.wait) ≠ 0 then
    let err ← capnpc.stderr.readToEnd
    throwError "capnpc returned error:\n{err}"

  --let req : CodeGeneratorRequest ← liftM (m := IO) <|
  --  IO.ofExcept <| msg.decode CodeGeneratorRequest.decoder

  return
