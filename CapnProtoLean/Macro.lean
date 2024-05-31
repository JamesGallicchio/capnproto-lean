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

  let _ ← dbgTrace s!"{repr req}" fun () => pure ()
