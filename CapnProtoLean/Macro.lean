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
  let req : CodeGeneratorRequest ← liftM (m := IO) <|
    CodeGeneratorRequest.fromBytes (do
      let bs ← out.read (1 <<< 12).toUSize
      if bs.isEmpty then return none else return some bs)

  if (← capnpc.wait) ≠ 0 then
    let err ← IO.FS.Handle.readToEnd capnpc.stderr
    throwError "capnpc returned error:\n{err}"

  dbgTrace s!"got request with at capnp version {req.view.capnpVersion}" fun () => do
  return
