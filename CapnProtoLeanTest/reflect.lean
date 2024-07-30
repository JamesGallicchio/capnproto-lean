import CapnProtoLean
import Lean

namespace CapnProtoLean

open System Lean Elab Command in
#eval show CommandElabM _ from do
  let files : Array FilePath := #["CapnProtoLeanTest/reflect/schema.capnp"]

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

  msg.

#exit

#generate_capnproto #[
  "CapnProtoLeanTest/reflect/schema.capnp"
]
