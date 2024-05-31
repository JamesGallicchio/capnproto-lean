import CapnProtoLean

open CapnProtoLean Decoder

mutual
inductive Test
| mk
    (foo : UInt64)
    (bar : Array UInt64)
    (zing : Array Test)
end

deriving instance Repr for Test

mutual
partial def Test.decoder : StructDecoder Test :=
  fun dataWords ptrWords =>
  context "test" do
  if dataWords ≠ 1 then
    error s!"dataWords: {dataWords}"
  if ptrWords ≠ 2 then
    error s!"ptrWords: {ptrWords}"
  let foo ← context "data[0]" <| moveOffBytes 0 <| uint64
  let bar ← context "ptr[0]" <| moveOffWords (1 + 0) <| listPtrPrim 5 uint64
  let zing ← context "ptr[1]" <| moveOffWords (1 + 1) <| listPtrStruct Test.decoder
  return .mk (foo := foo) (bar := bar) (zing := zing)
end

#eval show IO Unit from do
  let child ← IO.Process.spawn {
    cmd := "./target/debug/rust-capnp-test"
    cwd := some "./CapnProtoLeanTest/rust_capnp_test"
    stdout := .piped
  }
  let stdout := child.stdout
  let msg ← Message.fromHandle stdout
  let obj : Test ←
    match decode Test.decoder msg with
    | .ok (some req) => pure req
    | .ok none => throw (.userError "root was none?")
    | .error e =>
      throw (.userError s!"decode error: {e}")
  IO.println (repr obj)
  return ()
