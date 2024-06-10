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

/-
#eval show IO Unit from do
  let out ← IO.Process.output {
    cmd := "cargo"
    args := #["build"]
    cwd := some "./CapnProtoLeanTest/rust_capnp_test"
  }
  if out.exitCode != 0 then
    throw (.userError out.stderr)
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
      throw (.userError s!"decode erArray System.FilePathror: {e}")
  IO.println (repr obj)
  return ()
-/

#generate_capnproto
  { capnpVersion := default
    sourceInfo := #[]
    requestedFiles := #[]
    nodes :=
      #[.mk (id := 17973010740377189878) (displayName := "schema.capnp") (displayNamePrefixLength := 7) (scopeId := 0)
          (nestedNodes :=
          [.mk "Test" 17321779288571250033, .mk "Request" 11025885811572425229, .mk "Expr" 9503309125958469825])
          (body := .file) (isGeneric := false) (parameters := default) (annotations := default),
        .mk (id := 17321779288571250033) (displayName := "schema.capnp:Test") (displayNamePrefixLength := 13)
          (scopeId := 17973010740377189878) (nestedNodes := []) (body :=
          .struct (dataWordCount := 1) (pointerCount := 2) (preferredListEncoding := .inlineComposite) (isGroup :=
            false) (discriminantCount := 0) (discriminantOffset := 0) (fields :=
            [.mk (name := "foo") (codeOrder := 0) (discriminantValue := 65535) (body :=
                .slot (offset := 0) (type := .mk .uint64) (defaultValue := .mk <| .uint64 0) (hadExplicitDefault :=
                  false))
                (ordinal := .explicit 0) (annotations := default),
              .mk (name := "bar") (codeOrder := 1) (discriminantValue := 65535) (body :=
                .slot (offset := 0) (type := .mk (.list (elementType := .mk .uint64))) (defaultValue :=
                  .mk <| .list default) (hadExplicitDefault := false))
                (ordinal := .explicit 1) (annotations := default),
              .mk (name := "zing") (codeOrder := 2) (discriminantValue := 65535) (body :=
                .slot (offset := 1) (type :=
                  .mk (.list (elementType := .mk (body := .struct (typeId := 17321779288571250033) (brand := .mk [])))))
                  (defaultValue := .mk (.list default)) (hadExplicitDefault := false))
                (ordinal := (.explicit 2)) (annotations := default)]))
          (isGeneric := false) (parameters := default) (annotations := default)] } {{
  mutual
  end mutual
  end
  }}
