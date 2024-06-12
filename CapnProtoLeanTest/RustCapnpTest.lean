import CapnProtoLean

open CapnProtoLean

#generate_capnproto
  { capnpVersion := default
    sourceInfo := #[]
    requestedFiles := #[]
    nodes :=
      #[.mk (id := 17973010740377189878) (displayName := "schema.capnp") (displayNamePrefixLength := 7) (scopeId := 0)
          (nestedNodes :=
          #[.mk "Test" 17321779288571250033, .mk "Request" 11025885811572425229, .mk "Expr" 9503309125958469825])
          (body := .file) (isGeneric := false) (parameters := default) (annotations := default),
        .mk (id := 17321779288571250033) (displayName := "schema.capnp:Test") (displayNamePrefixLength := 13)
          (scopeId := 17973010740377189878) (nestedNodes := #[]) (body :=
          .struct (dataWordCount := 1) (pointerCount := 2) (preferredListEncoding := .inlineComposite) (isGroup :=
            false) (discriminantCount := 0) (discriminantOffset := 0) (fields :=
            #[.mk (name := "foo") (codeOrder := 0) (discriminantValue := 65535) (body :=
                .slot (offset := 0) (type := .mk .uint64) (defaultValue := .mk <| .uint64 0) (hadExplicitDefault :=
                  false))
                (ordinal := .explicit 0) (annotations := default),
              .mk (name := "bar") (codeOrder := 1) (discriminantValue := 65535) (body :=
                .slot (offset := 0) (type := .mk (.list (elementType := .mk .uint64))) (defaultValue :=
                  .mk <| .list default) (hadExplicitDefault := false))
                (ordinal := .explicit 1) (annotations := default),
              .mk (name := "zing") (codeOrder := 2) (discriminantValue := 65535) (body :=
                .slot (offset := 1) (type :=
                  .mk
                    (.list (elementType := .mk (body := .struct (typeId := 17321779288571250033) (brand := .mk #[])))))
                  (defaultValue := .mk (.list default)) (hadExplicitDefault := false))
                (ordinal := (.explicit 2)) (annotations := default)]))
          (isGeneric := false) (parameters := default) (annotations := default)] } {{
  def «schema.capnp:Test» : Type :=
    CapnProtoLean.Struct
  instance : CapnProtoLean.Struct.IsStruct «schema.capnp:Test»
      where
    fromStruct := id
    expectedDataWords := 1
    expectedPtrWords := 2
  def «schema.capnp:Test».foo (self : «schema.capnp:Test») : CapnProtoLean.DecodeM UInt64 :=
    CapnProtoLean.Struct.HasStructAccessor.get self 0
  def «schema.capnp:Test».bar (self : «schema.capnp:Test») : CapnProtoLean.DecodeM (CapnProtoLean.List.P UInt64) :=
    CapnProtoLean.Struct.HasStructAccessor.get self 0
  def «schema.capnp:Test».zing (self : «schema.capnp:Test») :
      CapnProtoLean.DecodeM (CapnProtoLean.List.P «schema.capnp:Test») :=
    CapnProtoLean.Struct.HasStructAccessor.get self 1
}}
