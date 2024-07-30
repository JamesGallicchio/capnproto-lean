import CapnProtoLean

open CapnProtoLean

def Test : Type := CapnProtoLean.Struct
instance : CapnProtoLean.Struct.IsStruct Test
    where
  fromStruct := id
  expectedDataWords := 2
  expectedPtrWords := 2

def Request : Type := CapnProtoLean.Struct
instance : CapnProtoLean.Struct.IsStruct Request
    where
  fromStruct := id
  expectedDataWords := 1
  expectedPtrWords := 2

def Request._value : Type := CapnProtoLean.Struct
instance : CapnProtoLean.Struct.IsStruct Request._value
    where
  fromStruct := id
  expectedDataWords := 1
  expectedPtrWords := 2

def Expr : Type := CapnProtoLean.Struct
instance : CapnProtoLean.Struct.IsStruct Expr where
  fromStruct := id
  expectedDataWords := 2
  expectedPtrWords := 2

def Expr.var : Type := CapnProtoLean.Struct
instance : CapnProtoLean.Struct.IsStruct Expr.var where
  fromStruct := id
  expectedDataWords := 2
  expectedPtrWords := 2
def Expr.const : Type := CapnProtoLean.Struct
instance : CapnProtoLean.Struct.IsStruct Expr.const where
  fromStruct := id
  expectedDataWords := 2
  expectedPtrWords := 2

def Expr.binary : Type := CapnProtoLean.Struct
instance : CapnProtoLean.Struct.IsStruct Expr.binary where
  fromStruct := id
  expectedDataWords := 2
  expectedPtrWords := 2


def Request.name (self : Request) : CapnProtoLean.DecodeM Text :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def Request.value (self : Request) : CapnProtoLean.DecodeM Request._value :=
  return self

inductive Request._value._cases
| test (t : Test)
| expr (e : Expr)

def Request._value.cases (self : Request._value) : CapnProtoLean.DecodeM _cases := do
  match ← self.uint16 0 with
  | 0 => return .test (← self.struct 1)
  | 1 => return .expr (← self.struct 1)
  | _ => throw .enumOOB

def Test.foo (self : Test) : CapnProtoLean.DecodeM UInt64 :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0

def Test.bar (self : Test) : CapnProtoLean.DecodeM (List.P UInt64) :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0

def Test.zing (self : Test) : CapnProtoLean.DecodeM (List.P Test) :=
  CapnProtoLean.Struct.HasStructAccessor.get self 1

def Test.bonk (self : Test) : CapnProtoLean.DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 64

def Test.boonk (self : Test) : CapnProtoLean.DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 65


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
  IO.println msg.segments[0]!.data
  let prog : DecodeM String := do
    let s : Request ← msg.root
    let name ← (← s.name).getString
    let val ←
      match ← (← s.value).cases with
      | .expr e => do
        
        sorry
      | .test t => do
        let foo ← t.foo
        let bar ← t.bar
        let bar' := s!"[{← bar.get 0} {← bar.get 1}]"
        let zing ← t.zing
        let bonk ← t.bonk
        let boonk ← t.boonk
        pure s!"{t.start} {foo} {bar.size} {bar'} {zing.size} {bonk} {boonk}"
    return s!"{toString s.start}\n{name}\n{val}"
  let res ←
    match prog msg with
    | .ok a => pure a
    | .error e => throw <| .userError (repr e).pretty
  IO.println res
