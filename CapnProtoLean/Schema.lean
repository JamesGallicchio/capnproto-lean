import CapnProtoLean.Encoding
import CapnProtoLean.Meta

namespace CapnProtoLean

abbrev Id := UInt64

def Node.Parameter := Struct
instance : Struct.IsStruct Node.Parameter where
  fromStruct := id
  expectedDataWords := 0
  expectedPtrWords := 1

def Node.NestedNode := Struct
instance : Struct.IsStruct Node.NestedNode where
  fromStruct := id
  expectedDataWords := 1
  expectedPtrWords := 1

def Node.SourceInfo := Struct
instance : Struct.IsStruct Node.SourceInfo where
  fromStruct := id
  expectedDataWords := 0
  expectedPtrWords := 0

def Node := Struct
instance : Struct.IsStruct Node where
  fromStruct := id
  expectedDataWords := 5
  expectedPtrWords := 6

def Node.struct := Struct
instance : Struct.IsStruct Node.struct where
  fromStruct := id
  expectedDataWords := 5
  expectedPtrWords := 6

def Node.enum := Struct
instance : Struct.IsStruct Node.enum where
  fromStruct := id
  expectedDataWords := 5
  expectedPtrWords := 6

def Node.interface := Struct
instance : Struct.IsStruct Node.interface where
  fromStruct := id
  expectedDataWords := 5
  expectedPtrWords := 6

def Node.const := Struct
instance : Struct.IsStruct Node.const where
  fromStruct := id
  expectedDataWords := 5
  expectedPtrWords := 6

def Node.annotation := Struct
instance : Struct.IsStruct Node.annotation where
  fromStruct := id
  expectedDataWords := 5
  expectedPtrWords := 6

def Annotation := Struct
instance : Struct.IsStruct Annotation where
  fromStruct := id
  expectedDataWords := 1
  expectedPtrWords := 2

def Field := Struct
instance : Struct.IsStruct Field where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 4

def Field.slot := Struct
instance : Struct.IsStruct Field.slot where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 4

def Field.group := Struct
instance : Struct.IsStruct Field.group where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 4

def Field.ordinal := Struct
instance : Struct.IsStruct Field.ordinal where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 4

def ElementSize := UInt16
instance : Struct.HasStructAccessor ElementSize :=
  inferInstanceAs (Struct.HasStructAccessor UInt16)

def Enumerant := Struct
instance : Struct.IsStruct Enumerant where
  fromStruct := id
  expectedDataWords := 1
  expectedPtrWords := 2

def Superclass := Struct
instance : Struct.IsStruct Superclass where
  fromStruct := id
  expectedDataWords := 1
  expectedPtrWords := 1

def Method := Struct
instance : Struct.IsStruct Method where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 5

def Type.anyPointer.unconstrained := Struct
instance : Struct.IsStruct «Type».anyPointer.unconstrained where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 1

def Type.anyPointer.parameter := Struct
instance : Struct.IsStruct «Type».anyPointer.parameter where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 1

def Type.anyPointer.implicitMethodParameter := Struct
instance : Struct.IsStruct «Type».anyPointer.implicitMethodParameter where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 1

def Type.anyPointer := Struct
instance : Struct.IsStruct «Type».anyPointer where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 1

def «Type».list := Struct
instance : Struct.IsStruct «Type».list where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 1

def «Type».enum := Struct
instance : Struct.IsStruct «Type».enum where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 1

def «Type».struct := Struct
instance : Struct.IsStruct «Type».struct where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 1

def «Type».interface := Struct
instance : Struct.IsStruct «Type».interface where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 1

def «Type» := Struct
instance : Struct.IsStruct «Type» where
  fromStruct := id
  expectedDataWords := 3
  expectedPtrWords := 1

def Brand := Struct
instance : Struct.IsStruct Brand where
  fromStruct := id
  expectedDataWords := 0
  expectedPtrWords := 1

def Brand.Scope := Struct
instance : Struct.IsStruct Brand.Scope where
  fromStruct := id
  expectedDataWords := 2
  expectedPtrWords := 1

def Brand.Binding := Struct
instance : Struct.IsStruct Brand.Binding where
  fromStruct := id
  expectedDataWords := 1
  expectedPtrWords := 1

def Value := Struct
instance : Struct.IsStruct Value where
  fromStruct := id
  expectedDataWords := 2
  expectedPtrWords := 1

def CapnpVersion := Struct
instance : Struct.IsStruct CapnpVersion where
  fromStruct := id
  expectedDataWords := 1
  expectedPtrWords := 0

namespace Node.Parameter
variable (self : Node.Parameter)
def name : DecodeM Text :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
end Node.Parameter

namespace Node.NestedNode
variable (self : Node.NestedNode)
def name : DecodeM Text :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def id : DecodeM Id :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
end Node.NestedNode

namespace Node
variable (self : Node)

def id : DecodeM Id :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def displayName : DecodeM Text :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def displayNamePrefixLength : DecodeM UInt32 :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def scopeId : DecodeM Id :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def parameters : DecodeM (List.P Node.Parameter) :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def isGeneric : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def nestedNodes : DecodeM (List.P Node.NestedNode) :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def annotations : DecodeM (List.P Annotation) :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0

namespace struct
variable (self : struct)
def dataWordCount : DecodeM <| UInt16 :=
  CapnProtoLean.Struct.HasStructAccessor.get self 7
def pointerCount : DecodeM <| UInt16 :=
  CapnProtoLean.Struct.HasStructAccessor.get self 12
def preferredListEncoding : DecodeM <| ElementSize :=
  CapnProtoLean.Struct.HasStructAccessor.get self 13
def isGroup : DecodeM <| Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 224
def discriminantCount : DecodeM <| UInt16 :=
  CapnProtoLean.Struct.HasStructAccessor.get self 15
def discriminantOffset : DecodeM <| UInt32 :=
  CapnProtoLean.Struct.HasStructAccessor.get self 8
def fields : DecodeM <| List.P Field :=
  CapnProtoLean.Struct.HasStructAccessor.get self 3
end struct

namespace enum
variable (self : enum)
def enumerants : DecodeM <| List.P Enumerant :=
  CapnProtoLean.Struct.HasStructAccessor.get self 3
end enum

namespace interface
variable (self : interface)
def methods : DecodeM <| List.P Method :=
  CapnProtoLean.Struct.HasStructAccessor.get self 3
def superclasses : DecodeM <| List.P Superclass :=
  CapnProtoLean.Struct.HasStructAccessor.get self 4
end interface

namespace const
variable (self : const)
def type : DecodeM «Type» :=
  CapnProtoLean.Struct.HasStructAccessor.get self 3
def value : DecodeM Value :=
  CapnProtoLean.Struct.HasStructAccessor.get self 4
end const

namespace annotation
variable (self : annotation)
def type : DecodeM «Type» :=
  CapnProtoLean.Struct.HasStructAccessor.get self 3
def targetsFile       : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 112
def targetsConst      : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 113
def targetsEnum       : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 114
def targetsEnumerant  : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 115
def targetsStruct     : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 116
def targetsField      : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 117
def targetsUnion      : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 118
def targetsGroup      : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 119
def targetsInterface  : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 120
def targetsMethod     : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 121
def targetsParam      : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 122
def targetsAnnotation : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 123
end annotation

inductive Cases
| file (v : Unit)
| struct (v : struct)
| enum (v : enum)
| interface (v : interface)
| const (v : const)
| annotation (v : annotation)

def cases : DecodeM Cases := do
  let discrim ← Struct.uint16 self 6
  match discrim with
  | 0 => return Cases.file ()
  | 1 => return Cases.struct self
  | 2 => return Cases.enum self
  | 3 => return Cases.interface self
  | 4 => return Cases.const self
  | 5 => return Cases.annotation self
  | _ => throw .enumOOB

end Node

namespace Field
variable (self : Field)

inductive Cases
| slot (v : slot)
| group (v : group)

def cases : DecodeM Cases := do
  let discrim ← Struct.uint16 self 4
  match discrim with
  | 0 => return Cases.slot self
  | 1 => return Cases.group self
  | _ => throw .enumOOB

namespace slot
variable (self : slot)
def offset : DecodeM UInt32 :=
  CapnProtoLean.Struct.HasStructAccessor.get self 1
def type : DecodeM «Type» :=
  CapnProtoLean.Struct.HasStructAccessor.get self 2
def defaultValue : DecodeM Value :=
  CapnProtoLean.Struct.HasStructAccessor.get self 3
def hadExplicitDefault : DecodeM Bool :=
  CapnProtoLean.Struct.HasStructAccessor.get self 128
end slot

namespace group
variable (self : group)
def typeId : DecodeM Id :=
  CapnProtoLean.Struct.HasStructAccessor.get self 2
end group

namespace ordinal
variable (self : ordinal)

inductive Cases
| implicit (v : Unit)
| explicit (v : UInt16)

def cases : DecodeM Cases := do
  let discrim ← Struct.uint16 self 5
  match discrim with
  | 0 => return Cases.implicit ()
  | 1 =>
    return Cases.explicit
      (← CapnProtoLean.Struct.HasStructAccessor.get self 6)
  | _ => throw .enumOOB

end ordinal

def name : DecodeM Text :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def codeOrder : DecodeM UInt16 :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def annotations : DecodeM (List.P Annotation) :=
  CapnProtoLean.Struct.HasStructAccessor.get self 1
def discriminantValue : DecodeM UInt16 :=
  CapnProtoLean.Struct.HasStructAccessor.get self 1
def getOrdinal : DecodeM Field.ordinal :=
  return self

end Field

namespace ElementSize
inductive Cases
| empty
| bit
| byte
| twoBytes
| fourBytes
| eightBytes
| pointer
| inlineComposite

def cases (self : ElementSize) : DecodeM Cases :=
  match show UInt16 from self with
  | 0 => return .empty
  | 1 => return .bit
  | 2 => return .byte
  | 3 => return .twoBytes
  | 4 => return .fourBytes
  | 5 => return .eightBytes
  | 6 => return .pointer
  | 7 => return .inlineComposite
  | _ => throw .enumOOB

end ElementSize

namespace Enumerant
variable (self : Enumerant)
def name : DecodeM <| Text :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def codeOrder : DecodeM <| UInt16 :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def annotations : DecodeM <| List.P Annotation :=
  CapnProtoLean.Struct.HasStructAccessor.get self 1
end Enumerant

namespace Superclass
variable (self : Superclass)
def id : DecodeM Id :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def brand : DecodeM Brand :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
end Superclass

namespace Method
variable (self : Method)
def name : DecodeM <| Text :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def codeOrder : DecodeM <| UInt16 :=
  CapnProtoLean.Struct.HasStructAccessor.get self 0
def implicitParameters : DecodeM <| List.P Node.Parameter :=
  CapnProtoLean.Struct.HasStructAccessor.get self 4
def paramStructType : DecodeM <| Id :=
  CapnProtoLean.Struct.HasStructAccessor.get self 1
def paramBrand : DecodeM <| Brand :=
  CapnProtoLean.Struct.HasStructAccessor.get self 2
def resultStructType : DecodeM <| Id :=
  CapnProtoLean.Struct.HasStructAccessor.get self 2
def resultBrand : DecodeM <| Brand :=
  CapnProtoLean.Struct.HasStructAccessor.get self 3
def annotations : DecodeM <| List.P Annotation :=
  CapnProtoLean.Struct.HasStructAccessor.get self 1
end Method

namespace «Type».anyPointer
variable (self : «Type».anyPointer)
namespace unconstrained
variable (self : unconstrained)
inductive Cases
| anyKind (v : Unit)
| struct (v : Unit)
| list (v : Unit)
| capability (v : Unit)

def cases : DecodeM Cases := do
  let discrim ← Struct.uint16 self 5
  match discrim with
  | 0 => return Cases.anyKind ()
  | 1 => return Cases.struct ()
  | 2 => return Cases.list ()
  | 3 => return Cases.capability ()
  | _ => throw .enumOOB
end unconstrained

namespace parameter
variable (self : Method)
def scopeId : DecodeM Id :=
  Struct.HasStructAccessor.get self 2
def parameterIndex : DecodeM UInt16 :=
  Struct.HasStructAccessor.get self 5
end parameter

namespace implicitMethodParameter
variable (self : implicitMethodParameter)
def parameterIndex : DecodeM UInt16 :=
  Struct.HasStructAccessor.get self 5
end implicitMethodParameter

inductive Cases
| unconstrained (v : «Type».anyPointer.unconstrained)
| parameter (v : «Type».anyPointer.unconstrained)
| implicitMethodParameter (v : «Type».anyPointer.unconstrained)

def cases : DecodeM Cases := do
  let discrim ← Struct.uint64 self 4
  match discrim with
  | 0 => return .unconstrained self
  | 1 => return .parameter self
  | 2 => return .implicitMethodParameter self
  | _ => throw .enumOOB

end anyPointer

namespace list
variable (self : list)
def elementType : DecodeM «Type» :=
  Struct.HasStructAccessor.get self 0
end list

namespace enum
variable (self : enum)
def typeId : DecodeM Id :=
  Struct.HasStructAccessor.get self 1
def brand : DecodeM Brand :=
  Struct.HasStructAccessor.get self 0
end enum

namespace struct
variable (self : struct)
def typeId : DecodeM Id :=
  Struct.HasStructAccessor.get self 1
def brand : DecodeM Brand :=
  Struct.HasStructAccessor.get self 0
end struct

namespace interface
variable (self : interface)
def typeId : DecodeM Id :=
  Struct.HasStructAccessor.get self 1
def brand : DecodeM Brand :=
  Struct.HasStructAccessor.get self 0
end interface

inductive Cases
| void
| bool
| int8
| int16
| int32
| int64
| uint8
| uint16
| uint32
| uint64
| float32
| float64
| text
| data
| list (v : list)
| enum (v : enum)
| struct (v : struct)
| interface (v : interface)
| anyPointer (v : «Type».anyPointer)

variable (self : «Type»)
def cases : DecodeM Cases := do
  let discrim ← Struct.uint16 self 0
  match discrim with
  | 0 => return .void
  | 1 => return .bool
  | 2 => return .int8
  | 3 => return .int16
  | 4 => return .int32
  | 5 => return .int64
  | 6 => return .uint8
  | 7 => return .uint16
  | 8 => return .uint32
  | 9 => return .uint64
  | 10 => return .float32
  | 11 => return .float64
  | 12 => return .text
  | 13 => return .data
  | 14 => return .list self
  | 15 => return .enum self
  | 16 => return .struct self
  | 17 => return .interface self
  | 18 => return .anyPointer self
  | _ => throw .enumOOB

end «Type»

namespace Brand
variable (self : Brand)
def scopes : DecodeM (List.P Brand.Scope) :=
  Struct.HasStructAccessor.get self 0
end Brand

namespace Brand.Scope
variable (self : Brand.Scope)

inductive Cases
| bind (v : List.P Brand.Binding)
| inherit (v : Unit)

def cases : DecodeM Cases := do
  let discrim ← Struct.uint16 self 4
  match discrim with
  | 0 => return .bind (← Struct.HasStructAccessor.get self 0)
  | 1 => return .inherit ()
  | _ => throw .enumOOB

def scopeId : DecodeM Id :=
  Struct.HasStructAccessor.get self 0

end Brand.Scope

namespace Brand.Binding
variable (self : Brand.Binding)

inductive Cases
| unbound (v : Unit)
| type (v : «Type»)

def cases : DecodeM Cases := do
  let discrim ← Struct.uint16 self 0
  match discrim with
  | 0 => return .unbound ()
  | 1 => return .type (← Struct.HasStructAccessor.get self 0)
  | _ => throw .enumOOB

end Brand.Binding
namespace Value
variable (self : Value)
inductive Cases
| void    (_ : Unit)
| bool    (_ : Bool)
| int8    (_ : Int8)
| int16   (_ : Int16)
| int32   (_ : Int32)
| int64   (_ : Int64)
| uint8   (_ : UInt8)
| uint16  (_ : UInt16)
| uint32  (_ : UInt32)
| uint64  (_ : UInt64)
| float32 (_ : Float32)
| float64 (_ : Float64)
| text    (_ : Text)
| data    (_ : Data)
| list    (_ : AnyPointer)
| enum    (_ : UInt16)
| struct  (_ : AnyPointer)
| interface (v : Unit)
| anyPointer (_ : AnyPointer)

def cases : DecodeM Cases := do
  let discrim ← Struct.uint16 self 0
  match discrim with
  | 0 => return .void ()
  | 1 => return .bool (← Struct.HasStructAccessor.get self 16)
  | 2 => return .int8 (← Struct.HasStructAccessor.get self 2)
  | 3 => return .int16 (← Struct.HasStructAccessor.get self 1)
  | 4 => return .int32 (← Struct.HasStructAccessor.get self 1)
  | 5 => return .int64 (← Struct.HasStructAccessor.get self 1)
  | 6 => return .uint8 (← Struct.HasStructAccessor.get self 2)
  | 7 => return .uint16 (← Struct.HasStructAccessor.get self 1)
  | 8 => return .uint32 (← Struct.HasStructAccessor.get self 1)
  | 9 => return .uint64 (← Struct.HasStructAccessor.get self 1)
  | 10 => return .float32 (← Struct.HasStructAccessor.get self 1)
  | 11 => throw .utf8Error --return .float64 (← Struct.HasStructAccessor.get self 1)
  | 12 => return .text (← Struct.HasStructAccessor.get self 0)
  | 13 => return .data (← Struct.HasStructAccessor.get self 0)
  | 14 => return .list (← Struct.HasStructAccessor.get self 0)
  | 15 => return .enum (← Struct.HasStructAccessor.get self 1)
  | 16 => return .struct (← Struct.HasStructAccessor.get self 0)
  | 17 => return .interface ()
  | 18 => return .anyPointer (← Struct.HasStructAccessor.get self 0)
  | _ => throw .enumOOB

end Value

namespace Annotation
variable (self : Annotation)
def id : DecodeM Id :=
  Struct.HasStructAccessor.get self 0
def brand : DecodeM Brand :=
  Struct.HasStructAccessor.get self 1
def value : DecodeM Value :=
  Struct.HasStructAccessor.get self 0
end Annotation

namespace CapnpVersion
variable (self : CapnpVersion)
def major : DecodeM UInt16 :=
  Struct.HasStructAccessor.get self 0
def minor : DecodeM UInt8 :=
  Struct.HasStructAccessor.get self 2
def micro : DecodeM UInt8 :=
  Struct.HasStructAccessor.get self 3
end CapnpVersion

def CodeGeneratorRequest.RequestedFile.Import := Struct
instance : Struct.IsStruct CodeGeneratorRequest.RequestedFile.Import where
  fromStruct := id
  expectedDataWords := 1
  expectedPtrWords := 1

namespace CodeGeneratorRequest.RequestedFile.Import
variable (self : CodeGeneratorRequest.RequestedFile.Import)
def id : DecodeM Id :=
  Struct.HasStructAccessor.get self 0
def name : DecodeM Text :=
  Struct.HasStructAccessor.get self 0
end CodeGeneratorRequest.RequestedFile.Import

def CodeGeneratorRequest.RequestedFile := Struct
instance : Struct.IsStruct CodeGeneratorRequest.RequestedFile where
  fromStruct := id
  expectedDataWords := 1
  expectedPtrWords := 2

namespace CodeGeneratorRequest.RequestedFile
variable (self : CodeGeneratorRequest.RequestedFile)
def id : DecodeM <| Id :=
  Struct.HasStructAccessor.get self 0
def filename : DecodeM <| Text :=
  Struct.HasStructAccessor.get self 0
def imports : DecodeM <| List.P CodeGeneratorRequest.RequestedFile.Import :=
  Struct.HasStructAccessor.get self 1
end CodeGeneratorRequest.RequestedFile

def CodeGeneratorRequest := Struct
instance : Struct.IsStruct CodeGeneratorRequest where
  fromStruct := id
  expectedDataWords := 0
  expectedPtrWords := 4

namespace CodeGeneratorRequest
variable (self : CodeGeneratorRequest)
def capnpVersion : DecodeM <| CapnpVersion :=
  Struct.HasStructAccessor.get self 2
def nodes : DecodeM <| List.P Node :=
  Struct.HasStructAccessor.get self 0
def sourceInfo : DecodeM <| List.P Node.SourceInfo :=
  Struct.HasStructAccessor.get self 3
def requestedFiles : DecodeM <| List.P CodeGeneratorRequest.RequestedFile :=
  Struct.HasStructAccessor.get self 1
end CodeGeneratorRequest
