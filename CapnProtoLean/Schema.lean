import CapnProtoLean.Encoding
import CapnProtoLean.Meta

namespace CapnProtoLean

open Meta

abbrev Id := UInt64

mutual
inductive Node.Parameter
| mk (name : String)

inductive Node.NestedNode
| mk
  (name : String)
  (id : Id)

inductive Node.Body : Type
| file
| struct
  (dataWordCount : UInt16)
  (pointerCount : UInt16)
  (preferredListEncoding : ElementSize)
  (isGroup : Bool)
  (discriminantCount : UInt16)
  (discriminantOffset : UInt32)
  (fields : List Field)
| enum
  (enumerants : List Enumerant)
| interface
  (methods : List Method)
  (superclasses : List Superclass)
| const
  (type : «Type»)
  (value : Value)
| annotation
  (type : «Type»)
  (targetsFile : «Type»)
  (targetsConst : «Type»)
  (targetsEnum : «Type»)
  (targetsEnumerant : «Type»)
  (targetsStruct : «Type»)
  (targetsField : «Type»)
  (targetsUnion : «Type»)
  (targetsGroup : «Type»)
  (targetsInterface : «Type»)
  (targetsMethod : «Type»)
  (targetsParam : «Type»)
  (targetsAnnotation : «Type»)

inductive Node.SourceInfo
| mk

inductive Node
| mk
  (id : Id)
  (displayName : String)
  (displayNamePrefixLength : UInt32)
  (scopeId : Id)
  (parameters : List Node.Parameter)
  (isGeneric : Bool)
  (nestedNodes : List Node.NestedNode)
  (annotations : List Annotation)
  (body : Node.Body)

inductive Field.Body
| slot
  (offset : UInt32)
  (type : «Type»)
  (defaultValue : Value)
  (hadExplicitDefault : Bool)
| group
  (typeId : Id)

inductive Field.Ordinal
| implicit
| explicit (_ : UInt16)

inductive Field
| mk
  (name : String)
  (codeOrder : UInt16)
  (annotations : List Annotation)
  (discriminantValue : UInt16)
  (body : Field.Body)
  (ordinal : Field.Ordinal)

inductive ElementSize
| empty
| bit
| byte
| twoBytes
| fourBytes
| eightBytes
| pointer
| inlineComposite

inductive Enumerant
| mk
  (name : String)
  (codeOrder : UInt16)
  (annotations : List Annotation)

inductive Superclass
| mk
  (id : Id)
  (brand : Brand)

inductive Method
| mk
  (name : String)
  (codeOrder : UInt16)
  (implicitParameters : List Node.Parameter)
  (paramStructType : Id)
  (paramBrand : Brand)
  (resultStructType : Id)
  (resultBrand : Brand)
  (annotations : List Annotation)

inductive «Type».Body.AnyPointer.Unconstrained
| anyKind
| struct
| list
| capability

inductive «Type».Body.AnyPointer
| unconstrained (u : «Type».Body.AnyPointer.Unconstrained)
| parameter
  (scopeId : Id)
  (parameterIndex : UInt16)
| implicitMethodParameter
  (parameterIndex : UInt16)

inductive «Type».Body
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
| list
  (elementType : «Type»)
| enum
  (typeId : Id)
  (brand : Brand)
| struct
  (typeId : Id)
  (brand : Brand)
| interface
  (typeId : Id)
  (brand : Brand)
| anyPointer (a : «Type».Body.AnyPointer)

inductive «Type»
| mk
  (body : «Type».Body)

inductive Brand.Scope.Body
| bind (l : List Brand.Binding)
| inherit

inductive Brand.Scope
| mk
  (scopeId : Id)
  (body : Brand.Scope.Body)

inductive Brand.Binding.Body
| unbound
| type (t : «Type»)

inductive Brand.Binding
| mk
  (body : Brand.Binding.Body)

inductive Brand
| mk
  (scopes : List Brand.Scope)

inductive Value.Body
| void (_ : Unit)
| bool (_ : Bool)
| int8 (_ : Int8)
| int16 (_ : Int16)
| int32 (_ : Int32)
| int64 (_ : Int64)
| uint8 (_ : UInt8)
| uint16 (_ : UInt16)
| uint32 (_ : UInt32)
| uint64 (_ : UInt64)
| float32 (_ : Float32)
| float64 (_ : Float64)
| text (_ : String)
| data (_ : ByteArray)
| list (_ : AnyPointer)
| enum (_ : UInt16)
| struct (_ : AnyPointer)
| interface
| anyPointer (_ : AnyPointer)

inductive Value
| mk
  (body : Value.Body)

inductive Annotation
| mk
  (id : Id)
  (brand : Brand)
  (value : Value)

end

deriving instance Repr for Node, Node.SourceInfo

structure CapnpVersion where
  major : UInt16
  minor : UInt8
  micro : UInt8

namespace CapnpVersion
deriving instance Inhabited, Repr for CapnpVersion
instance : ToString CapnpVersion where
  toString x := s!"{x.major}.{x.minor}.{x.micro}"
end CapnpVersion


inductive CodeGeneratorRequest.RequestedFile.Import
| mk
  (id : Id)
  (name : String)
deriving Repr

inductive CodeGeneratorRequest.RequestedFile
| mk
  (id : Id)
  (filename : String)
  (imports : Array CodeGeneratorRequest.RequestedFile.Import)
deriving Repr

structure CodeGeneratorRequest where
  capnpVersion : Option CapnpVersion
  nodes : Array Node
  sourceInfo : Array Node.SourceInfo
  requestedFiles : Array CodeGeneratorRequest.RequestedFile
deriving Repr


open Decoder

mutual
def CapnpVersion.decoder : StructDecoder CapnpVersion :=
  fun dataWords ptrWords =>
  context "capnpversion" do
  if dataWords ≠ 1 then
    error s!"CapnpVersion: dataWords: {dataWords}"
  if ptrWords ≠ 0 then
    error s!"CapnpVersion: ptrWords: {ptrWords}"
  let major ← context "data[0]" <| moveOffBytes 0 <| uint16
  let minor ← context "data[2]" <| moveOffBytes 2 <| uint8
  let micro ← context "data[3]" <| moveOffBytes 3 <| uint8
  return {major,minor,micro}

def CodeGeneratorRequest.RequestedFile.Import.decoder
    : StructDecoder CodeGeneratorRequest.RequestedFile.Import :=
  fun dataWords ptrWords =>
  context "codegeneratorrequest.RequestedFile.Import" do
  if dataWords ≠ 1 then
    error s!"dataWords: {dataWords}"
  if ptrWords ≠ 1 then
    error s!"ptrWords: {ptrWords}"
  let id ← context "data[0]" <| moveOffBytes 0 <| uint64
  let name ← context "ptr[0]" <| moveOffWords (1 + 0) <| text
  return .mk (id := id) (name := name)

def CodeGeneratorRequest.RequestedFile.decoder : StructDecoder CodeGeneratorRequest.RequestedFile :=
  fun dataWords ptrWords =>
  context "codegeneratorrequest.RequestedFile" do
  if dataWords ≠ 1 then
    error s!"dataWords: {dataWords}"
  if ptrWords ≠ 2 then
    error s!"ptrWords: {ptrWords}"
  let id ← context "data[0]" <| moveOffBytes 0 <| uint64
  let filename ← context "ptr[0]" <| moveOffWords (1 + 0) <| text
  let imports ← context "ptr[1]" <| moveOffWords (1 + 1) <|
    listPtrStruct CodeGeneratorRequest.RequestedFile.Import.decoder
  return .mk (id := id) (filename := filename) (imports := imports)

def CodeGeneratorRequest.decoder : StructDecoder CodeGeneratorRequest :=
  fun dataWords ptrWords =>
  context "codegeneratorrequest" do
  if dataWords ≠ 0 then
    error s!"CodeGenReq: dataWords: {dataWords}"
  if ptrWords ≠ 4 then
    error s!"CodeGenReq: ptrWords: {ptrWords}"
  let capnpVersion ← context "ptr[2]" <| moveOffWords 2 <| structPtr CapnpVersion.decoder
  let nodes := #[]
  let sourceInfo := #[]
  let requestedFiles ← context "ptr[1]" <| moveOffWords (0 + 1) <|
    listPtrStruct CodeGeneratorRequest.RequestedFile.decoder
  return {capnpVersion,nodes,sourceInfo,requestedFiles}
end
