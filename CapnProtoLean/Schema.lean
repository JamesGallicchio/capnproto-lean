import CapnProtoLean.Basic
import CapnProtoLean.Meta

namespace CapnProtoLean

open Meta

abbrev Id := UInt64

declare_nonempty_type Node
declare_nonempty_type Field
declare_nonempty_type ElementSize
declare_nonempty_type Enumerant
declare_nonempty_type Method
declare_nonempty_type Superclass
declare_nonempty_type «Type»
declare_nonempty_type Value
declare_nonempty_type Annotation
declare_nonempty_type Brand

namespace Node

declare_nonempty_type Parameter
declare_nonempty_type NestedNode
declare_nonempty_type SourceInfo

structure Parameter.View where
  name : Text
deriving Inhabited
declare_view Parameter Parameter.View

structure NestedNode.View where
  name : Text
  id : Id
deriving Inhabited
declare_view NestedNode NestedNode.View

inductive Body : Type
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
deriving Inhabited

structure View where
  id : Id
  displayName : Text
  displayNamePrefixLength : UInt32
  scopeId : Id
  parameters : List Parameter
  isGeneric : Bool
  nestedNodes : List NestedNode
  annotations : List Annotation
  body : Body
deriving Inhabited

end Node

declare_view Node Node.View

namespace Field

declare_nonempty_type Annotation

inductive Body
| slot
  (offset : UInt32)
  (type : «Type»)
  (defaultValue : Value)
  (hadExplicitDefault : Bool)
| group
  (typeId : Id)
deriving Inhabited

inductive Ordinal
| implicit
| explicit (_ : UInt16)
deriving Inhabited

structure View where
  name : Text
  codeOrder : UInt16
  annotations : List Annotation
  discriminantValue : UInt16
  body : Body
  ordinal : Ordinal
deriving Inhabited

end Field
declare_view Field Field.View

namespace ElementSize

inductive Enum
| empty
| bit
| byte
| twoBytes
| fourBytes
| eightBytes
| pointer
| inlineComposite
deriving Inhabited

end ElementSize

declare_view ElementSize ElementSize.Enum

namespace Enumerant
structure View where
  name : Text
  codeOrder : UInt16
  annotations : List Annotation
deriving Inhabited
end Enumerant
declare_view Enumerant Enumerant.View

namespace Superclass
structure View where
  id : Id
  brand : Brand
deriving Nonempty
end Superclass
declare_view Superclass Superclass.View

namespace Method
structure View where
  name : Text
  codeOrder : UInt16
  implicitParameters : List Node.Parameter
  paramStructType : Id
  paramBrand : Brand
  resultStructType : Id
  resultBrand : Brand
  annotations : List Annotation
deriving Nonempty
end Method
declare_view Method Method.View

namespace «Type»

inductive Body.AnyPointer.Unconstrained
| anyKind
| struct
| list
| capability

inductive Body.AnyPointer
| unconstrained (u : Body.AnyPointer.Unconstrained)
| parameter
  (scopeId : Id)
  (parameterIndex : UInt16)
| implicitMethodParameter
  (parameterIndex : UInt16)

inductive Body
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
| anyPointer (a : Body.AnyPointer)
deriving Inhabited

structure View where
  body : Body
deriving Inhabited

end «Type»
declare_view «Type» «Type».View

namespace Brand
declare_nonempty_type Scope
declare_nonempty_type Binding

namespace Scope
inductive Body
| bind (l : List Binding)
| inherit
deriving Inhabited

structure View where
  scopeId : Id
  body : Body
deriving Inhabited
end Scope
declare_view Scope Scope.View


namespace Binding
inductive Body
| unbound
| type (t : «Type»)
deriving Nonempty

structure View where
  body : Body
deriving Nonempty
end Binding
declare_view Binding Binding.View

structure View where
  scopes : List Scope
deriving Nonempty
end Brand
declare_view Brand Brand.View

namespace Value
inductive Body
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
| text (_ : Text)
| data (_ : ByteArray)
| list (_ : AnyPointer)
| enum (_ : UInt16)
| struct (_ : AnyPointer)
| interface
| anyPointer (_ : AnyPointer)
deriving Nonempty

structure View where
  body : Body
deriving Nonempty
end Value
declare_view Value Value.View

namespace Annotation
structure View where
  id : Id
  brand : Brand
  value : Value
deriving Nonempty
end Annotation
declare_view Annotation Annotation.View

declare_nonempty_type CapnpVersion

namespace CapnpVersion
structure View where
  major : UInt16
  minor : UInt8
  micro : UInt8
deriving Inhabited, Repr
end CapnpVersion
declare_view CapnpVersion CapnpVersion.View
namespace CapnpVersion
instance : Inhabited CapnpVersion := ⟨.ofView default⟩
instance : Repr CapnpVersion := ⟨(reprPrec ·.view)⟩
instance : ToString View where
  toString x := s!"{x.major}.{x.minor}.{x.micro}"
instance : ToString CapnpVersion where
  toString x := toString x.view
end CapnpVersion

declare_nonempty_type CodeGeneratorRequest

namespace CodeGeneratorRequest
declare_nonempty_type RequestedFile

namespace RequestedFile
declare_nonempty_type Import

namespace Import
structure View where
  id : Id
  name : Text
deriving Nonempty
end Import
declare_view Import Import.View

structure View where
  id : Id
  filename : Text
  imports : List Import
deriving Nonempty
end RequestedFile
declare_view RequestedFile RequestedFile.View

structure View where
  capnpVersion : CapnpVersion
  nodes : List Node
  sourceInfo : List Node.SourceInfo
  requestedFiles : List RequestedFile
deriving Inhabited
end CodeGeneratorRequest
declare_view CodeGeneratorRequest CodeGeneratorRequest.View

partial def CodeGeneratorRequest.fromBytes [Monad m]
      (getBytes : m (Option ByteArray)) : m CodeGeneratorRequest := do
  let mut bytes := 0
  while true do
    match ← getBytes with
    | none => break
    | some bs => bytes := bytes + bs.size
  return .ofView default
