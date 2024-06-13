import CapnProtoLean.Int
import Batteries

namespace CapnProtoLean

instance : Repr ByteArray where
  reprPrec arr prec :=
    .group ("ByteArray.mk" ++ .line ++ reprPrec arr.data prec)

structure Segment where
  data : ByteArray
deriving Inhabited, Repr

structure Message where
  segments : Array Segment
deriving Inhabited, Repr

namespace Message

structure Loc where
  /-- Index of segment in message -/
  segIdx : UInt32
  /-- Index within a segment, in *bytes* -/
  idx : UInt32
deriving Inhabited

namespace Loc

def plusBytes (w : UInt32) (l : Loc) : Loc := {
  segIdx := l.segIdx
  idx := l.idx + w
}

def plusWords (w : UInt32) (l : Loc) : Loc := {
  segIdx := l.segIdx
  idx := l.idx + 8 * w
}

instance : ToString Loc where
  toString := fun {segIdx, idx} =>
    s!"{segIdx}:{idx}"

end Loc

/-- Read a `Message` in by repeatedly reading from a handle `h` -/
partial def fromHandle (h : IO.FS.Handle) : IO Message := do
  -- Number of segments is the first UInt32 of the stream, plus 1
  let sizeSegs ← try (do
    let numSegs := (← readUInt32) + 1
    let mut sizeSegs := #[]
    for _ in [0:numSegs.val] do
      sizeSegs := sizeSegs.push (← readUInt32)
    if (4 + numSegs*4) % 8 = 4 then
      let _ ← readUInt32
    return sizeSegs)
    catch _ => throw (.userError "Failed to parse message header")
  let segs : Array Segment ← sizeSegs.mapM (fun size =>
    return {data := ← readBytesExact (8 * size.toUSize)}
  )
  return {segments := segs}
where
  readUInt32 : IO UInt32 := do
    let bytes ← readBytesExact 4
    if h : bytes.size = 4 then
      return bytes.ugetUInt32LE 0 (by rw [h]; decide)
    throw (.userError s!"readBytesExact: expected 4 bytes, got {bytes.size}")
  readBytesExact (size : USize) : IO ByteArray := do
    readBytesExactAux size (ByteArray.mkEmpty size.val)
  readBytesExactAux (size : USize) (acc : ByteArray) : IO ByteArray := do
    if size = 0 then
      return acc
    let new ← h.read size
    let newLen := new.size.toUSize
    if newLen = 0 then
      throw (.userError s!"readBytesExact: reached EOF")
    let acc := acc ++ new
    readBytesExactAux (size - newLen) acc

inductive OutOfBounds
  | seg (loc : Message.Loc) (segs : Nat)
  | idx (loc : Message.Loc) (len : Nat) (segLen : Nat)

variable (m : Message) (l : Loc) in
section

@[inline]
def getUInt8 : Except OutOfBounds UInt8 := do
  if h : _ then
    let seg := m.segments.uget l.segIdx.toUSize h
    if h : _ then
      return seg.data.uget l.idx.toUSize h
    else
      throw <| OutOfBounds.idx l 1 seg.data.size
  else
    throw <| OutOfBounds.seg l m.segments.size

@[inline]
def getUInt16 : Except OutOfBounds UInt16 := do
  if h : _ then
    let seg := m.segments.uget l.segIdx.toUSize h
    if h : _ then
      return seg.data.ugetUInt16LE l.idx.toUSize h
    else
      throw <| OutOfBounds.idx l 1 seg.data.size
  else
    throw <| OutOfBounds.seg l m.segments.size

@[inline]
def getUInt32 : Except OutOfBounds UInt32 := do
  if h : _ then
    let seg := m.segments.uget l.segIdx.toUSize h
    if h : _ then
      return seg.data.ugetUInt32LE l.idx.toUSize h
    else
      throw <| OutOfBounds.idx l 1 seg.data.size
  else
    throw <| OutOfBounds.seg l m.segments.size

def getUInt64 : Except OutOfBounds UInt64 := do
  if h : _ then
    let seg := m.segments.uget l.segIdx.toUSize h
    if h : _ then
      return seg.data.ugetUInt64LE l.idx.toUSize h
    else
      throw <| OutOfBounds.idx l 1 seg.data.size
  else
    throw <| OutOfBounds.seg l m.segments.size
end

end Message

structure AnyPointer where
  data : UInt64
deriving Inhabited, Repr

namespace AnyPointer

def isNull (p : AnyPointer) : Bool := p.data = 0

def isStruct (p : AnyPointer) : Bool :=
  p.data &&& 0x3 = 0
def isList (p : AnyPointer) : Bool :=
  p.data &&& 0x3 = 1
def isFar (p : AnyPointer) : Bool :=
  p.data &&& 0x3 = 2
def isOther (p : AnyPointer) : Bool :=
  p.data &&& 0x3 = 3

instance : ToString AnyPointer where
  toString p :=
    let digits := Nat.toDigits 16 p.data.toNat
    let digits := List.leftpad 16 '0' digits
    String.mk ('0' :: 'x' :: digits)
end AnyPointer

structure SegmentPointer extends AnyPointer where
  isStruct_or_isList : toAnyPointer.isStruct ∨ toAnyPointer.isList
    := by first | decide | simp_all

namespace SegmentPointer

instance : Inhabited SegmentPointer where
  default := {
    data := 0
  }

/-- Offset, in words, from the end of the pointer to the
    start of the object's data. -/
def offset (s : SegmentPointer) : Int32 :=
  -- Get lower 32 bits
  let low : UInt32 := s.data.toUInt32
  -- Sign-extend by two bits
  let shifted : UInt32 := low.shiftArithRight 2
  shifted

end SegmentPointer

structure StructPointer extends SegmentPointer where
  isStruct : toSegmentPointer.isStruct := by first | decide | simp_all

namespace StructPointer
instance : Inhabited StructPointer where
  default := {
    data := 0
  }

/-- Size of the struct's data section, in words. -/
def dataSize (s : StructPointer) : UInt16 :=
  (s.data >>> 32).toUInt16

/-- Size of the struct's pointer section, in words. -/
def pointerSize (s : StructPointer) : UInt16 :=
  (s.data >>> 48).toUInt16

end StructPointer

structure ListPointer extends SegmentPointer where
  isList : toSegmentPointer.isList := by first | decide | simp_all

namespace ListPointer
instance : Inhabited ListPointer where
  default := { data := 1 }

/--
Size of each element:
    0 = 0 (e.g. List(Void))
    1 = 1 bit
    2 = 1 byte
    3 = 2 bytes
    4 = 4 bytes
    5 = 8 bytes (non-pointer)
    6 = 8 bytes (pointer)
    7 = composite
-/
def elemSize (s : ListPointer) : UInt8 :=
  (s.data >>> 32).toUInt8 &&& 0b111

/--
Size of the list:
    when C <> 7: Number of elements in the list.
    when C = 7: Number of words in the list, not counting the tag word
    (see below).
-/
def size (s : ListPointer) : UInt32 :=
  (s.data >>> 35).toUInt32

end ListPointer

structure FarPointer extends AnyPointer where
  isFar : toAnyPointer.isFar := by first | decide | simp_all


namespace FarPointer
instance : Inhabited FarPointer := ⟨⟨⟨2⟩, by decide⟩⟩

/--
If B == 0, then the “landing pad” of a far pointer
is normally just another pointer,
which in turn points to the actual object.

If B == 1, then the “landing pad”
is itself another far pointer that is interpreted differently:
This far pointer (which always has B = 0)
points to the start of the object’s content,
located in some other segment.
The landing pad is itself immediately followed by a tag word.
The tag word looks exactly like an intra-segment pointer
to the target object would look, except that the offset is always zero.

The reason for the convoluted double-far convention
is to make it possible to form a new pointer to an object
in a segment that is full.
If you can’t allocate even one word
in the segment where the target resides,
then you will need to allocate a landing pad in some other segment,
and use this double-far approach.
This should be exceedingly rare in practice
since pointers are normally set to point to new objects,
not existing ones.
-/
def landingPadIsFar (f : FarPointer) : Bool :=
  f.data &&& 0b100 != 0

/--
Offset, in words, from the start of the target segment
to the location of the far-pointer landing-pad within that
segment. Unsigned.
-/
def offset (f : FarPointer) : UInt32 :=
  ((f.data >>> 3) &&& 0x1fffffff).toUInt32

/--
ID of the target segment.
(Segments are numbered sequentially starting from zero.)
-/
def segmentId (f : FarPointer) : UInt32 :=
  (f.data >>> 32).toUInt32

/-- Get message location from the segment ID and offset in `p`-/
def toLoc (p : FarPointer) : Message.Loc :=
  {segIdx := p.segmentId, idx := 8 * p.offset}

end FarPointer

structure OtherPointer extends AnyPointer where
  isOther : toAnyPointer.isOther

namespace OtherPointer
instance : Inhabited OtherPointer := ⟨⟨⟨3⟩, by decide⟩⟩
end OtherPointer

structure DataPointer extends ListPointer where
  h : toListPointer.elemSize = 2 := by first | decide | simp_all

namespace DataPointer
instance : Inhabited DataPointer where
  default := {
    data := 0x200000001
  }
end DataPointer

/-- CapnProto specific meaning here.
Default value should be the same as a value where everything is zero. -/
class HasDefault (α : Type) where
  default : α

namespace HasDefault
instance : HasDefault Bool := ⟨false⟩
instance : HasDefault UInt8  := ⟨0⟩
instance : HasDefault UInt16 := ⟨0⟩
instance : HasDefault UInt32 := ⟨0⟩
instance : HasDefault UInt64 := ⟨0⟩
instance : HasDefault Int8  := ⟨show UInt8 from 0⟩
instance : HasDefault Int16 := ⟨show UInt16 from 0⟩
instance : HasDefault Int32 := ⟨show UInt32 from 0⟩
instance : HasDefault Int64 := ⟨show UInt64 from 0⟩
instance : HasDefault AnyPointer := ⟨⟨show UInt64 from 0⟩⟩
end HasDefault

structure Struct where
  start : Message.Loc
  (dataWords ptrWords : UInt16)

namespace Struct

instance : HasDefault Struct where
  default := {
    start := default
    dataWords := 0
    ptrWords := 0
  }

class IsStruct (α : Type) where
  fromStruct : Struct → α
  (expectedDataWords expectedPtrWords : UInt16)

end Struct

structure List where
  start : Message.Loc
  elemSize : UInt8
  hElemSize : elemSize < 8
  dataSize : UInt32
  tag? : if elemSize = 7 then StructPointer else Unit

namespace List

instance : HasDefault List where
  default := {
    start := default
    elemSize := 0
    hElemSize := by decide
    dataSize := 0
    tag? := ()
  }

variable (self : List)

@[inline]
def elemCt : UInt32 :=
  if h : self.elemSize = 7 then
    (show StructPointer by simpa [h] using self.tag?).offset
  else
    self.dataSize

/-- Element width, in *bytes* -/
@[inline]
def elemWidth : UInt32 :=
  match self with
  | {elemSize := ⟨⟨0,_⟩⟩, ..} => 0
  | {elemSize := ⟨⟨1,_⟩⟩, ..}
  | {elemSize := ⟨⟨2,_⟩⟩, ..} => 1
  | {elemSize := ⟨⟨3,_⟩⟩, ..} => 2
  | {elemSize := ⟨⟨4,_⟩⟩, ..} => 4
  | {elemSize := ⟨⟨5,_⟩⟩, ..}
  | {elemSize := ⟨⟨6,_⟩⟩, ..} => 8
  | {elemSize := ⟨⟨7,_⟩⟩, tag?, ..} =>
    let sp : StructPointer := tag?
    8 * (sp.dataSize + sp.pointerSize).toUInt32
  | {elemSize := ⟨⟨n+8,_⟩⟩, ..} =>
    by contradiction

structure OutOfBounds where
  (elemCt idx : UInt32)

@[inline]
def getLocOfIdx (i : UInt32) : Except OutOfBounds Message.Loc :=
  if i < self.elemCt then
    return self.start.plusBytes (self.elemWidth * i)
  else
    throw {elemCt := self.elemCt, idx := i}

inductive ElemSizeError
  | prim (expected actual : UInt8)
  | structGotBool

end List

namespace AnyPointer

inductive ResolveError
  | unexpectedPointer (p : AnyPointer)
  | unexpectedLandingPad (fp : FarPointer) (lp : AnyPointer)
  | unexpectedFarLPTag (fp : FarPointer) (lp : FarPointer) (tag : AnyPointer)
  | unexpectedListTag (p : ListPointer) (tag : AnyPointer)
  | unexpectedListElemSize (p : ListPointer) (elemSize : UInt8)
  | oob (p : ListPointer) (oob : Message.OutOfBounds)
  | oob2 (fp : FarPointer) (oob : Message.OutOfBounds)
  | oob3 (fp lp : FarPointer) (oob : Message.OutOfBounds)

abbrev ReadM := ReaderT Message <| Except ResolveError

/--
Resolve pointer `p` as a struct pointer, assuming it was at location `loc`.
-/
@[inline]
private def resolveStructPtr (loc : Message.Loc) (p : AnyPointer) : ReadM Struct := do
  if p.isNull then
    return HasDefault.default

  if h : p.isStruct then
    return ← handleSegPointer {p with}

  else if h : p.isFar then
    let fp : FarPointer := {p with}
    let lp : AnyPointer := ⟨←
      (← read).getUInt64 fp.toLoc |>.mapError (ResolveError.oob2 fp ·)
    ⟩
    if !fp.landingPadIsFar then
      if lp.isNull then
        throw <| ResolveError.unexpectedLandingPad fp lp

      if h : lp.isStruct then
        return ← handleSegPointer {lp with}
      else
        throw <| ResolveError.unexpectedLandingPad fp lp
    else
      if h : lp.isFar then
        let lp : FarPointer := {lp with}
        let tag : AnyPointer := ⟨←
          (← read).getUInt64 (fp.toLoc.plusWords 1) |>.mapError (ResolveError.oob3 fp lp ·)
        ⟩
        if h : tag.isStruct then
          let tag : StructPointer := {tag with}
          return {
            start := lp.toLoc
            dataWords := tag.dataSize
            ptrWords := tag.pointerSize
          }
        else
          throw <| .unexpectedFarLPTag fp lp tag
      else
        throw <| .unexpectedLandingPad fp lp
  else
    throw <| .unexpectedPointer p

where
  handleSegPointer (p : StructPointer) := do
    return {
      start := {segIdx := loc.segIdx, idx := loc.idx + p.offset + 1}
      dataWords := p.dataSize
      ptrWords := p.pointerSize
    }

/--
Resolve pointer `p` as a list pointer, assuming it was at location `loc`.
-/
@[inline]
private def resolveListPtr (loc : Message.Loc) (p : AnyPointer) : ReadM List := do
  if p.isNull then
    return HasDefault.default

  if h : p.isList then
    let p : ListPointer := {p with}
    let start := loc.plusWords (p.offset + (1 : Int32))
    findTag start p

  else if h : p.isFar then
    let fp : FarPointer := {p with}
    let lp : AnyPointer := ⟨←
      (← read).getUInt64 fp.toLoc |>.mapError (ResolveError.oob2 fp ·)
    ⟩
    if !fp.landingPadIsFar then
      if lp.isNull then
        throw <| .unexpectedLandingPad fp lp

      if h : lp.isList then
        let lp : ListPointer := {lp with}
        let start := loc.plusWords (lp.offset + (1 : Int32))
        findTag start lp
      else
        throw <| .unexpectedLandingPad fp lp
    else
      if h : lp.isFar then
        let lp : FarPointer := {lp with}
        let tag : AnyPointer := ⟨←
          (← read).getUInt64 (fp.toLoc.plusWords 1) |>.mapError (ResolveError.oob3 fp lp ·)
        ⟩
        if h : tag.isList then
          let tag : ListPointer := {tag with}
          let start := lp.toLoc
          findTag start tag
        else
          throw <| .unexpectedFarLPTag fp lp tag
      else
        throw <| .unexpectedLandingPad fp lp
  else
    throw <| .unexpectedPointer p
where findTag (start) (p : ListPointer) : ReadM List := do
  if h : p.elemSize = 7 then
    let tag : AnyPointer := ⟨←
      (← read).getUInt64 start |>.mapError (ResolveError.oob p ·)
    ⟩
    if h : tag.isStruct then
      let tag : StructPointer := {tag with}
      if hElemSize : _ then
        return {
          start := start.plusWords 1
          elemSize := p.elemSize
          hElemSize
          dataSize := p.size
          tag? := by simp [*]; exact tag
        }
      else
        throw (ResolveError.unexpectedListElemSize p p.elemSize)
    else
      throw <| .unexpectedListTag p tag
  else
    if hElemSize : _ then
      return {
        start
        elemSize := p.elemSize
        hElemSize
        dataSize := p.size
        tag? := by simp [*]; exact ()
      }
    else
      throw (ResolveError.unexpectedListElemSize p p.elemSize)

end AnyPointer

inductive DecodeErr
| messageOOB (e : Message.OutOfBounds)
| resolveErr (e : AnyPointer.ResolveError)
| listOOB (e : List.OutOfBounds)
| listElemSize (e : List.ElemSizeError)
| utf8Error
| enumOOB

instance : Coe Message.OutOfBounds DecodeErr := ⟨DecodeErr.messageOOB⟩
instance : Coe AnyPointer.ResolveError DecodeErr := ⟨DecodeErr.resolveErr⟩
instance : Coe List.OutOfBounds DecodeErr := ⟨DecodeErr.listOOB⟩
instance : Coe List.ElemSizeError DecodeErr := ⟨DecodeErr.listElemSize⟩

def DecodeM := ReaderT Message <| Except DecodeErr
deriving Monad, MonadReader, MonadExcept

instance : MonadLift (AnyPointer.ReadM) DecodeM where
monadLift m := do
    match m (← read) with
    | .ok a => return a
    | .error e => throw (↑e)

instance : MonadLift (Except Message.OutOfBounds) DecodeM where
monadLift m := do
    match m with
    | .ok a => return a
    | .error e => throw (↑e)

instance : MonadLift (Except List.OutOfBounds) DecodeM where
monadLift m := do
    match m with
    | .ok a => return a
    | .error e => throw (↑e)

instance : MonadLift (Except List.ElemSizeError) DecodeM where
monadLift m := do
    match m with
    | .ok a => return a
    | .error e => throw (↑e)

namespace Struct

variable (self : Struct)

/-- Read a `Bool` at `offset` bits into the struct's data section. -/
@[inline]
def bool (offset : UInt32) : DecodeM Bool := do
  if offset < 64 * self.dataWords.toUInt32 then
    let byteIdx := offset / 8
    let bitIdx := (offset % 8).toUInt8
    let byte ← (← read).getUInt8 (self.start.plusBytes byteIdx)
    return (byte >>> bitIdx) &&& 0b1 = 0b1
  else
    return HasDefault.default

/-- Read a `UInt8` at `offset` bytes into the struct's data section. -/
@[inline]
def uint8 (offset : UInt32) : DecodeM UInt8 := do
  if offset < 8 * self.dataWords.toUInt32 then
    (← read).getUInt8 (self.start.plusBytes offset)
  else
    return HasDefault.default

/-- Read a `UInt16` at `offset` bytes into the struct's data section. -/
@[inline]
def uint16 (offset : UInt32) : DecodeM UInt16 := do
  if offset < 8 * self.dataWords.toUInt32 then
    (← read).getUInt16 (self.start.plusBytes (2 * offset))
  else
    return HasDefault.default

/-- Read a `UInt32` at `offset` bytes into the struct's data section. -/
@[inline]
def uint32 (offset : UInt32) : DecodeM UInt32 := do
  if offset < 8 * self.dataWords.toUInt32 then
    (← read).getUInt32 (self.start.plusBytes (4 * offset))
  else
    return HasDefault.default

/-- Read a `UInt64` at `offset` bytes into the struct's data section. -/
@[inline]
def uint64 (offset : UInt32) : DecodeM UInt64 := do
  if offset < 8 * self.dataWords.toUInt32 then
    (← read).getUInt64 (self.start.plusBytes (8 * offset))
  else
    return HasDefault.default

/-- Read a `Int8` at `offset` bytes into the struct's data section. -/
@[inline]
def int8 (offset : UInt32) : DecodeM Int8 := do
  return (← self.uint8 offset)

/-- Read a `Int16` at `offset` bytes into the struct's data section. -/
@[inline]
def int16 (offset : UInt32) : DecodeM Int16 := do
  return (← self.uint16 offset)

/-- Read a `Int32` at `offset` bytes into the struct's data section. -/
@[inline]
def int32 (offset : UInt32) : DecodeM Int32 := do
  return (← self.uint32 offset)

/-- Read a `Int64` at `offset` bytes into the struct's data section. -/
@[inline]
def int64 (offset : UInt32) : DecodeM Int64 := do
  return (← self.uint64 offset)

/-- Read a `Float32` at `offset` bytes into the struct's data section. -/
@[inline]
def float32 (offset : UInt32) : DecodeM Float32 := do
  return (← self.uint32 offset)

/-- Read a `Float64` at `offset` bytes into the struct's data section. -/
@[inline]
def float64 (offset : UInt32) : DecodeM Float64 := do
  return Float64.ofUInt64 (← self.uint64 offset)

/-- Read a pointer at `offset` *words* into the struct's pointer section. -/
@[inline]
def anyPointer (offset : UInt32) : DecodeM AnyPointer := do
  if offset < self.ptrWords.toUInt32 then
    return ⟨← (← read).getUInt64 (self.start.plusWords (self.dataWords.toUInt32 + offset))⟩
  else
    return HasDefault.default

@[inline]
def struct (offset : UInt32) : DecodeM Struct := do
  if offset < self.ptrWords.toUInt32 then
    let loc := self.start.plusWords (self.dataWords.toUInt32 + offset)
    let p : AnyPointer := ⟨← (← read).getUInt64 loc⟩
    p.resolveStructPtr loc
  else
    return HasDefault.default

@[inline]
def list (offset : UInt32) : DecodeM List := do
  if offset < self.ptrWords.toUInt32 then
    let loc := self.start.plusWords (self.dataWords.toUInt32 + offset)
    let p : AnyPointer := ⟨← (← read).getUInt64 loc⟩
    p.resolveListPtr loc
  else
    return HasDefault.default

class HasStructAccessor (α : Type) where
  get : Struct → UInt32 → DecodeM α

instance : HasStructAccessor Bool     := ⟨fun s off m => do bool s off m⟩
instance : HasStructAccessor UInt8    := ⟨fun s off m => do uint8 s off m⟩
instance : HasStructAccessor UInt16   := ⟨fun s off m => do uint16 s off m⟩
instance : HasStructAccessor UInt32   := ⟨fun s off m => do uint32 s off m⟩
instance : HasStructAccessor UInt64   := ⟨fun s off m => do uint64 s off m⟩
instance : HasStructAccessor Int8     := ⟨fun s off m => do int8 s off m⟩
instance : HasStructAccessor Int16    := ⟨fun s off m => do int16 s off m⟩
instance : HasStructAccessor Int32    := ⟨fun s off m => do int32 s off m⟩
instance : HasStructAccessor Int64    := ⟨fun s off m => do int64 s off m⟩
instance : HasStructAccessor Float32  := ⟨fun s off m => do float32 s off m⟩
instance : HasStructAccessor Float64  := ⟨fun s off m => do float64 s off m⟩
instance : HasStructAccessor AnyPointer := ⟨anyPointer⟩
instance : HasStructAccessor Struct := ⟨struct⟩
instance : HasStructAccessor List := ⟨list⟩
instance [IsStruct α] : HasStructAccessor α := ⟨(IsStruct.fromStruct <$> struct · ·)⟩

end Struct

namespace List

protected structure Void extends List where
  elemSize0 : elemSize = 0

def asVoid (self : List) : Except ElemSizeError List.Void :=
  if h : _ then return {self with elemSize0 := h}
  else .error (.prim 0 self.elemSize)

protected structure Bool extends List where
  elemSize0 : elemSize = 1

def asBool (self : List) : Except ElemSizeError List.Bool :=
  if h : _ then return {self with elemSize0 := h}
  else .error (.prim 0 self.elemSize)

protected structure UInt8 extends List where
  elemSize0 : elemSize = 2

def asUInt8 (self : List) : Except ElemSizeError List.UInt8 :=
  if h : _ then return {self with elemSize0 := h}
  else .error (.prim 0 self.elemSize)

protected structure UInt16 extends List where
  elemSize0 : elemSize = 3

def asUInt16 (self : List) : Except ElemSizeError List.UInt16 :=
  if h : _ then return {self with elemSize0 := h}
  else .error (.prim 0 self.elemSize)

protected structure UInt32 extends List where
  elemSize0 : elemSize = 4

def asUInt32 (self : List) : Except ElemSizeError List.UInt32 :=
  if h : _ then return {self with elemSize0 := h}
  else .error (.prim 0 self.elemSize)

protected structure UInt64 extends List where
  elemSize0 : elemSize = 5

def asUInt64 (self : List) : Except ElemSizeError List.UInt64 :=
  if h : _ then return {self with elemSize0 := h}
  else .error (.prim 0 self.elemSize)

protected structure Pointer extends List where
  elemSize0 : elemSize = 6

def asPointer (self : List) : Except ElemSizeError List.Pointer :=
  if h : _ then return {self with elemSize0 := h}
  else .error (.prim 0 self.elemSize)

protected structure Struct extends List where
  notBool : elemSize ≠ 0

def asStruct (self : List) : Except ElemSizeError List.Struct :=
  if h : _ then return {self with notBool := h}
  else .error (.structGotBool)

namespace Struct

variable (self : List.Struct)

/-- If interpreting this as a list of structs, how many data words are present? -/
@[inline]
def elemDataWords : UInt16 :=
  match self with
  | {elemSize := ⟨⟨0,_⟩⟩, ..} => 0
  | {elemSize := ⟨⟨1,_⟩⟩, ..}
  | {elemSize := ⟨⟨2,_⟩⟩, ..}
  | {elemSize := ⟨⟨3,_⟩⟩, ..}
  | {elemSize := ⟨⟨4,_⟩⟩, ..}
  | {elemSize := ⟨⟨5,_⟩⟩, ..} => 1
  | {elemSize := ⟨⟨6,_⟩⟩, ..} => 0
  | {elemSize := ⟨⟨7,_⟩⟩, tag?, ..} =>
    let sp : StructPointer := tag?
    sp.dataSize
  | {elemSize := ⟨⟨n+8,_⟩⟩, ..} =>
    by contradiction

/-- If interpreting this as a list of structs, how many pointer words are present? -/
@[inline]
def elemPtrWords : UInt16 :=
  match self with
  | {elemSize := ⟨⟨0,_⟩⟩, ..} => 0
  | {elemSize := ⟨⟨1,_⟩⟩, ..}
  | {elemSize := ⟨⟨2,_⟩⟩, ..}
  | {elemSize := ⟨⟨3,_⟩⟩, ..}
  | {elemSize := ⟨⟨4,_⟩⟩, ..}
  | {elemSize := ⟨⟨5,_⟩⟩, ..} => 0
  | {elemSize := ⟨⟨6,_⟩⟩, ..} => 1
  | {elemSize := ⟨⟨7,_⟩⟩, tag?, ..} =>
    let sp : StructPointer := tag?
    sp.pointerSize
  | {elemSize := ⟨⟨n+8,_⟩⟩, ..} =>
    by contradiction

end Struct

/-- Get `i`th bool. -/
def Bool.get (i : UInt32) (self : List.Bool) : DecodeM Bool := do
  let loc ← self.getLocOfIdx (i / 8)
  let byte ← (← read).getUInt8 loc
  let bool := (byte >>> (i % 8).toUInt8) &&& 0b1 = 0b1
  return bool

def UInt8.get (i : UInt32) (self : List.UInt8) : DecodeM UInt8 := do
  let loc ← self.getLocOfIdx i
  let byte ← (← read).getUInt8 loc
  return byte

def UInt16.get (i : UInt32) (self : List.UInt16) : DecodeM UInt16 := do
  let loc ← self.getLocOfIdx i
  let res ← (← read).getUInt16 loc
  return res

def UInt32.get (i : UInt32) (self : List.UInt32) : DecodeM UInt32 := do
  let loc ← self.getLocOfIdx i
  let res ← (← read).getUInt32 loc
  return res

def UInt64.get (i : UInt32) (self : List.UInt64) : DecodeM UInt64 := do
  let loc ← self.getLocOfIdx i
  let res ← (← read).getUInt64 loc
  return res

def Pointer.get (i : UInt32) (self : List.Pointer) : DecodeM AnyPointer := do
  let loc ← self.getLocOfIdx i
  let res ← (← read).getUInt64 loc
  return ⟨res⟩

def Pointer.getStruct (i : UInt32) (self : List.Pointer) : DecodeM Struct := do
  let loc ← self.getLocOfIdx i
  let p : AnyPointer := ⟨← (← read).getUInt64 loc⟩
  p.resolveStructPtr loc

def Pointer.getList (i : UInt32) (self : List.Pointer) : DecodeM List := do
  let loc ← self.getLocOfIdx i
  let p : AnyPointer := ⟨← (← read).getUInt64 loc⟩
  p.resolveListPtr loc

/-- Decode a list pointer whose elements are structs. -/
@[inline]
def Struct.get (i : UInt32) (self : List.Struct) : DecodeM Struct := do
  let loc ← self.getLocOfIdx i
  return {
    start := loc
    dataWords := self.elemDataWords
    ptrWords  := self.elemPtrWords
  }

class CanList (α : Type) where
  listTy : Type
  fromList : List → Except ElemSizeError listTy
  toList : listTy → List
  get : UInt32 → listTy → DecodeM α

instance : CanList Bool where
  listTy := List.Bool
  fromList := List.asBool
  toList := List.Bool.toList
  get i l := do List.Bool.get i l

instance : CanList UInt8 where
  listTy := List.UInt8
  fromList := List.asUInt8
  toList := List.UInt8.toList
  get := List.UInt8.get

instance : CanList UInt16 where
  listTy := List.UInt16
  fromList := List.asUInt16
  toList := List.UInt16.toList
  get := List.UInt16.get

instance : CanList UInt32 where
  listTy := List.UInt32
  fromList := List.asUInt32
  toList := List.UInt32.toList
  get := List.UInt32.get

instance : CanList UInt64 where
  listTy := List.UInt64
  fromList := List.asUInt64
  toList := List.UInt64.toList
  get := List.UInt64.get

instance [Struct.IsStruct α] : CanList α where
  listTy := List.Struct
  fromList := List.asStruct
  toList := List.Struct.toList
  get i l := Struct.IsStruct.fromStruct <$> List.Struct.get i l

/-- Helper type so that `List.P α` looks directly usable -/
def P (α : Type) [CanList α] := CanList.listTy α
def P.size [CanList α] (l : P α) : UInt32 := (CanList.toList l).elemCt
def P.get [CanList α] (i : UInt32) (l : P α) : DecodeM α := CanList.get i l

instance [CanList α] : CanList (P α) where
  listTy := List.Pointer
  fromList := List.asPointer
  toList := List.Pointer.toList
  get i l := do
    let l ← List.Pointer.getList i l
    CanList.fromList l

instance [CanList α] : Struct.HasStructAccessor (P α) where
  get s off := do
    let l ← Struct.list s off
    CanList.fromList l

end List

def Data := List.UInt8
instance : Struct.HasStructAccessor Data :=
  inferInstanceAs (Struct.HasStructAccessor (List.P UInt8))
instance : List.CanList Data :=
  inferInstanceAs (List.CanList (List.P UInt8))

def Data.getBytes (d : Data) : DecodeM ByteArray := do
  let start := d.start
  let len := d.elemCt
  let msg := (← read)
  let some seg := msg.segments.get? start.segIdx.val
    | throw (.messageOOB <| Message.OutOfBounds.seg start msg.segments.size)
  if start.idx.val + len.val ≤ seg.data.size then
    let res := seg.data.copySlice start.idx.val (ByteArray.mkEmpty len.val) 0 len.val
    return res
  else
    throw (.messageOOB <| Message.OutOfBounds.idx start len.val seg.data.size)

def Text := Data
instance : Struct.HasStructAccessor Text :=
  inferInstanceAs (Struct.HasStructAccessor Data)
instance : List.CanList Text :=
  inferInstanceAs (List.CanList Data)
def Text.getString (t : Text) : DecodeM String := do
  let bytes ← Data.getBytes t
  let some res := String.fromUTF8? bytes
    | throw .utf8Error
  return res
