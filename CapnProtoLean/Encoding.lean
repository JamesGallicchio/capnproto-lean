import CapnProtoLean.Int
import Batteries

namespace CapnProtoLean

structure AnyPointer where
  data : UInt64
deriving Inhabited

namespace AnyPointer

def isStruct (p : AnyPointer) : Bool :=
  p.data &&& 0x3 = 0
def isList (p : AnyPointer) : Bool :=
  p.data &&& 0x3 = 1
def isFar (p : AnyPointer) : Bool :=
  p.data &&& 0x3 = 2
def isOther (p : AnyPointer) : Bool :=
  p.data &&& 0x3 = 3

end AnyPointer

structure StructPointer where
  p : AnyPointer
  isStruct : p.isStruct

namespace StructPointer
instance : Inhabited StructPointer := ⟨⟨⟨0⟩, by decide⟩⟩

/-- Offset, in words, from the end of the pointer to the
    start of the struct's data section. Signed. -/
def offset (s : StructPointer) : Int32 :=
  -- Get lower 32 bits
  let low : UInt32 := s.p.data.toUInt32
  -- Sign-extend by two bits
  let shifted : UInt32 := low.shiftArithRight 2
  shifted

/-- Size of the struct's data section, in words. -/
def dataSize (s : StructPointer) : UInt16 :=
  (s.p.data >>> 32).toUInt16

/-- Size of the struct's pointer section, in words. -/
def pointerSize (s : StructPointer) : UInt16 :=
  (s.p.data >>> 48).toUInt16

end StructPointer

structure ListPointer where
  p : AnyPointer
  isList : p.isList

namespace ListPointer
instance : Inhabited ListPointer := ⟨⟨⟨1⟩, by decide⟩⟩

/-- Offset, in words, from the end of the pointer to the
    start of the first element of the list.  Signed. -/
def offset (s : ListPointer) : Int32 :=
  -- Get lower 32 bits
  let low : UInt32 := s.p.data.toUInt32
  -- Sign-extend by two bits
  let shifted : UInt32 := low.shiftArithRight 2
  shifted

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
  (s.p.data >>> 32).toUInt8 &&& 0b111

/--
Size of the list:
    when C <> 7: Number of elements in the list.
    when C = 7: Number of words in the list, not counting the tag word
    (see below).
-/
def size (s : ListPointer) : UInt32 :=
  (s.p.data >>> 35).toUInt32

end ListPointer

structure FarPointer where
  p : AnyPointer
  isFar : p.isFar

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
  f.p.data &&& 0b100 != 0

/--
Offset, in words, from the start of the target segment
    to the location of the far-pointer landing-pad within that
    segment.  Unsigned.
-/
def offset (f : FarPointer) : UInt32 :=
  (f.p.data >>> 3).toUInt32

/--
ID of the target segment.
(Segments are numbered sequentially starting from zero.)
-/
def segmentId (f : FarPointer) : UInt32 :=
  (f.p.data >>> 32).toUInt32

end FarPointer

structure OtherPointer where
  p : AnyPointer
  isOther : p.isOther

namespace OtherPointer
instance : Inhabited OtherPointer := ⟨⟨⟨3⟩, by decide⟩⟩
end OtherPointer

structure Data where
  p : ListPointer
  h : p.elemSize = 2
namespace Data
instance : Inhabited Data := ⟨⟨⟨⟨0x200000001⟩, by decide⟩, by decide⟩⟩
end Data

def Text := Data
deriving Inhabited

structure Segment where
  data : ByteArray
deriving Inhabited

structure Message where
  segments : Array Segment
deriving Inhabited

structure Segment.Loc where
  /-- Index within a segment, in *bytes* -/
  idx : Nat

structure Message.Loc extends Segment.Loc where
  /-- Index of segment in message -/
  segIdx : Nat

inductive Message.DecodeError
| segIdxOutOfRange (idx : Nat) (ctx : String)
| other (s : String)

namespace Message.DecodeError
instance : ToString Message.DecodeError where
  toString
    | .segIdxOutOfRange idx ctx => s!"Segment index {idx} out of range: {ctx}"
    | .other s => s
end Message.DecodeError

inductive Segment.DecodeError
| msg (m : Message.DecodeError)
| farPointer (fp : FarPointer)

def Segment.Decoder := ReaderT Segment <| ReaderT Segment.Loc <| Except Segment.DecodeError
deriving Monad

def Message.Decoder := ReaderT Message <| ReaderT Message.Loc <| Except DecodeError
deriving Monad

def Segment.Decoder.toMessage (d : Segment.Decoder α) : Message.Decoder α :=
  fun msg {segIdx,idx} =>
  if h : segIdx < msg.segments.size then
    let seg := msg.segments[segIdx]
    match d seg {idx} with
    | .ok a => .ok a
    | .error (.msg m) => throw m
    | .error (.farPointer fp) =>
      if fp.landingPadIsFar then
        throw (.other "far pointer with far landing pad not yet supported")
      else
        throw (.other "far pointers not yet supported")
  else
    throw (.segIdxOutOfRange segIdx s!"{segIdx} out of range for array of size {msg.segments.size}")

namespace Message

instance : MonadLift (Segment.Decoder) (Message.Decoder) where
  monadLift := Segment.Decoder.toMessage

def decode (f : Message.Decoder α) (msg : Message) : Except Message.DecodeError α :=
  f msg {segIdx := 0, idx := 0}

partial def fromHandle (h : IO.FS.Handle) : IO Message := do
  -- Number of segments is the first UInt32 of the stream, plus 1
  let numSegs := (← readUInt32) + 1
  let mut sizeSegs := #[]
  for _ in [0:numSegs.val] do
    sizeSegs := sizeSegs.push (← readUInt32)
  if (4 + numSegs*4) % 8 = 4 then
    let _ ← readUInt32
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

end Message

namespace Segment

/--
Get the pointer field located `offset` bytes into the struct pointed to by `s`,
under the assumption that `s` itself was located at `ptrIdx` in `seg`.
-/
def getStructPtr (ptrIdx : UInt32) (s : StructPointer) (offset : UInt32)
  (seg : Segment) : AnyPointer :=
  let idx := ptrIdx + (show UInt32 from s.offset) + s.dataSize.toUInt32 + offset
  if h : idx.val + 7 < seg.data.size then
    ⟨seg.data.ugetUInt64LE idx.toUSize (by simp_all)⟩
  else default

/--
Get the `Bool` field located `offset` bytes and `offb` bits into the data section
of the struct pointed to by `s`,
assuming that `s` was itself located at `ptrIdx` in `seg`.
-/
def getStructBool (ptrIdx : UInt32) (s : StructPointer) (offset : UInt32) (offb : UInt8)
  (seg : Segment) : Bool :=
  let idx := ptrIdx + (show UInt32 from s.offset) + offset
  if h : idx.toNat < seg.data.size then
    let byte := seg.data.uget idx.toUSize (by simp [h])
    (byte >>> offb) &&& 0b1 = 0b1
  else
    default

/--
Get the `UInt8` field located `offset` bytes into the data section
of the struct pointed to by `s`,
assuming that `s` was itself located at `ptrIdx` in `seg`.
-/
def getStructUInt8 (ptrIdx : UInt32) (s : StructPointer) (offset : UInt32)
  (seg : Segment) : UInt8 :=
  let idx := ptrIdx + (show UInt32 from s.offset) + offset
  if h : idx.toNat < seg.data.size then
    seg.data.uget idx.toUSize (by simp [h])
  else
    default

/--
Get the `UInt16` field located `offset` bytes into the data section
of the struct pointed to by `s`,
assuming that `s` was itself located at `ptrIdx` in `seg`.
-/
def getStructUInt16 (ptrIdx : UInt32) (s : StructPointer) (offset : UInt32)
  (seg : Segment) : UInt16 :=
  let idx := ptrIdx + (show UInt32 from s.offset) + offset
  if h : idx.toNat+1 < seg.data.size then
    seg.data.ugetUInt16LE idx.toUSize (by simp [h])
  else default

/--
Get the `UInt32` field located `offset` bytes into the data section
of the struct pointed to by `s`,
assuming that `s` was itself located at `ptrIdx` in `seg`.
-/
def getStructUInt32 (ptrIdx : UInt32) (s : StructPointer) (offset : UInt32)
  (seg : Segment) : UInt32 :=
  let idx := ptrIdx + (show UInt32 from s.offset) + offset
  if h : idx.toNat+3 < seg.data.size then
    seg.data.ugetUInt32LE idx.toUSize (by simp [h])
  else default

/--
Get the `UInt64` field located `offset` bytes into the data section
of the struct pointed to by `s`,
assuming that `s` was itself located at `ptrIdx` in `seg`.
-/
def getStructUInt64 (ptrIdx : UInt32) (s : StructPointer) (offset : UInt32)
  (seg : Segment) : UInt64 :=
  let idx := ptrIdx + (show UInt32 from s.offset) + offset
  if h : idx.toNat+7 < seg.data.size then
    seg.data.ugetUInt64LE idx.toUSize (by simp [h])
  else default

/--
Get the `Int8` field located `offset` bytes into the data section
of the struct pointed to by `s`,
assuming that `s` was itself located at `ptrIdx` in `seg`.
-/
def getStructInt8 (ptrIdx : UInt32) (s : StructPointer) (offset : UInt32)
  (seg : Segment) : Int8 :=
  getStructUInt8 ptrIdx s offset seg
  |> Int8.ofUInt8

/--
Get the `Int16` field located `offset` bytes into the data section
of the struct pointed to by `s`,
assuming that `s` was itself located at `ptrIdx` in `seg`.
-/
def getStructInt16 (ptrIdx : UInt32) (s : StructPointer) (offset : UInt32)
  (seg : Segment) : Int16 :=
  getStructUInt16 ptrIdx s offset seg
  |> Int16.ofUInt16

/--
Get the `Int32` field located `offset` bytes into the data section
of the struct pointed to by `s`,
assuming that `s` was itself located at `ptrIdx` in `seg`.
-/
def getStructInt32 (ptrIdx : UInt32) (s : StructPointer) (offset : UInt32)
  (seg : Segment) : Int32 :=
  getStructUInt32 ptrIdx s offset seg
  |> Int32.ofUInt32

/--
Get the `Int64` field located `offset` bytes into the data section
of the struct pointed to by `s`,
assuming that `s` was itself located at `ptrIdx` in `seg`.
-/
def getStructInt64 (ptrIdx : UInt32) (s : StructPointer) (offset : UInt32)
  (seg : Segment) : Int64 :=
  getStructUInt64 ptrIdx s offset seg
  |> Int64.ofUInt64

/--
Get the `Float32` field located `offset` bytes into the data section
of the struct pointed to by `s`,
assuming that `s` was itself located at `ptrIdx` in `seg`.
-/
def getStructFloat32 (ptrIdx : UInt32) (s : StructPointer) (offset : UInt32)
  (seg : Segment) : Float32 :=
  getStructUInt32 ptrIdx s offset seg
  |> Float32.ofUInt32

/--
Get the `Float64` field located `offset` bytes into the data section
of the struct pointed to by `s`,
assuming that `s` was itself located at `ptrIdx` in `seg`.
-/
def getStructFloat64 (ptrIdx : UInt32) (s : StructPointer) (offset : UInt32)
  (seg : Segment) : Float64 :=
  getStructUInt64 ptrIdx s offset seg
  |> Float64.ofUInt64

end Segment
