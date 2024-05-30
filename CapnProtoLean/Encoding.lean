import CapnProtoLean.Int
import Batteries

namespace CapnProtoLean

structure Segment where
  data : ByteArray
deriving Inhabited

structure Message where
  segments : Array Segment
deriving Inhabited

structure Segment.Loc where
  /-- Index within a segment, in *bytes* -/
  idx : UInt32

structure Message.Loc extends Segment.Loc where
  /-- Index of segment in message -/
  segIdx : UInt32

structure UnparsedValue where
  msg : Message
  loc : Message.Loc

structure AnyPointer where
  data : UInt64
deriving Inhabited

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

structure Data extends ListPointer where
  h : toListPointer.elemSize = 2 := by first | decide | simp_all

namespace Data
instance : Inhabited Data where
  default := {
    data := 0x200000001
  }
end Data

def Text := Data
deriving Inhabited

inductive DecodeError
| segIdxOutOfRange (idx : UInt32) (ctx : String)
| other (s : String)

namespace DecodeError
instance : ToString DecodeError where
  toString
    | .segIdxOutOfRange idx ctx => s!"Segment index {idx} out of range: {ctx}"
    | .other s => s
end DecodeError

def Segment.Decoder := ReaderT Segment <| ReaderT Segment.Loc <| Except DecodeError
deriving Monad, MonadExcept

def Segment.StructDecoder (α : Type) := (dataWords ptrWords : UInt16) → Segment.Decoder α
def Segment.StructDecoder.run (dataWords ptrWords : UInt16) (d : StructDecoder α) : Decoder α :=
  d dataWords ptrWords

def Message.Decoder := ReaderT Message <| ReaderT Message.Loc <| Except DecodeError
deriving Monad, MonadExcept

def Segment.Decoder.toMessage (d : Segment.Decoder α) : Message.Decoder α :=
  fun msg {segIdx,idx} =>
  if h : segIdx.val < msg.segments.size then
    let seg := msg.segments.uget segIdx.toUSize h
    d seg {idx}
  else
    throw (.segIdxOutOfRange segIdx s!"{segIdx} out of range for message with {msg.segments.size} segments")

namespace Message

instance : MonadLift (Segment.Decoder) (Message.Decoder) where
  monadLift := Segment.Decoder.toMessage

def decode (f : Message.Decoder α) (msg : Message) : Except DecodeError α :=
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

namespace Decoder

/-- Read a `Bool` at the current location. -/
@[inline]
def bool (bit : UInt8) : Segment.Decoder Bool :=
  fun seg loc =>
    if h : loc.idx.val < seg.data.size then
      let byte := seg.data.uget loc.idx.toUSize h
      pure <| (byte >>> bit) &&& 0b1 = 0b1
    else
      throw (.other
        s!"uint8 out of range: {loc.idx} with segment size {seg.data.size}"
      )

/-- Read a `UInt8` at the current location. -/
@[inline]
def uint8 : Segment.Decoder UInt8 :=
  fun seg loc =>
    if h : loc.idx.val < seg.data.size then
      pure (seg.data.uget loc.idx.toUSize h)
    else
      throw (.other
        s!"uint8 out of range: {loc.idx} with segment size {seg.data.size}"
      )

/-- Read a `UInt16` at the current location. -/
@[inline]
def uint16 : Segment.Decoder UInt16 :=
  fun seg loc =>
    if h : loc.idx.val + 1 < seg.data.size then
      pure (seg.data.ugetUInt16LE loc.idx.toUSize h)
    else
      throw (.other
        s!"uint16 out of range: {loc.idx} with segment size {seg.data.size}"
      )

/-- Read a `UInt32` at the current location. -/
@[inline]
def uint32 : Segment.Decoder UInt32 :=
  fun seg loc =>
    if h : loc.idx.val + 3 < seg.data.size then
      pure (seg.data.ugetUInt32LE loc.idx.toUSize h)
    else
      throw (.other
        s!"uint32 out of range: {loc.idx} with segment size {seg.data.size}"
      )

/-- Read a `UInt64` at the current location. -/
@[inline]
def uint64 : Segment.Decoder UInt64 :=
  fun seg loc =>
    if h : loc.idx.val + 7 < seg.data.size then
      pure (seg.data.ugetUInt64LE loc.idx.toUSize h)
    else
      throw (.other
        s!"uint64 out of range: {loc.idx} with segment size {seg.data.size}"
      )

/-- Read a `Int8` at the current location. -/
@[inline]
def int8 : Segment.Decoder Int8 := do
  return (← uint8)

/-- Read a `Int16` at the current location. -/
@[inline]
def int16 : Segment.Decoder Int16 := do
  return (← uint16)

/-- Read a `Int32` at the current location. -/
@[inline]
def int32 : Segment.Decoder Int32 := do
  return (← uint32)

/-- Read a `Int64` at the current location. -/
@[inline]
def int64 : Segment.Decoder Int64 := do
  return (← uint64)

/-- Read a `Float32` at the current location. -/
@[inline]
def float32 : Segment.Decoder Float32 := do
  return (← uint32)

/-- Read a `Float64` at the current location. -/
@[inline]
def float64 : Segment.Decoder Float64 := do
  return Float64.ofUInt64 (← uint64)

@[inline]
def anyPointer : Segment.Decoder AnyPointer := do
  return ⟨← uint64⟩

@[inline]
def unparsedValue : Message.Decoder UnparsedValue :=
  fun msg loc => pure {msg, loc}

/-- Move by `x` bytes from current location. -/
@[inline]
def moveOffBytes (x : Int32) (d : Segment.Decoder α) : Segment.Decoder α :=
  fun seg loc => d seg {idx := loc.idx + x}

/-- Move by `x` words from current location. -/
@[inline]
def moveOffWords (x : Int32) (d : Segment.Decoder α) : Segment.Decoder α :=
  fun seg loc => d seg {idx := loc.idx + 8 * (show UInt32 from x)}

/-- Move to a different location (potentially another segment). -/
@[inline]
def moveLoc (loc : Message.Loc) (d : Message.Decoder α) : Message.Decoder α :=
  fun msg _ => d msg loc

/--
Follow a struct pointer.
-/
@[inline]
def structPtr (d : Segment.StructDecoder α)
      : Message.Decoder (Option α) := do
  let p : AnyPointer ← anyPointer
  if p.isNull then
    return none

  if h : p.isStruct then
    return ← handleSegPointer {p with}

  else if h : p.isFar then
    let p : FarPointer := {p with}
    moveLoc p.toLoc do
      let lp : AnyPointer ← anyPointer
      if !p.landingPadIsFar then
        if lp.isNull then
          return none

        if h : lp.isStruct then
          return ← handleSegPointer {lp with}
        else
          throw (.other s!"Expected landing pad struct pointer, got {lp.data.toNat.toDigits 16}")
      else
        if h : lp.isFar then
          let lp : FarPointer := {lp with}
          let tag : AnyPointer ← moveOffWords (1 : UInt32) anyPointer
          if h : tag.isStruct then
            let tag : StructPointer := {tag with}
            moveLoc lp.toLoc do
              d.run tag.dataSize tag.pointerSize
          else
            throw (.other s!"Expected tag struct pointer, got {tag.data.toNat.toDigits 16}")
        else
          throw (.other s!"Expected landing pad far struct pointer, got {lp.data.toNat.toDigits 16}")
  else
    throw (.other s!"Expected struct or far pointer, got {p.data.toNat.toDigits 16}")

where
  handleSegPointer (p : StructPointer) := do
    return some (← moveOffWords p.offset <| d.run p.dataSize p.pointerSize)

/--
Follow a list pointer. Provides elemSize tag and listSize in bytes.
-/
@[inline]
private def listPtr (d : UInt8 → UInt32 → Segment.Decoder (Array α)) : Message.Decoder (Array α) := do
  let p : AnyPointer ← anyPointer
  if p.isNull then
    return #[]

  if h : p.isList then
    let p : ListPointer := {p with}
    d p.elemSize p.size

  else if h : p.isFar then
    let p : FarPointer := {p with}
    moveLoc p.toLoc do
      let lp : AnyPointer ← anyPointer
      if !p.landingPadIsFar then
        if lp.isNull then
          return #[]

        if h : lp.isList then
          let lp : ListPointer := {lp with}
          d lp.elemSize lp.size
        else
          throw (.other s!"Expected landing pad list pointer, got {lp.data.toNat.toDigits 16}")
      else
        if h : lp.isFar then
          let lp : FarPointer := {lp with}
          let tag : AnyPointer ← moveOffWords (1 : UInt32) <| (do return ⟨← uint64⟩)
          if h : tag.isList then
            let tag : ListPointer := {tag with}
            moveLoc lp.toLoc do
              d tag.elemSize tag.size
          else
            throw (.other s!"Expected tag list pointer, got {tag.data.toNat.toDigits 16}")
        else
          throw (.other s!"Expected landing pad far list pointer, got {lp.data.toNat.toDigits 16}")
  else
    throw (.other s!"list struct or far pointer, got {p.data.toNat.toDigits 16}")



/-- Decode a list of booleans. -/
def listBool : Message.Decoder (Array Bool) := do
  listPtr (fun elemSize size => do
    if elemSize ≠ 1 then
      throw (.other s!"List expected element size 1 (1 bit) but got {elemSize}")

    let mut res : Array Bool := Array.mkEmpty size.val
    for i in [0:size.val] do
      let byte ← moveOffBytes (Int32.ofUInt32 (i / 8).toUInt32) <| uint8
      let bool := (byte >>> (i % 8).toUInt8) &&& 0b1 = 0b1
      res := res.push bool

    return res
  )


/-- Decode a list pointer whose elements are primitive/not structs.

`elemSize` values are the same as for `ListPointer.elemSize`:
    0 = 0 (e.g. List(Void))
    1 = 1 bit
    2 = 1 byte
    3 = 2 bytes
    4 = 4 bytes
    5 = 8 bytes (non-pointer)
    6 = 8 bytes (pointer)
    7 = composite
-/
def listPtrPrim (elemSize : UInt8) (d : Segment.Decoder α) : Message.Decoder (Array α) := do
  let byteWidth ← match elemSize with
    | 2 => pure (1 : UInt32)
    | 3 => pure 2
    | 4 => pure 4
    | 5 => pure 8
    | 6 => pure 8
    | _ => throw (.other s!"List primitive decoder with elemSize = {elemSize}?")
  listPtr (fun elemSize' size => do
    if elemSize' ≠ elemSize then
      throw (.other s!"List expected element size {elemSize} but got {elemSize'}")

    let mut res : Array α := Array.mkEmpty size.val
    for i in [0:size.val] do
      let a ← moveOffBytes (byteWidth * i.toUInt32 :) d
      res := res.push a

    return res
  )

/-- Decode a list pointer whose elements are structs. -/
@[inline]
def listPtrStruct (d : Segment.StructDecoder α) : Message.Decoder (Array α) := do
  listPtr (fun elemSize size => do
    if elemSize = 7 then
      -- Composite: the first word of the object data is a tag
      let tag ← anyPointer
      if h : tag.isStruct then
        let tag : StructPointer := {tag with}
        let numElems : UInt32 := tag.offset
        let dataWords := tag.dataSize
        let ptrWords := tag.pointerSize
        if (dataWords.toUInt32 + ptrWords.toUInt32) * numElems ≠ size then
          throw (.other s!"list ptr struct math not working out: {dataWords}, {ptrWords}, {numElems}, {size}")
        moveOffWords (1 : UInt32) do
        let mut res : Array α := Array.mkEmpty numElems.val
        for i in [0:numElems.val] do
          let a ← moveOffWords ((dataWords.toUInt32 + ptrWords.toUInt32) * i.toUInt32)
                  <| d.run dataWords ptrWords
          res := res.push a
        return res
      else
        throw (.other s!"Composite list pointer -- expected struct tag word, got {tag.data.toNat.toDigits 16}")
    else
      -- elemSize is allowed to be 2 (1 byte) thru 6 (8 bytes)
      -- and interpreted as a prefix of the struct
      throw (.other s!"Not supported: list with elem size tag {elemSize} interpreted as struct")
  )


end Decoder
