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

structure Segment.Loc where
  /-- Index within a segment, in *bytes* -/
  idx : UInt32

structure Message.Loc extends Segment.Loc where
  /-- Index of segment in message -/
  segIdx : UInt32

instance : ToString Message.Loc where
  toString := fun {segIdx, idx} =>
    s!"{segIdx}:{idx}"

structure UnparsedValue where
  msg : Message
  loc : Message.Loc

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

def DecodeCtx := List (String × Message.Loc)

structure DecodeError where
  msg : Message
  context : DecodeCtx
  error : String
  loc : Message.Loc

namespace DecodeError
instance : ToString DecodeError where
  toString err :=
    match err with
    | {msg:=_, context, error, loc} =>
      s!"{loc} {error}\n" ++ (
        context.map (fun (name, loc) => s!"{loc} {name}\n")
        |> String.join)
end DecodeError

def Decoder := ReaderT Message <| ReaderT Message.Loc <| ReaderT DecodeCtx <| Except DecodeError
deriving Monad, MonadExcept

def StructDecoder (α : Type) := (dataWords ptrWords : UInt16) → Decoder α
def StructDecoder.run (dataWords ptrWords : UInt16) (d : StructDecoder α) : Decoder α :=
  d dataWords ptrWords


namespace Message

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

end Message

namespace Decoder

/-- Report an error, with context -/
def error (s : String) : Decoder α :=
  fun msg loc context => do
  throw {
    msg, context, loc, error := s
  }

@[inline]
def context (name : String) (d : Decoder α) : Decoder α :=
  fun msg loc context => d msg loc ((name, loc) :: context)

@[inline]
def getMsg : Decoder Message := fun msg _ _ => pure msg

@[inline]
def getLoc : Decoder Message.Loc := fun _ loc _ => pure loc

@[inline]
def getSeg : Decoder Segment := do
  let msg ← getMsg
  let loc ← getLoc
  if h : _ then
    return msg.segments.uget loc.segIdx.toUSize h
  else
    error s!"segment idx out of range (message has {msg.segments.size} segments)"

/-- Read a `Bool` at the current location. -/
@[inline]
def bool (bit : UInt8) : Decoder Bool :=
  context "bool" <| do
    let seg ← getSeg
    let loc ← getLoc
    if h : _ then
      let byte := seg.data.uget loc.idx.toUSize h
      pure <| (byte >>> bit) &&& 0b1 = 0b1
    else
      error s!"idx out of range: segment {loc.segIdx} has {seg.data.size} bytes"

/-- Read a `UInt8` at the current location. -/
@[inline]
def uint8 : Decoder UInt8 :=
  context "uint8" <| do
    let seg ← getSeg
    let loc ← getLoc
    if h : _ then
      pure (seg.data.uget loc.idx.toUSize h)
    else
      error s!"idx out of range: segment {loc.segIdx} has {seg.data.size} bytes"

/-- Read a `UInt16` at the current location. -/
@[inline]
def uint16 : Decoder UInt16 :=
  context "uint16" <| do
    let seg ← getSeg
    let loc ← getLoc
    if h : _ then
      pure (seg.data.ugetUInt16LE loc.idx.toUSize h)
    else
      error s!"idx out of range: segment {loc.segIdx} has {seg.data.size} bytes"

/-- Read a `UInt32` at the current location. -/
@[inline]
def uint32 : Decoder UInt32 :=
  context "uint32" <| do
    let seg ← getSeg
    let loc ← getLoc
    if h : _ then
      pure (seg.data.ugetUInt32LE loc.idx.toUSize h)
    else
      error s!"idx out of range: segment {loc.segIdx} has {seg.data.size} bytes"

/-- Read a `UInt64` at the current location. -/
@[inline]
def uint64 : Decoder UInt64 :=
  context "uint64" <| do
    let seg ← getSeg
    let loc ← getLoc
    if h : _ then
      pure (seg.data.ugetUInt64LE loc.idx.toUSize h)
    else
      error s!"idx out of range: segment {loc.segIdx} has {seg.data.size} bytes"

/-- Read a `Int8` at the current location. -/
@[inline]
def int8 : Decoder Int8 := do
  context "int8" <| return (← uint8)

/-- Read a `Int16` at the current location. -/
@[inline]
def int16 : Decoder Int16 := do
  context "int16" <| return (← uint16)

/-- Read a `Int32` at the current location. -/
@[inline]
def int32 : Decoder Int32 := do
  context "int32" <| return (← uint32)

/-- Read a `Int64` at the current location. -/
@[inline]
def int64 : Decoder Int64 := do
  context "int64" <| return (← uint64)

/-- Read a `Float32` at the current location. -/
@[inline]
def float32 : Decoder Float32 := do
  context "float32" <| return (← uint32)

/-- Read a `Float64` at the current location. -/
@[inline]
def float64 : Decoder Float64 := do
  context "float64" <| return Float64.ofUInt64 (← uint64)

@[inline]
def anyPointer : Decoder AnyPointer := do
  context "anyPointer" <| return ⟨← uint64⟩

@[inline]
def unparsedValue : Decoder UnparsedValue := do
  context "unparsedValue" <| return {msg := ← getMsg, loc := ← getLoc}

/-- Move by `x` bytes within current segment. -/
@[inline]
def moveOffBytes (x : Int32) (d : Decoder α) : Decoder α :=
  fun msg loc ctx => d msg {loc with idx := loc.idx + x} ctx

/-- Move by `x` words from current location. -/
@[inline]
def moveOffWords (x : Int32) (d : Decoder α) : Decoder α :=
  fun msg loc ctx =>
    d msg {loc with idx := loc.idx + 8 * (show UInt32 from x)} ctx

/-- Move to a different location (potentially another segment). -/
@[inline]
def moveLoc (loc : Message.Loc) (d : Decoder α) : Decoder α :=
  fun msg _ ctx => d msg loc ctx

/--
Follow a struct pointer.
-/
@[inline]
def structPtr (d : StructDecoder α) : Decoder (Option α) :=
  context "structPtr" <| do
  let p : AnyPointer ← anyPointer
  context s!"{p}" <| do
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
          error s!"Expected landing pad struct pointer, got {lp}"
      else
        if h : lp.isFar then
          let lp : FarPointer := {lp with}
          let tag : AnyPointer ← moveOffWords (1 : UInt32) anyPointer
          if h : tag.isStruct then
            let tag : StructPointer := {tag with}
            moveLoc lp.toLoc do
              return some <| ← d.run tag.dataSize tag.pointerSize
          else
            error s!"Expected tag struct pointer, got {tag}"
        else
          error s!"Expected landing pad far struct pointer, got {lp}"
  else
    error s!"Expected struct or far pointer, got {p}"

where
  handleSegPointer (p : StructPointer) := do
    return some (← moveOffWords (p.offset + 1) <| d.run p.dataSize p.pointerSize)

/--
Follow a list pointer. Provides elemSize tag and listSize in bytes.
-/
@[inline]
private def listPtr (d : UInt8 → UInt32 → Decoder α) : Decoder (Option α) := do
  let p : AnyPointer ← anyPointer
  context s!"{p}" <| do
  if p.isNull then
    return none

  if h : p.isList then
    let p : ListPointer := {p with}
    moveOffWords (p.offset + 1) <|
      d p.elemSize p.size

  else if h : p.isFar then
    let p : FarPointer := {p with}
    moveLoc p.toLoc do
      let lp : AnyPointer ← anyPointer
      context s!"{lp}" <| do
      if !p.landingPadIsFar then
        if lp.isNull then
          return none

        if h : lp.isList then
          let lp : ListPointer := {lp with}
          d lp.elemSize lp.size
        else
          error s!"Expected landing pad list pointer, got {lp}"
      else
        if h : lp.isFar then
          let lp : FarPointer := {lp with}
          let tag : AnyPointer ← moveOffWords (1 : UInt32) <| (do return ⟨← uint64⟩)
          if h : tag.isList then
            let tag : ListPointer := {tag with}
            moveLoc lp.toLoc do
              d tag.elemSize tag.size
          else
            error s!"Expected tag list pointer, got {tag}"
        else
          error s!"Expected landing pad far list pointer, got {lp}"
  else
    error s!"list struct or far pointer, got {p}"


/-- Decode a list of booleans. -/
def listBool : Decoder (Array Bool) :=
  context "listBool" <| do
  let res ← listPtr (fun elemSize size => do
    if elemSize ≠ 1 then
      error s!"List expected element size 1 (1 bit) but got {elemSize}"

    let mut res : Array Bool := Array.mkEmpty size.val
    for i in [0:size.val] do
      let byte ← moveOffBytes (Int32.ofUInt32 (i / 8).toUInt32) <| uint8
      let bool := (byte >>> (i % 8).toUInt8) &&& 0b1 = 0b1
      res := res.push bool

    return res
  )
  return res.getD #[]


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
def listPtrPrim (elemSize : UInt8) (d : Decoder α) : Decoder (Array α) :=
  context "listPtrPrim" <| do
  let byteWidth ← match elemSize with
    | 2 => pure (1 : UInt32)
    | 3 => pure 2
    | 4 => pure 4
    | 5 => pure 8
    | 6 => pure 8
    | _ => error s!"List primitive decoder with elemSize = {elemSize}?"
  let res ← listPtr (fun elemSize' size => do
    if elemSize' ≠ elemSize then
      error s!"List expected element size {elemSize} but got {elemSize'}"

    let mut res : Array α := Array.mkEmpty size.val
    for i in [0:size.val] do
      let a ← moveOffBytes (byteWidth * i.toUInt32 :) d
      res := res.push a

    return res
  )
  return res.getD #[]

/-- Decode a list pointer whose elements are structs. -/
@[inline]
def listPtrStruct (d : StructDecoder α) : Decoder (Array α) :=
  context "listPtrStruct" <| do
  let res ← listPtr (fun elemSize size =>
    context "listPtr continuation" <| do
    if elemSize = 7 then
      -- Composite: the first word of the object data is a tag
      let tag ← anyPointer
      if h : tag.isStruct then
        let tag : StructPointer := {tag with}
        let numElems : UInt32 := tag.offset
        let dataWords := tag.dataSize
        let ptrWords := tag.pointerSize
        if (dataWords.toUInt32 + ptrWords.toUInt32) * numElems ≠ size then
          error s!"list ptr struct math not working out: {dataWords}, {ptrWords}, {numElems}, {size}"
        moveOffWords (1 : UInt32) do
        let mut res : Array α := Array.mkEmpty numElems.val
        for i in [0:numElems.val] do
          let a ← moveOffWords ((dataWords.toUInt32 + ptrWords.toUInt32) * i.toUInt32)
                  <| d.run dataWords ptrWords
          res := res.push a
        return res
      else
        error s!"Composite list pointer -- expected struct tag word, got {tag}"
    else
      -- elemSize is allowed to be 2 (1 byte) thru 6 (8 bytes)
      -- and interpreted as a prefix of the struct
      error s!"Not supported: list with elem size tag {elemSize} interpreted as struct"
  )
  return res.getD #[]

def data : Decoder ByteArray := do
  let res ← listPtr (fun elemSize elemCount => do
    if elemSize ≠ 2 then
      error s!"data pointer has elemSize {elemSize}"
    let seg ← getSeg
    let loc ← getLoc
    return seg.data.copySlice
      (srcOff := loc.idx.toNat)
      (dest := ByteArray.empty) (destOff := 0)
      (len := elemCount.toNat)
  )
  return res.getD (ByteArray.empty)

def text : Decoder String := do
  let bytes ← data
  match String.fromUTF8? bytes with
  | none => error "expected text, but bytes are not valid UTF8"
  | some s => pure s

end Decoder

def decode (f : StructDecoder α) (msg : Message) : Except DecodeError (Option α) :=
  (Decoder.structPtr f) msg {segIdx := 0, idx := 0} []
