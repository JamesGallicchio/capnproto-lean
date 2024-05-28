
def Int8 := UInt8
def Int8.ofUInt8 : UInt8 → Int8 := id

def ByteArray.ugetUInt8LE := ByteArray.uget

def Int16 := UInt16
def Int16.ofUInt16 : UInt16 → Int16 := id

def ByteArray.ugetUInt16LE (A : ByteArray) (i : USize)
    (harray : i.val + 1 < A.size) : UInt16 :=
  Id.run do
  let mut res : UInt16 := 0
  for h : j in [0:2] do
    have : j < 2 := h.upper
    let byte := A.uget
      (USize.ofNat (i.val+j))
      (by simp [USize.ofNat, USize.toNat]
          rw [Nat.mod_eq]
          simp; split
          · apply Nat.lt_of_lt_of_le; apply Nat.mod_lt; simp
            omega
          · omega)
    res := res ||| byte.toUInt16 <<< ⟨8*j, by unfold UInt16.size; omega⟩
  return res

def Int32 := UInt32
def Int32.ofUInt32 : UInt32 → Int32 := id

def UInt32.shiftArithRight (x y : UInt32) : UInt32 :=
  (x >>> y) ||| (0 - ((x &&& 0x80000000) >>> y))

def ByteArray.ugetUInt32LE (A : ByteArray) (i : USize)
    (harray : i.val + 3 < A.size) : UInt32 :=
  Id.run do
  let mut res : UInt32 := 0
  for h : j in [0:4] do
    have : j < 4 := h.upper
    let byte := A.uget
      (USize.ofNat (i.val+j))
      (by simp [USize.ofNat, USize.toNat]
          rw [Nat.mod_eq]
          simp; split
          · apply Nat.lt_of_lt_of_le; apply Nat.mod_lt; simp
            omega
          · omega)
    res := res ||| byte.toUInt32 <<< ⟨8*j, by unfold UInt32.size; omega⟩
  return res

def UInt64.shiftArithRight (x y : UInt64) : UInt64 :=
  (x >>> y) ||| (0 - ((x &&& 0x8000000000000000) >>> y))

def ByteArray.ugetUInt64LE (A : ByteArray) (i : USize)
    (harray : i.val + 7 < A.size) : UInt64 :=
  Id.run do
  let mut res : UInt64 := 0
  for h : j in [0:8] do
    have : j < 8 := h.upper
    let byte := A.uget
      (USize.ofNat (i.val+j))
      (by simp [USize.ofNat, USize.toNat]
          rw [Nat.mod_eq]
          simp; split
          · apply Nat.lt_of_lt_of_le; apply Nat.mod_lt; simp
            omega
          · omega)
    res := res ||| byte.toUInt64 <<< ⟨8*j, by unfold UInt64.size; omega⟩
  return res

def Int64 := UInt64
def Int64.ofUInt64 : UInt64 → Int64 := id

def Float32 := UInt32
def Float32.ofUInt32 : UInt32 → Float32 := id

def Float64 := Float
deriving Inhabited

def Float64.ofUInt64 (x : UInt64) : Float64 :=
  unsafe (unsafeCast x)
