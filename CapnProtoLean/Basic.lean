
namespace CapnProtoLean

def Int8 := UInt8
def Int16 := UInt16
def Int32 := UInt32
def Int64 := UInt64
def Float32 := Float
def Float64 := Float

def AnyPointer : Type := UInt64
deriving Inhabited

def Data := AnyPointer
deriving Inhabited
def Text := AnyPointer
deriving Inhabited

namespace AnyPointer
end AnyPointer

structure Message where
  segments : Array ByteArray

inductive DecodeError

def DecodeM := ReaderT Message (Except DecodeError)
deriving Monad

namespace Message

end Message
