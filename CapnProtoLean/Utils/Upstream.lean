instance : Repr ByteArray where
  reprPrec arr prec :=
    .group ("ByteArray.mk" ++ .line ++ reprPrec arr.data prec)
