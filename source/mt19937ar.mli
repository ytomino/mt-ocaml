type t

external make_int32: int32 -> t = "mlmt_mt19937ar_make_int32"
external make_int32_array: int32 array -> t = "mlmt_mt19937ar_make_int32_array"
val make: int array -> t
val make_self_init: unit -> t

external bits31: t -> int = "mlmt_mt19937ar_bits31"
external bits32: t -> int32 = "mlmt_mt19937ar_bits32"
external float_bits32: t -> float = "mlmt_mt19937ar_float_bits32"
