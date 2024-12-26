val int: bits32:('a -> int32) -> bits64:('a -> int64) -> 'a -> int -> int
val int32: bits32:('a -> int32) -> 'a -> int32 -> int32
val int64: bits32:('a -> int32) -> bits64:('a -> int64) -> 'a -> int64 -> int64

val bind_int: bits32:('a -> int32) -> bits64:('a -> int64) -> int -> 'a ->
	unit -> int
val bind_int32: bits32:('a -> int32) -> int32 -> 'a -> unit -> int32
val bind_int64: bits32:('a -> int32) -> bits64:('a -> int64) -> int64 -> 'a ->
	unit -> int64

val int_from_int64_bits: width:int -> bits:('a -> int64) -> 'a -> int -> int
val int32_from_int64_bits: width:int -> bits:('a -> int64) -> 'a -> int32 ->
	int32
val int64_from_int64_bits: width:int -> bits:('a -> int64) -> 'a -> int64 ->
	int64
val float_from_int64_bits: width:int -> bits:('a -> int64) -> 'a -> float ->
	float (* [0,bound) *)
val float_exclusive_from_int64_bits: width:int -> bits:('a -> int64) -> 'a ->
	float -> float (* (0,bound) *)
val float_inclusive_from_int64_bits: width:int -> bits:('a -> int64) -> 'a ->
	float -> float (* [0,bound] *)

val bind_int_from_int64_bits: width:int -> bits:('a -> int64) -> int -> 'a ->
	unit -> int
val bind_int32_from_int64_bits: width:int -> bits:('a -> int64) -> int32 ->
	'a -> unit -> int32
val bind_int64_from_int64_bits: width:int -> bits:('a -> int64) -> int64 ->
	'a -> unit -> int64
