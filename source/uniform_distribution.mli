(** Converting from the generated bit sequence to the ranged number with a
    uniform distribution. *)

(** {1 In 32 bit units} *)

(** {2 Normal functions} *)

val int: bits32:('a -> int32) -> bits64:('a -> int64) -> 'a -> int -> int
(** \[0,[bound]).
    Draws out values repeatedly as needed by [bits32] if the [bound] fits into
    32 bits otherwise [bits64]. *)

val int32: bits32:('a -> int32) -> 'a -> int32 -> int32
val int64: bits32:('a -> int32) -> bits64:('a -> int64) -> 'a -> int64 -> int64

(** {2 Pre-bindable functions}
    For multiple times to call these functions with the same parameters, it can
    make more efficient functions by partial applying the parameters to do part
    of the needed calculation before the calls.
    For example: {[
	let f = bind_int ~bits32 ~bits64 bound in
	List.init times (fun _ -> f state ())]}
    is better than: {[
	List.init times (fun _ -> int ~bits32 ~bits64 state bound)]} *)

val bind_int: bits32:('a -> int32) -> bits64:('a -> int64) -> int -> 'a ->
	unit -> int
(** Like {!int} except the order of the parameters differs for efficiency of
    partial application. *)

val bind_int32: bits32:('a -> int32) -> int32 -> 'a -> unit -> int32
val bind_int64: bits32:('a -> int32) -> bits64:('a -> int64) -> int64 -> 'a ->
	unit -> int64

(** {1 In any bit units.} *)

(** {2 Normal functions} *)

val int_from_int64_bits: width:int -> bits:('a -> int64) -> 'a -> int -> int
(** \[0,[bound]).
    Draws out values repeatedly as needed by [bits] in [width] bit units.
    [bound] must be fits into [width] bits, and [bits] must return a value fits
    into [width] bits. *)

val int32_from_int64_bits: width:int -> bits:('a -> int64) -> 'a -> int32 ->
	int32
val int64_from_int64_bits: width:int -> bits:('a -> int64) -> 'a -> int64 ->
	int64

val float_from_int64_bits: width:int -> bits:('a -> int64) -> 'a -> float ->
	float
(** \[0,[bound]).
    Draws out [width] bits once by [bits].
    [bits] must return a value fits into [width] bits.
    The result has the precision of the minimum of 53 bits and [width] bits. *)

val float_exclusive_from_int64_bits: width:int -> bits:('a -> int64) -> 'a ->
	float -> float
(** (0,[bound]).
    Draws out values repeatedly as needed by [bits] in [width] bit units.
    [width] must be at least 2 and [bits] must return a value fits into [width]
    bits.
    The result has the precision of the minimum of 53 bits and [width] bits. *)

val float_inclusive_from_int64_bits: width:int -> bits:('a -> int64) -> 'a ->
	float -> float
(** \[0,[bound]\].
    Draws out values repeatedly as needed by [bits] in [width] bit units.
    [bits] must return a value fits into [width] bits.
    The result has the precision of the minimum of 53 bits and [width] - 1
    bits. *)

(** {2 Pre-bindable functions} *)

val bind_int_from_int64_bits: width:int -> bits:('a -> int64) -> int -> 'a ->
	unit -> int
(** Like {!int_from_int64_bits} except the order of the parameters differs for
    efficiency of partial application. *)

val bind_int32_from_int64_bits: width:int -> bits:('a -> int64) -> int32 ->
	'a -> unit -> int32
val bind_int64_from_int64_bits: width:int -> bits:('a -> int64) -> int64 ->
	'a -> unit -> int64
