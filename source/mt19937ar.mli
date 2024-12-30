(** Mersenne Twister pseudorandom number generator, with the period 2^19937 - 1
    in 32 bit units. *)

(** {1 Generator} *)

type t
(** The type of the state. *)

(** {2 Initialization} *)

external make_int32: int32 -> t = "mlmt_mt19937ar_make_int32"
external make_int32_array: int32 array -> t = "mlmt_mt19937ar_make_int32_array"
val make: int array -> t
val make_self_init: unit -> t

external copy: t -> t = "mlmt_mt19937ar_copy"

(** {2 Bit generation} *)

external bits31: t -> int = "mlmt_mt19937ar_bits31" [@@ocaml.noalloc]
(** Draws out 31 bits of higher part of 32 bits.
    It calls {e genrand_int31}. *)

external bits32: t -> (int32 [@ocaml.unboxed]) =
	"mlmt_mt19937ar_bits32" "mlmt_mt19937ar_bits32_unboxed"
	[@@ocaml.noalloc]
(** Draws out 32 bits.
    It calls {e genrand_int32}. *)

val bits64: t -> int64
(** Draws out 64 bits.
    The result is composed by drawing out 32 bit twice, first for the lower
    bits and then for upper bits, with {!bits32}. *)

external float_bits32: t -> (float [@ocaml.unboxed]) =
	"mlmt_mt19937ar_float_bits32" "mlmt_mt19937ar_float_bits32_unboxed"
	[@@ocaml.noalloc]
(** Draws out 32 bits as [float] \[0,1).
    It calls {e genrand_real2}. *)

(** {2 Uniform distribution} *)

val int: t -> int -> int
(** \[0,[bound]).
    Draws out values repeatedly as needed in 32 bit units if the [bound] fits
    into 32 bits otherwise 64 bit units. *)

val int32: t -> int32 -> int32
val int64: t -> int64 -> int64
val nativeint: t -> nativeint -> nativeint

val float: t -> float -> float
(** \[0,[bound]), unlike [Stdlib.Random.float].
    Draws out 64 bits once. *)

val bool: t -> bool
(** [false] or [true].
    Draws out 32 bits once. *)

(** {2 Serialization} *)

val import: int32 array * int -> t
external export: t -> int32 array * int = "mlmt_mt19937ar_export"
