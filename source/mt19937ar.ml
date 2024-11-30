external random_seed: unit -> int array = "caml_sys_random_seed";;

let int64_of_unsigned_int32 (x: int32) =
	Int64.logand (Int64.of_int32 x) 0xFFFFFFFFL;;

type t;;

external make_int32: int32 -> t = "mlmt_mt19937ar_make_int32";;
external make_int32_array: int32 array -> t =
	"mlmt_mt19937ar_make_int32_array";;

let make (seed: int array) = make_int32_array (Array.map Int32.of_int seed);;
let make_self_init () = make (random_seed ());;

external copy: t -> t = "mlmt_mt19937ar_copy";;

external bits31: t -> int = "mlmt_mt19937ar_bits31";;
external bits32: t -> int32 = "mlmt_mt19937ar_bits32";;

let bits64 (state: t) =
	let make_int32 ~hi ~lo =
		Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32)
			(int64_of_unsigned_int32 lo)
	in
	let lo = bits32 state in
	let hi = bits32 state in
	make_int32 ~hi ~lo;;

external float_bits32: t -> float = "mlmt_mt19937ar_float_bits32";;
