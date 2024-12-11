external random_seed: unit -> int array = "caml_sys_random_seed";;

let int64_of_unsigned_int32 (x: int32) =
	Int64.logand (Int64.of_int32 x) 0xFFFFFFFFL;;

let n = 624;;

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

let int: t -> int -> int = Uniform_distribution.int ~bits32 ~bits64;;
let int32: t -> int32 -> int32 = Uniform_distribution.int32 ~bits32;;
let int64: t -> int64 -> int64 = Uniform_distribution.int64 ~bits32 ~bits64;;

let nativeint: t -> nativeint -> nativeint =
	if Sys.word_size <= 32 then (
		fun state bound -> Nativeint.of_int32 (int32 state (Nativeint.to_int32 bound))
	) else (
		fun state bound -> Int64.to_nativeint (int64 state (Int64.of_nativeint bound))
	);;

let float (state: t) (bound: float) =
	if bound > 0. then (
		let x = Int64.shift_right_logical (bits64 state) (64 - 53) in
		let x = ldexp (Int64.to_float x) (-53) in (* [0,1) *)
		bound *. x
	) else invalid_arg "Mt19937ar.float";; (* __FUNCTION__ *)

let bool (state: t) = bits31 state land 0x40000000 <> 0;;

external unsafe_import: int32 array * int -> t =
	"mlmt_mt19937ar_unsafe_import";;

let import (source: int32 array * int) =
	let mt, mti = source in
	if Array.length mt = n && mti >= 0 && mti <= n then unsafe_import source
	else invalid_arg "Mt19937ar.import";; (* __FUNCTION__ *)

external export: t -> int32 array * int = "mlmt_mt19937ar_export";;
