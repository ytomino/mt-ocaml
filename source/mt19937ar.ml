external random_seed: unit -> int array = "caml_sys_random_seed";;

let int_of_unsigned_int32: int32 -> int =
	if Sys.word_size <= 32 then Int32.to_int
	else (fun x -> (Int32.to_int x) land (1 lsl 32 - 1));;

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

let raw_int32: t -> int32 -> int32 =
	let rec draw32 state max_dividend =
		let x = bits32 state in
		if Int32.unsigned_compare x max_dividend <= 0 then x
		else draw32 state max_dividend
	in
	fun state bound ->
	let d = Int32.succ (Int32.unsigned_div (Int32.sub Int32.zero bound) bound) in
	let m = Int32.pred (Int32.mul bound d) in
	Int32.unsigned_div (draw32 state m) d;;

let raw_int64: t -> int64 -> int64 =
	let rec draw64 state max_dividend =
		let x = bits64 state in
		if Int64.unsigned_compare x max_dividend <= 0 then x
		else draw64 state max_dividend
	in
	fun state bound ->
	let d = Int64.succ (Int64.unsigned_div (Int64.sub Int64.zero bound) bound) in
	let m = Int64.pred (Int64.mul bound d) in
	Int64.unsigned_div (draw64 state m) d;;

let int (state: t) (bound: int) =
	if bound > 1 then (
		let bound64 = Int64.of_int bound in
		let c = Int64.compare bound64 0x100000000L in
		if c < 0 then (
			let bound32 = Int32.of_int bound in
			int_of_unsigned_int32 (raw_int32 state bound32)
		) else
		if c = 0 then int_of_unsigned_int32 (bits32 state)
		else Int64.to_int (raw_int64 state bound64)
	) else 0;;

let int32 (state: t) (bound: int32) =
	if bound > 1l then raw_int32 state bound
	else 0l;;
