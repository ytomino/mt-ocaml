let int_of_unsigned_int32: int32 -> int =
	if Sys.word_size <= 32 then Int32.to_int
	else (fun x -> (Int32.to_int x) land (1 lsl 32 - 1));;

let int64_of_unsigned_int32 (x: int32) =
	Int64.logand (Int64.of_int32 x) 0xFFFFFFFFL;;

let raw_int32: type t. (t -> int32) -> t -> int32 -> int32 =
	let rec draw32 bits32 state max_dividend =
		let x = bits32 state in
		if Int32.unsigned_compare x max_dividend <= 0 then x
		else draw32 bits32 state max_dividend
	in
	fun bits32 state bound ->
	let d = Int32.succ (Int32.unsigned_div (Int32.sub Int32.zero bound) bound) in
	let m = Int32.pred (Int32.mul bound d) in
	Int32.unsigned_div (draw32 bits32 state m) d;;

let raw_int64: type t. (t -> int64) -> t -> int64 -> int64 =
	let rec draw64 bits64 state max_dividend =
		let x = bits64 state in
		if Int64.unsigned_compare x max_dividend <= 0 then x
		else draw64 bits64 state max_dividend
	in
	fun bits64 state bound ->
	let d = Int64.succ (Int64.unsigned_div (Int64.sub Int64.zero bound) bound) in
	let m = Int64.pred (Int64.mul bound d) in
	Int64.unsigned_div (draw64 bits64 state m) d;;

let int (type t) ~(bits32: t -> int32) ~(bits64: t -> int64) (state: t)
	(bound: int) =
	if bound > 1 then (
		let bound64 = Int64.of_int bound in
		let c = Int64.compare bound64 0x100000000L in
		if c < 0 then (
			let bound32 = Int32.of_int bound in
			int_of_unsigned_int32 (raw_int32 bits32 state bound32)
		) else
		if c = 0 then int_of_unsigned_int32 (bits32 state)
		else Int64.to_int (raw_int64 bits64 state bound64)
	) else
	if bound > 0 then 0
	else invalid_arg "Uniform_distribution.int";; (* __FUNCTION__ *)

let int32 (type t) ~(bits32: t -> int32) (state: t) (bound: int32) =
	if bound > 1l then raw_int32 bits32 state bound else
	if bound > 0l then 0l
	else invalid_arg "Uniform_distribution.int32";; (* __FUNCTION__ *)

let int64 (type t) ~(bits32: t -> int32) ~(bits64: t -> int64) (state: t)
	(bound: int64) =
	if bound > 1L then (
		let c = Int64.compare bound 0x100000000L in
		if c < 0 then (
			let bound32 = Int64.to_int32 bound in
			int64_of_unsigned_int32 (raw_int32 bits32 state bound32)
		) else
		if c = 0 then int64_of_unsigned_int32 (bits32 state)
		else raw_int64 bits64 state bound
	) else
	if bound > 0L then 0L
	else invalid_arg "Uniform_distribution.int64";; (* __FUNCTION__ *)
