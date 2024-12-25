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

let raw_int63: type t. int64 -> (t -> int64) -> t -> int64 -> int64 =
	let rec draw63 max_bound bits state max_dividend =
		let x = bits state in
		assert (Int64.unsigned_compare x max_bound < 0);
		(* x >= 0L && max_dividend > 0L *)
		if x <= max_dividend then x
		else draw63 max_bound bits state max_dividend
	in
	fun max_bound bits state bound ->
	(* Int64.sub max_bound bound >= 0L && bound > 1L *)
	let d = Int64.succ (Int64.div (Int64.sub max_bound bound) bound) in
	let m = Int64.pred (Int64.mul bound d) in
	(* m > 0L && d > 0L *)
	Int64.div (draw63 max_bound bits state m) d;;

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

let int_from_int64_bits (type t) ~(width: int) ~(bits: t -> int64)
	: t -> int -> int =
	let loc = "Uniform_distribution.int_from_int64_bits" in (* __FUNCTION__ *)
	if width > 0 && width < 64 then (
		let max_bound = Int64.shift_left 1L width in
		fun state bound ->
		let bound64 = Int64.of_int bound in
		if Int64.unsigned_compare (Int64.sub bound64 2L) (Int64.sub max_bound 2L) <= 0
			(* bound > 1 && bound <= 2 ** n *)
		then Int64.to_int (raw_int63 max_bound bits state bound64)
		else
		if bound = 1 then 0
		else invalid_arg loc
	) else if width = 64 then (
		fun state bound ->
		if bound > 1 then (
			let bound64 = Int64.of_int bound in
			Int64.to_int (raw_int64 bits state bound64)
		) else
		if bound > 0 then 0
		else invalid_arg loc
	) else invalid_arg loc
	[@@inline always];;

let int32_from_int64_bits (type t) ~(width: int) ~(bits: t -> int64)
	: t -> int32 -> int32 =
	let loc = "Uniform_distribution.int32_from_int64_bits" in (* __FUNCTION__ *)
	if width > 0 && width < 64 then (
		let max_bound = Int64.shift_left 1L width in
		fun state bound ->
		let bound64 = Int64.of_int32 bound in
		if Int64.unsigned_compare (Int64.sub bound64 2L) (Int64.sub max_bound 2L) <= 0
			(* bound > 1 && bound <= 2 ** n *)
		then Int64.to_int32 (raw_int63 max_bound bits state bound64)
		else
		if bound = 1l then 0l
		else invalid_arg loc
	) else if width = 64 then (
		fun state bound ->
		if bound > 1l then (
			let bound64 = Int64.of_int32 bound in
			Int64.to_int32 (raw_int64 bits state bound64)
		) else
		if bound > 0l then 0l
		else invalid_arg loc
	) else invalid_arg loc
	[@@inline always];;

let int64_from_int64_bits (type t) ~(width: int) ~(bits: t -> int64)
	: t -> int64 -> int64 =
	let loc = "Uniform_distribution.int64_from_int64_bits" in (* __FUNCTION__ *)
	if width > 0 && width < 64 then (
		let max_bound = Int64.shift_left 1L width in
		fun state bound ->
		if Int64.unsigned_compare (Int64.sub bound 2L) (Int64.sub max_bound 2L) <= 0
			(* bound > 1 && bound <= 2 ** n *)
		then raw_int63 max_bound bits state bound
		else
		if bound = 1L then 0L
		else invalid_arg loc
	) else if width = 64 then (
		fun state bound ->
		if bound > 1L then raw_int64 bits state bound else
		if bound > 0L then 0L
		else invalid_arg loc
	) else invalid_arg loc
	[@@inline always];;

let float_from_int64_bits (type t) ~(width: int) ~(bits: t -> int64)
	: t -> float -> float =
	let loc = "Uniform_distribution.float_from_int64_bits" in (* __FUNCTION__ *)
	if width > 0 && width <= 53 then (
		let max_bound = Int64.shift_left 1L width in
		let exp = ~-width in
		fun state bound ->
		if bound > 0. then (
			let x = bits state in
			assert (Int64.unsigned_compare x max_bound < 0);
			let x = ldexp (Int64.to_float x) exp in (* [0,1) *)
			bound *. x
		) else invalid_arg loc
	) else if width >= 53 && width < 64 then (
		let max_bound = Int64.shift_left 1L width in
		let shift = width - 53 in
		fun state bound ->
		if bound > 0. then (
			let x = bits state in
			assert (Int64.unsigned_compare x max_bound < 0);
			let x = Int64.shift_right_logical x shift in
			let x = ldexp (Int64.to_float x) (-53) in (* [0,1) *)
			bound *. x
		) else invalid_arg loc
	) else if width = 64 then (
		fun state bound ->
		if bound > 0. then (
			let x = bits state in
			let x = Int64.shift_right_logical x (64 - 53) in
			let x = ldexp (Int64.to_float x) (-53) in (* [0,1) *)
			bound *. x
		) else invalid_arg loc
	) else invalid_arg loc
	[@@inline always];;
