let int_of_unsigned_int32: int32 -> int =
	if Sys.word_size <= 32 then Int32.to_int
	else (fun x -> (Int32.to_int x) land (1 lsl 32 - 1));;

let int64_of_unsigned_int32 (x: int32) =
	Int64.logand (Int64.of_int32 x) 0xFFFFFFFFL;;

let draw32: type t. (t -> int32) -> t -> int32 -> int32 =
	let rec draw32 bits32 state max_dividend =
		let x = bits32 state in
		if Int32.unsigned_compare x max_dividend <= 0 then x
		else draw32 bits32 state max_dividend
	in
	draw32;;

let divisor32 (bound: int32) =
	Int32.succ (Int32.unsigned_div (Int32.sub Int32.zero bound) bound);;

let max_dividend32 (bound: int32) (divisor: int32) =
	Int32.pred (Int32.mul bound divisor);;

let draw_div32 (type t) (bits: t -> int32) (state: t) (max_dividend: int32)
	(divisor: int32) =
	Int32.unsigned_div (draw32 bits state max_dividend) divisor;;

let raw_int32 (type t) (bits32: t -> int32) (state: t) (bound: int32) =
	let d = divisor32 bound in
	let m = max_dividend32 bound d in
	draw_div32 bits32 state m d;;

let draw63: type t. int64 -> (t -> int64) -> t -> int64 -> int64 =
	let rec draw63 max_bound bits state max_dividend =
		let x = bits state in
		assert (Int64.unsigned_compare x max_bound < 0);
		(* x >= 0L && max_dividend > 0L *)
		if x <= max_dividend then x
		else draw63 max_bound bits state max_dividend
	in
	draw63;;

let divisor63 (max_bound: int64) (bound: int64) =
	(* Int64.sub max_bound bound >= 0L && bound > 1L *)
	Int64.succ (Int64.div (Int64.sub max_bound bound) bound);;

let max_dividend63 (bound: int64) (divisor: int64) =
	Int64.pred (Int64.mul bound divisor);;

let draw_div63 (type t) (max_bound: int64) (bits: t -> int64) (state: t)
	(max_dividend: int64) (divisor: int64) =
	(* m > 0L && d > 0L *)
	Int64.div (draw63 max_bound bits state max_dividend) divisor;;

let raw_int63 (type t) (max_bound: int64) (bits: t -> int64) (state: t)
	(bound: int64) =
	let d = divisor63 max_bound bound in
	let m = max_dividend63 bound d in
	draw_div63 max_bound bits state m d;;

let draw64: type t. (t -> int64) -> t -> int64 -> int64 =
	let rec draw64 bits64 state max_dividend =
		let x = bits64 state in
		if Int64.unsigned_compare x max_dividend <= 0 then x
		else draw64 bits64 state max_dividend
	in
	draw64;;

let divisor64 (bound: int64) =
	Int64.succ (Int64.unsigned_div (Int64.sub Int64.zero bound) bound);;

let max_dividend64: int64 -> int64 -> int64 = max_dividend63;;

let draw_div64 (type t) (bits: t -> int64) (state: t) (max_dividend: int64)
	(divisor: int64) =
	Int64.unsigned_div (draw64 bits state max_dividend) divisor;;

let raw_int64 (type t) (bits64: t -> int64) (state: t) (bound: int64) =
	let d = divisor64 bound in
	let m = max_dividend64 bound d in
	draw_div64 bits64 state m d;;

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

let zero_f _ _ = 0;;
let zero32_f _ _ = 0l;;
let zero64_f _ _ = 0L;;

let bind_int (type t) ~(bits32: t -> int32) ~(bits64: t -> int64) (bound: int)
	: t -> unit -> int =
	if bound > 1 then (
		let bound64 = Int64.of_int bound in
		let c = Int64.compare bound64 0x100000000L in
		if c < 0 then (
			let bound32 = Int32.of_int bound in
			let divisor = divisor32 bound32 in
			let max_dividend = max_dividend32 bound32 divisor in
			fun state _ ->
			let x = draw_div32 bits32 state max_dividend divisor in
			int_of_unsigned_int32 x
		) else if c = 0 then (
			fun state _ -> int_of_unsigned_int32 (bits32 state)
		) else (
			let divisor = divisor64 bound64 in
			let max_dividend = max_dividend64 bound64 divisor in
			fun state _ ->
			let x = draw_div64 bits64 state max_dividend divisor in
			Int64.to_int x
		)
	) else
	if bound > 0 then zero_f
	else invalid_arg "Uniform_distribution.bind_int" (* __FUNCTION__ *)
	[@@inline always];;

let bind_int32 (type t) ~(bits32: t -> int32) (bound: int32)
	: t -> unit -> int32 =
	if bound > 1l then (
		let divisor = divisor32 bound in
		let max_dividend = max_dividend32 bound divisor in
		fun state _ -> draw_div32 bits32 state max_dividend divisor
	) else
	if bound > 0l then zero32_f
	else invalid_arg "Uniform_distribution.bind_int32" (* __FUNCTION__ *)
	[@@inline always];;

let bind_int64 (type t) ~(bits32: t -> int32) ~(bits64: t -> int64)
	(bound: int64)
	: t -> unit -> int64 =
	if bound > 1L then (
		let c = Int64.compare bound 0x100000000L in
		if c < 0 then (
			let bound32 = Int64.to_int32 bound in
			let divisor = divisor32 bound32 in
			let max_dividend = max_dividend32 bound32 divisor in
			fun state _ ->
			let x = draw_div32 bits32 state max_dividend divisor in
			int64_of_unsigned_int32 x
		) else if c = 0 then (
			fun state _ -> int64_of_unsigned_int32 (bits32 state)
		) else (
			let divisor = divisor64 bound in
			let max_dividend = max_dividend64 bound divisor in
			fun state _ -> draw_div64 bits64 state max_dividend divisor
		)
	) else
	if bound > 0L then zero64_f
	else invalid_arg "Uniform_distribution.bind_int64" (* __FUNCTION__ *)
	[@@inline always];;

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

let fe53_bound = 0x1FFFFFFFFFFFFFL;;
let fe64_divisor = divisor64 fe53_bound;;
let fe64_max_dividend = max_dividend64 fe53_bound fe64_divisor;;

let float_exclusive_from_int64_bits (type t) ~(width: int) ~(bits: t -> int64)
	: t -> float -> float =
	let loc =
		"Uniform_distribution.float_exclusive_from_int64_bits" (* __FUNCTION__ *)
	in
	if width > 1 && width <= 53 then (
		let fe_max_bound = Int64.shift_left 1L width in
		let fe_bound = Int64.pred fe_max_bound in
		let fe_divisor = 1L in (* divisor63 fe_max_bound fe_bound *)
		let fe_max_dividend = max_dividend63 fe_bound fe_divisor in
		let exp = ~-width in
		fun state bound ->
		if bound > 0. then (
			let x =
				draw63 fe_max_bound bits state fe_max_dividend
					(* draw_div63 fe_max_bound bits state fe_max_dividend fe_divisor *)
			in
			let x = Int64.succ x in
			let x = ldexp (Int64.to_float x) exp in (* (0,1) *)
			bound *. x
		) else invalid_arg loc
	) else if width >= 53 && width < 64 then (
		let fe_max_bound = Int64.shift_left 1L width in
		let fe_divisor = divisor63 fe_max_bound fe53_bound in
		let fe_max_dividend = max_dividend63 fe53_bound fe_divisor in
		fun state bound ->
		if bound > 0. then (
			let x = draw_div63 fe_max_bound bits state fe_max_dividend fe_divisor in
			let x = Int64.succ x in
			let x = ldexp (Int64.to_float x) (-53) in (* (0,1) *)
			bound *. x
		) else invalid_arg loc
	) else if width = 64 then (
		fun state bound ->
		if bound > 0. then (
			let x = draw_div64 bits state fe64_max_dividend fe64_divisor in
			let x = Int64.succ x in
			let x = ldexp (Int64.to_float x) (-53) in (* (0,1) *)
			bound *. x
		) else invalid_arg loc
	) else invalid_arg loc
	[@@inline always];;

let fi53_bound = 0x20000000000001L;;
let fi64_divisor = divisor64 fi53_bound;;
let fi64_max_dividend = max_dividend64 fi53_bound fi64_divisor;;

let float_inclusive_from_int64_bits (type t) ~(width: int) ~(bits: t -> int64)
	: t -> float -> float =
	let loc =
		"Uniform_distribution.float_inclusive_from_int64_bits" (* __FUNCTION__ *)
	in
	if width > 0 && width <= 54 then (
		let fi_max_bound = Int64.shift_left 1L width in
		let fi_bound = Int64.succ (Int64.shift_right_logical fi_max_bound 1) in
		let fi_divisor = 1L in (* divisor63 fi_max_bound fi_bound *)
		let fi_max_dividend = max_dividend63 fi_bound fi_divisor in
		let exp = ~-(width - 1) in
		fun state bound ->
		if bound > 0. then (
			let x =
				draw63 fi_max_bound bits state fi_max_dividend
					(* draw_div63 fe_max_bound bits state fe_max_dividend fe_divisor *)
			in
			let x = ldexp (Int64.to_float x) exp in (* [0,1] *)
			bound *. x
		) else invalid_arg loc
	) else if width >= 54 && width < 64 then (
		let fi_max_bound = Int64.shift_left 1L width in
		let fi_divisor = divisor63 fi_max_bound fi53_bound in
		let fi_max_dividend = max_dividend63 fi53_bound fi_divisor in
		fun state bound ->
		if bound > 0. then (
			let x = draw_div63 fi_max_bound bits state fi_max_dividend fi_divisor in
			let x = ldexp (Int64.to_float x) (-53) in (* [0,1] *)
			bound *. x
		) else invalid_arg loc
	) else if width = 64 then (
		fun state bound ->
		if bound > 0. then (
			let x = draw_div64 bits state fi64_max_dividend fi64_divisor in
			let x = ldexp (Int64.to_float x) (-53) in (* [0,1] *)
			bound *. x
		) else invalid_arg loc
	) else invalid_arg loc
	[@@inline always];;

let bind_int_from_int64_bits (type t) ~(width: int) ~(bits: t -> int64)
	(bound: int)
	: t -> unit -> int =
	let loc = "Uniform_distribution.bind_int_from_int64_bits" in (* __FUNCTION__ *)
	if width > 0 && width < 64 then (
		let bound64 = Int64.of_int bound in
		let max_bound = Int64.shift_left 1L width in
		if Int64.unsigned_compare (Int64.sub bound64 2L) (Int64.sub max_bound 2L) <= 0
			(* bound > 1 && bound <= 2 ** n *)
		then (
			let divisor = divisor63 max_bound bound64 in
			let max_dividend = max_dividend63 bound64 divisor in
			fun state _ ->
			let x = draw_div63 max_bound bits state max_dividend divisor in
			Int64.to_int x
		) else
		if bound = 1 then zero_f
		else invalid_arg loc
	) else if width = 64 then (
		if bound > 1 then (
			let bound64 = Int64.of_int bound in
			let divisor = divisor64 bound64 in
			let max_dividend = max_dividend64 bound64 divisor in
			fun state _ ->
			let x = draw_div64 bits state max_dividend divisor in
			Int64.to_int x
		) else
		if bound > 0 then zero_f
		else invalid_arg loc
	) else invalid_arg loc
	[@@inline always];;

let bind_int32_from_int64_bits (type t) ~(width: int) ~(bits: t -> int64)
	(bound: int32)
	: t -> unit -> int32 =
	let loc = "Uniform_distribution.bind_int32_from_int64_bits" (* __FUNCTION__ *)
	in
	if width > 0 && width < 64 then (
		let bound64 = Int64.of_int32 bound in
		let max_bound = Int64.shift_left 1L width in
		if Int64.unsigned_compare (Int64.sub bound64 2L) (Int64.sub max_bound 2L) <= 0
			(* bound > 1 && bound <= 2 ** n *)
		then (
			let divisor = divisor63 max_bound bound64 in
			let max_dividend = max_dividend63 bound64 divisor in
			fun state _ ->
			let x = draw_div63 max_bound bits state max_dividend divisor in
			Int64.to_int32 x
		) else
		if bound = 1l then zero32_f
		else invalid_arg loc
	) else if width = 64 then (
		if bound > 1l then (
			let bound64 = Int64.of_int32 bound in
			let divisor = divisor64 bound64 in
			let max_dividend = max_dividend64 bound64 divisor in
			fun state _ ->
			let x = draw_div64 bits state max_dividend divisor in
			Int64.to_int32 x
		) else
		if bound > 0l then zero32_f
		else invalid_arg loc
	) else invalid_arg loc
	[@@inline always];;

let bind_int64_from_int64_bits (type t) ~(width: int) ~(bits: t -> int64)
	(bound: int64)
	: t -> unit -> int64 =
	let loc = "Uniform_distribution.bind_int64_from_int64_bits" (* __FUNCTION__ *)
	in
	if width > 0 && width < 64 then (
		let max_bound = Int64.shift_left 1L width in
		if Int64.unsigned_compare (Int64.sub bound 2L) (Int64.sub max_bound 2L) <= 0
			(* bound > 1 && bound <= 2 ** n *)
		then (
			let divisor = divisor63 max_bound bound in
			let max_dividend = max_dividend63 bound divisor in
			fun state _ -> draw_div63 max_bound bits state max_dividend divisor
		) else
		if bound = 1L then zero64_f
		else invalid_arg loc
	) else if width = 64 then (
		if bound > 1L then (
			let divisor = divisor64 bound in
			let max_dividend = max_dividend64 bound divisor in
			fun state _ -> draw_div64 bits state max_dividend divisor
		) else
		if bound > 0L then zero64_f
		else invalid_arg loc
	) else invalid_arg loc
	[@@inline always];;
