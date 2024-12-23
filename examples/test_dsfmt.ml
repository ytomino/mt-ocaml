module type DsfmtS = sig
	val min_float_array_length: int
	type t
	val make_int32: int32 -> t
	val copy: t -> t
	val bits52: t -> int64
	val float_bits52: t -> float
	val int: t -> int -> int
	val int32: t -> int32 -> int32
end;;

module Check (Dsfmt: DsfmtS) = struct
	
	(* state management *)
	
	let s1 = Dsfmt.make_int32 1234l in
	let s2 = Dsfmt.copy s1 in
	for _ = 1 to Dsfmt.min_float_array_length + 1 do
		let r = Dsfmt.float_bits52 s1 in
		assert (Dsfmt.float_bits52 s2 = r)
	done;;
	
	(* drawing out all of [0,bound) *)
	
	let bound = 6 in
	let s = Dsfmt.make_int32 1234l in
	(* int *)
	let drawn = ref 0 in
	while !drawn <> 1 lsl bound - 1 do
		let x = Dsfmt.int s bound in
		assert (x >= 0 && x < bound);
		drawn := !drawn lor (1 lsl x)
	done;
	(* int32 *)
	let boundl = Int32.of_int bound in
	let drawn = ref 0 in
	while !drawn <> 1 lsl bound - 1 do
		let x = Dsfmt.int32 s boundl in
		assert (x >= 0l && x < boundl);
		drawn := !drawn lor (1 lsl Int32.to_int x)
	done;;
	
	(* taking from higher bits *)
	
	let s1 = Dsfmt.make_int32 1234l in
	let s2 = Dsfmt.copy s1 in
	(* bits52 *)
	for _ = 1 to 10 do
		let x1 = Int64.of_float (ldexp (Dsfmt.float_bits52 s1) 52) in
		let x2 = Dsfmt.bits52 s2 in
		assert (x1 = x2)
	done;
	(* int *)
	for i = 1 to min 52 (Sys.word_size - 3) do
		for _ = 1 to 10 do
			let x1 = Int64.to_int (Int64.shift_right_logical (Dsfmt.bits52 s1) (52 - i)) in
			let x2 = Dsfmt.int s2 (1 lsl i) in
			assert (x1 = x2)
		done
	done;
	(* int32 *)
	for i = 1 to 30 do
		for _ = 1 to 10 do
			let x1 = Int64.to_int32 (Int64.shift_right_logical (Dsfmt.bits52 s1) (52 - i))
			in
			let x2 = Dsfmt.int32 s2 (Int32.shift_left 1l i) in
			assert (x1 = x2)
		done
	done;;
	
end;;

module _ = Check (Dsfmt_19937);;
module _ = Check (Dsfmt_216091);;

prerr_endline "ok";;
