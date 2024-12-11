module type SfmtS = sig
	val min_int32_array_length: int
	type t
	val make_int32: int32 -> t
	val copy: t -> t
	val bits32: t -> int32
	val bits64: t -> int64
	val int: t -> int -> int
	val int32: t -> int32 -> int32
	val int64: t -> int64 -> int64
	val nativeint: t -> nativeint -> nativeint
end;;

module Check (Sfmt: SfmtS) = struct
	
	(* state management *)
	
	let s1 = Sfmt.make_int32 12345l in
	let s2 = Sfmt.copy s1 in
	for _ = 1 to Sfmt.min_int32_array_length + 1 do
		let r = Sfmt.bits32 s1 in
		assert (Sfmt.bits32 s2 = r)
	done;;
	
	(* drawing out all of [0,bound) *)
	
	let bound = 6 in
	let s = Sfmt.make_int32 12345l in
	(* int *)
	let drawn = ref 0 in
	while !drawn <> 1 lsl bound - 1 do
		let x = Sfmt.int s bound in
		assert (x >= 0 && x < bound);
		drawn := !drawn lor (1 lsl x)
	done;
	(* int32 *)
	let boundl = Int32.of_int bound in
	let drawn = ref 0 in
	while !drawn <> 1 lsl bound - 1 do
		let x = Sfmt.int32 s boundl in
		assert (x >= 0l && x < boundl);
		drawn := !drawn lor (1 lsl Int32.to_int x)
	done;
	(* int64 *)
	let boundL = Int64.of_int bound in
	let drawn = ref 0 in
	while !drawn <> 1 lsl bound - 1 do
		let x = Sfmt.int64 s boundL in
		assert (x >= 0L && x < boundL);
		drawn := !drawn lor (1 lsl Int64.to_int x)
	done;
	(* nativeint *)
	let boundn = Nativeint.of_int bound in
	let drawn = ref 0 in
	while !drawn <> 1 lsl bound - 1 do
		let x = Sfmt.nativeint s boundn in
		assert (x >= 0n && x < boundn);
		drawn := !drawn lor (1 lsl Nativeint.to_int x)
	done;;
	
	(* taking from higher bits *)
	
	let draw_bits state n =
		if n <= 32 then Int64.shift_left (Int64.of_int32 (Sfmt.bits32 state)) 32
		else Sfmt.bits64 state
	in
	let s1 = Sfmt.make_int32 12345l in
	let s2 = Sfmt.copy s1 in
	(* int *)
	for i = 1 to Sys.word_size - 3 do
		for _ = 1 to 10 do
			let x1 = Int64.to_int (Int64.shift_right_logical (draw_bits s1 i) (64 - i)) in
			let x2 = Sfmt.int s2 (1 lsl i) in
			assert (x1 = x2)
		done
	done;
	(* int32 *)
	for i = 1 to 30 do
		for _ = 1 to 10 do
			let x1 = Int32.shift_right_logical (Sfmt.bits32 s1) (32 - i) in
			let x2 = Sfmt.int32 s2 (Int32.shift_left 1l i) in
			assert (x1 = x2)
		done
	done;
	(* int64 *)
	for i = 1 to 62 do
		for _ = 1 to 10 do
			let x1 = Int64.shift_right_logical (draw_bits s1 i) (64 - i) in
			let x2 = Sfmt.int64 s2 (Int64.shift_left 1L i) in
			assert (x1 = x2)
		done
	done;
	(* nativeint *)
	for i = 1 to Sys.word_size - 2 do
		for _ = 1 to 10 do
			let x1 =
				Int64.to_nativeint (Int64.shift_right_logical (draw_bits s1 i) (64 - i))
			in
			let x2 = Sfmt.nativeint s2 (Nativeint.shift_left 1n i) in
			assert (x1 = x2)
		done
	done;;
	
end;;

module _ = Check (Sfmt_19937);;
module _ = Check (Sfmt_216091);;

prerr_endline "ok";;
