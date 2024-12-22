module type DsfmtS = sig
	val min_float_array_length: int
	type t
	val make_int32: int32 -> t
	val copy: t -> t
	val float_bits52: t -> float
end;;

module Check (Dsfmt: DsfmtS) = struct
	
	(* state management *)
	
	let s1 = Dsfmt.make_int32 1234l in
	let s2 = Dsfmt.copy s1 in
	for _ = 1 to Dsfmt.min_float_array_length + 1 do
		let r = Dsfmt.float_bits52 s1 in
		assert (Dsfmt.float_bits52 s2 = r)
	done;;
	
end;;

module _ = Check (Dsfmt_19937);;
module _ = Check (Dsfmt_216091);;

prerr_endline "ok";;
