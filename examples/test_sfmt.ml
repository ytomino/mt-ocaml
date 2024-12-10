module type SfmtS = sig
	val min_int32_array_length: int
	type t
	val make_int32: int32 -> t
	val copy: t -> t
	val bits32: t -> int32
end;;

module Check (Sfmt: SfmtS) = struct
	
	(* state management *)
	
	let s1 = Sfmt.make_int32 12345l in
	let s2 = Sfmt.copy s1 in
	for _ = 1 to Sfmt.min_int32_array_length + 1 do
		let r = Sfmt.bits32 s1 in
		assert (Sfmt.bits32 s2 = r)
	done;;
	
end;;

module _ = Check (Sfmt_19937);;
module _ = Check (Sfmt_216091);;

prerr_endline "ok";;
