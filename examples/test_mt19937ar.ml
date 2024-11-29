(* state management *)

let s1 = Mt19937ar.make_int32 5489l in
let s2 = Mt19937ar.copy s1 in
for _ = 1 to 624 + 1 do
	let r = Mt19937ar.bits32 s1 in
	assert (Mt19937ar.bits32 s2 = r)
done;;

prerr_endline "ok";;
