(* state management *)

let s1 = Mt19937ar.make_int32 5489l in
let s2 = Mt19937ar.copy s1 in
let s3 = Mt19937ar.import (Mt19937ar.export s1) in
for _ = 1 to 624 + 1 do
	let r = Mt19937ar.bits32 s1 in
	assert (Mt19937ar.bits32 s2 = r);
	assert (Mt19937ar.bits32 s3 = r)
done;
let data = Mt19937ar.export s1 in
assert (Mt19937ar.export s2 = data);
assert (Mt19937ar.export s3 = data);;

(* drawing out all of [0,bound) *)

let bound = 6 in
let s = Mt19937ar.make_int32 5489l in
(* int *)
let drawn = ref 0 in
while !drawn <> 1 lsl bound - 1 do
	let x = Mt19937ar.int s bound in
	assert (x >= 0 && x < bound);
	drawn := !drawn lor (1 lsl x)
done;
(* int32 *)
let boundl = Int32.of_int bound in
let drawn = ref 0 in
while !drawn <> 1 lsl bound - 1 do
	let x = Mt19937ar.int32 s boundl in
	assert (x >= 0l && x < boundl);
	drawn := !drawn lor (1 lsl Int32.to_int x)
done;
(* int64 *)
let boundL = Int64.of_int bound in
let drawn = ref 0 in
while !drawn <> 1 lsl bound - 1 do
	let x = Mt19937ar.int64 s boundL in
	assert (x >= 0L && x < boundL);
	drawn := !drawn lor (1 lsl Int64.to_int x)
done;
(* nativeint *)
let boundn = Nativeint.of_int bound in
let drawn = ref 0 in
while !drawn <> 1 lsl bound - 1 do
	let x = Mt19937ar.nativeint s boundn in
	assert (x >= 0n && x < boundn);
	drawn := !drawn lor (1 lsl Nativeint.to_int x)
done;
(* float *)
let bound_float = Float.of_int bound in
let drawn = ref 0 in
while !drawn <> 1 lsl bound - 1 do
	let x = Mt19937ar.float s bound_float in
	assert (x >= 0. && x < bound_float);
	drawn := !drawn lor (1 lsl int_of_float x)
done;
(* bool *)
let drawn = ref 0 in
while !drawn <> 3 do
	let x = Mt19937ar.bool s in
	drawn := !drawn lor (1 lsl Bool.to_int x)
done;;

(* drawing out 0 and 1 as lowerest bit *)

let s = Mt19937ar.make_int32 5489l in
(* float *)
let drawn = ref 0 in
while !drawn <> 3 do
	let x = int_of_float (mod_float (ldexp (Mt19937ar.float s 1.) 55) 8.) in
	assert (x land 3 = 0);
	drawn := !drawn lor (1 lsl (x lsr 2))
done;
let drawn = ref 0 in
while !drawn <> 3 do
	let x = int_of_float (mod_float (ldexp (Mt19937ar.float_bits32 s) 34) 8.) in
	assert (x land 3 = 0);
	drawn := !drawn lor (1 lsl (x lsr 2))
done;;

(* taking from higher bits *)

let draw_bits state n =
	if n <= 32 then Int64.shift_left (Int64.of_int32 (Mt19937ar.bits32 state)) 32
	else Mt19937ar.bits64 state
in
let s1 = Mt19937ar.make_int32 5489l in
let s2 = Mt19937ar.copy s1 in
(* int *)
for i = 1 to Sys.word_size - 3 do
	for _ = 1 to 10 do
		let x1 = Int64.to_int (Int64.shift_right_logical (draw_bits s1 i) (64 - i)) in
		let x2 = Mt19937ar.int s2 (1 lsl i) in
		assert (x1 = x2)
	done
done;
(* int32 *)
for i = 1 to 30 do
	for _ = 1 to 10 do
		let x1 = Int32.shift_right_logical (Mt19937ar.bits32 s1) (32 - i) in
		let x2 = Mt19937ar.int32 s2 (Int32.shift_left 1l i) in
		assert (x1 = x2)
	done
done;
(* int64 *)
for i = 1 to 62 do
	for _ = 1 to 10 do
		let x1 = Int64.shift_right_logical (draw_bits s1 i) (64 - i) in
		let x2 = Mt19937ar.int64 s2 (Int64.shift_left 1L i) in
		assert (x1 = x2)
	done
done;
(* nativeint *)
for i = 1 to Sys.word_size - 2 do
	for _ = 1 to 10 do
		let x1 =
			Int64.to_nativeint (Int64.shift_right_logical (draw_bits s1 i) (64 - i))
		in
		let x2 = Mt19937ar.nativeint s2 (Nativeint.shift_left 1n i) in
		assert (x1 = x2)
	done
done;
(* float *)
for i = 1 to 53 do
	for _ = 1 to 10 do
		let x1 =
			Int64.to_float (Int64.shift_right_logical (Mt19937ar.bits64 s1) (64 - i))
		in
		let x2 = floor (Mt19937ar.float s2 (ldexp 1. i)) in
		assert (x1 = x2)
	done
done;
for _ = 1 to 10 do
	let _: int32 = Mt19937ar.bits32 s1 in (* Mt19937ar.float draw 64bit *)
	let x1 = Mt19937ar.float_bits32 s1 in
	let x2 = ldexp (floor (ldexp (Mt19937ar.float s2 1.) 32)) (-32) in
	assert (x1 = x2)
done;
(* bool *)
for _ = 1 to 10 do
	let x1 = Int32.shift_right_logical (Mt19937ar.bits32 s1) 31 <> 0l in
	let x2 = Mt19937ar.bool s2 in
	assert (x1 = x2)
done;;

prerr_endline "ok";;
