open Uniform_distribution;;

let nocall _ = assert false;;

let const (type t) (x: t) (state: int ref) =
	let i = !state in
	if i = 0 then (
		incr state;
		x
	) else invalid_arg "index out of bounds";;

let of_array (type t) (xs: t array) (state: int ref) =
	let i = !state in
	if i < Array.length xs then (
		incr state;
		xs.(i)
	) else invalid_arg "index out of bounds";;

let try_invalid_argument (type t) (message: string) (f: t -> t) (bound: t) =
	try
		let _: t = f bound in
		false
	with
	| Invalid_argument m -> m = message;;

let try_get (type t) (f: t -> t) =
	try_invalid_argument "index out of bounds" f;;
let try_int_from_int64_bits =
	try_invalid_argument "Uniform_distribution.int_from_int64_bits";;
let try_int32_from_int64_bits =
	try_invalid_argument "Uniform_distribution.int32_from_int64_bits";;
let try_int64_from_int64_bits =
	try_invalid_argument "Uniform_distribution.int64_from_int64_bits";;

(* 2**0 *)
let bound = 1 in
(* int *)
let f bits32 bits64 = int ~bits32 ~bits64 in
assert (f nocall nocall () bound = 0);
(* int_from_int64_bits *)
let f width bits = int_from_int64_bits ~width ~bits in
assert (f 1 nocall () bound = 0);
assert (f 63 nocall () bound = 0);
assert (f 64 nocall () bound = 0);
let boundl = Int32.of_int bound in
(* int32 *)
let f bits32 = int32 ~bits32 in
assert (f nocall () boundl = 0l);
(* int32_from_int64_bits *)
let f width bits = int32_from_int64_bits ~width ~bits in
assert (f 1 nocall () boundl = 0l);
assert (f 63 nocall () boundl = 0l);
assert (f 64 nocall () boundl = 0l);
let boundL = Int64.of_int bound in
(* int64 *)
let f bits32 bits64 = int64 ~bits32 ~bits64 in
assert (f nocall nocall () boundL = 0L);
(* int64_from_int64_bits *)
let f width bits = int64_from_int64_bits ~width ~bits in
assert (f 1 nocall () boundL = 0L);
assert (f 63 nocall () boundL = 0L);
assert (f 64 nocall () boundL = 0L);;

(* 2**1 *)
let bound = 2 in
(* int *)
let f bits32 bits64 = int ~bits32 ~bits64 in
assert (f (const 0l) nocall (ref 0) bound = 0);
assert (f (const 0x7FFFFFFFl) nocall (ref 0) bound = 0);
assert (f (const 0x80000000l) nocall (ref 0) bound = 1);
assert (f (const 0xFFFFFFFFl) nocall (ref 0) bound = 1);
(* int_from_int64_bits *)
let f width bits = int_from_int64_bits ~width ~bits in
assert (f 1 (const 0L) (ref 0) bound = 0);
assert (f 1 (const 1L) (ref 0) bound = 1);
assert (f 63 (const 0L) (ref 0) bound = 0);
assert (f 63 (const 0x3FFFFFFFFFFFFFFFL) (ref 0) bound = 0);
assert (f 63 (const 0x4000000000000000L) (ref 0) bound = 1);
assert (f 63 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) bound = 1);
assert (f 64 (const 0L) (ref 0) bound = 0);
assert (f 64 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) bound = 0);
assert (f 64 (const 0x8000000000000000L) (ref 0) bound = 1);
assert (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0) bound = 1);
let boundl = Int32.of_int bound in
(* int32 *)
let f bits32 = int32 ~bits32 in
assert (f (const 0l) (ref 0) boundl = 0l);
assert (f (const 0x7FFFFFFFl) (ref 0) boundl = 0l);
assert (f (const 0x80000000l) (ref 0) boundl = 1l);
assert (f (const 0xFFFFFFFFl) (ref 0) boundl = 1l);
(* int32_from_int64_bits *)
let f width bits = int32_from_int64_bits ~width ~bits in
assert (f 1 (const 0L) (ref 0) boundl = 0l);
assert (f 1 (const 1L) (ref 0) boundl = 1l);
assert (f 63 (const 0L) (ref 0) boundl = 0l);
assert (f 63 (const 0x3FFFFFFFFFFFFFFFL) (ref 0) boundl = 0l);
assert (f 63 (const 0x4000000000000000L) (ref 0) boundl = 1l);
assert (f 63 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) boundl = 1l);
assert (f 64 (const 0L) (ref 0) boundl = 0l);
assert (f 64 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) boundl = 0l);
assert (f 64 (const 0x8000000000000000L) (ref 0) boundl = 1l);
assert (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0) boundl = 1l);
let boundL = Int64.of_int bound in
(* int64 *)
let f bits32 bits64 = int64 ~bits32 ~bits64 in
assert (f (const 0l) nocall (ref 0) boundL = 0L);
assert (f (const 0x7FFFFFFFl) nocall (ref 0) boundL = 0L);
assert (f (const 0x80000000l) nocall (ref 0) boundL = 1L);
assert (f (const 0xFFFFFFFFl) nocall (ref 0) boundL = 1L);
(* int64_from_int64_bits *)
let f width bits = int64_from_int64_bits ~width ~bits in
assert (f 1 (const 0L) (ref 0) boundL = 0L);
assert (f 1 (const 1L) (ref 0) boundL = 1L);
assert (f 63 (const 0L) (ref 0) boundL = 0L);
assert (f 63 (const 0x3FFFFFFFFFFFFFFFL) (ref 0) boundL = 0L);
assert (f 63 (const 0x4000000000000000L) (ref 0) boundL = 1L);
assert (f 63 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) boundL = 1L);
assert (f 64 (const 0L) (ref 0) boundL = 0L);
assert (f 64 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) boundL = 0L);
assert (f 64 (const 0x8000000000000000L) (ref 0) boundL = 1L);
assert (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0) boundL = 1L);;

(* 2**1 + 1 *)
let bound = 3 in
(* int *)
let f bits32 bits64 = int ~bits32 ~bits64 in
assert (f (const 0l) nocall (ref 0) bound = 0);
assert (f (const 0x55555554l) nocall (ref 0) bound = 0);
assert (f (const 0x55555555l) nocall (ref 0) bound = 1);
assert (f (const 0xAAAAAAA9l) nocall (ref 0) bound = 1);
assert (f (const 0xAAAAAAAAl) nocall (ref 0) bound = 2);
assert (f (const 0xFFFFFFFEl) nocall (ref 0) bound = 2);
assert (try_get (f (const 0xFFFFFFFFl) nocall (ref 0)) bound);
assert (f (of_array [| 0xFFFFFFFFl; 0l |]) nocall (ref 0) bound = 0);
(* int_from_int64_bits *)
let f width bits = int_from_int64_bits ~width ~bits in
assert (try_int_from_int64_bits (f 1 nocall ()) bound);
assert (f 2 (const 0L) (ref 0) bound = 0);
assert (f 2 (const 1L) (ref 0) bound = 1);
assert (f 2 (const 2L) (ref 0) bound = 2);
assert (try_get (f 2 (const 3L) (ref 0)) bound);
assert (f 2 (of_array [| 0x3L; 0L |]) (ref 0) bound = 0);
assert (f 64 (const 0L) (ref 0) bound = 0);
assert (f 64 (const 0x5555555555555554L) (ref 0) bound = 0);
assert (f 64 (const 0x5555555555555555L) (ref 0) bound = 1);
assert (f 64 (const 0xAAAAAAAAAAAAAAA9L) (ref 0) bound = 1);
assert (f 64 (const 0xAAAAAAAAAAAAAAAAL) (ref 0) bound = 2);
assert (f 64 (const 0xFFFFFFFFFFFFFFFEL) (ref 0) bound = 2);
assert (try_get (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0)) bound);
assert (f 64 (of_array [| 0xFFFFFFFFFFFFFFFFL; 0L |]) (ref 0) bound = 0);
let boundl = Int32.of_int bound in
(* int32_from_int64_bits *)
let f width bits = int32_from_int64_bits ~width ~bits in
assert (try_int32_from_int64_bits (f 1 nocall ()) boundl);
let boundL = Int64.of_int bound in
(* int64_from_int64_bits *)
let f width bits = int64_from_int64_bits ~width ~bits in
assert (try_int64_from_int64_bits (f 1 nocall ()) boundL);;

(* 2**30 - 1, max of int on 32bit architecture *)
let bound = 0x3FFFFFFF in
assert (Sys.word_size <> 32 || bound = max_int);
(* int *)
let f bits32 bits64 = int ~bits32 ~bits64 in
assert (f (const 0l) nocall (ref 0) bound = 0);
assert (f (const 0x7FFFFFFFl) nocall (ref 0) bound = 0x1FFFFFFF);
assert (f (const 0x80000000l) nocall (ref 0) bound = 0x20000000);
assert (f (const 0xFFFFFFFBl) nocall (ref 0) bound = 0x3FFFFFFE);
assert (try_get (f (const 0xFFFFFFFCl) nocall (ref 0)) bound);
assert (try_get (f (const 0xFFFFFFFFl) nocall (ref 0)) bound);
(* int_from_int64_bits *)
let f width bits = int_from_int64_bits ~width ~bits in
assert (f 30 (const 0L) (ref 0) bound = 0);
assert (f 30 (const 0x1FFFFFFFL) (ref 0) bound = 0x1FFFFFFF);
assert (f 30 (const 0x20000000L) (ref 0) bound = 0x20000000);
assert (f 30 (const 0x3FFFFFFEL) (ref 0) bound = 0x3FFFFFFE);
assert (try_get (f 30 (const 0x3FFFFFFFL) (ref 0)) bound);
assert (f 64 (const 0L) (ref 0) bound = 0);
assert (f 64 (const 0x80000001FFFFFFFFL) (ref 0) bound = 0x1FFFFFFF);
assert (f 64 (const 0x8000000200000000L) (ref 0) bound = 0x20000000);
assert (f 64 (const 0xFFFFFFFFFFFFFFEFL) (ref 0) bound = 0x3FFFFFFE);
assert (try_get (f 64 (const 0xFFFFFFFFFFFFFFF0L) (ref 0)) bound);
assert (try_get (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0)) bound);;

(* 2**31 - 1, max of int32 *)
let boundl = 0x7FFFFFFFl in
assert (boundl = Int32.max_int);
(* int32 *)
let f bits32 = int32 ~bits32 in
assert (f (const 0l) (ref 0) boundl = 0l);
assert (f (const 0x7FFFFFFFl) (ref 0) boundl = 0x3FFFFFFFl);
assert (f (const 0x80000000l) (ref 0) boundl = 0x40000000l);
assert (f (const 0xFFFFFFFDl) (ref 0) boundl = 0x7FFFFFFEl);
assert (try_get (f (const 0xFFFFFFFEl) (ref 0)) boundl);
assert (try_get (f (const 0xFFFFFFFFl) (ref 0)) boundl);
(* int32_from_int64_bits *)
let f width bits = int32_from_int64_bits ~width ~bits in
assert (f 31 (const 0L) (ref 0) boundl = 0l);
assert (f 31 (const 0x3FFFFFFFL) (ref 0) boundl = 0x3FFFFFFFl);
assert (f 31 (const 0x40000000L) (ref 0) boundl = 0x40000000l);
assert (f 31 (const 0x7FFFFFFEL) (ref 0) boundl = 0x7FFFFFFEl);
assert (try_get (f 31 (const 0x7FFFFFFFL) (ref 0)) boundl);
assert (f 64 (const 0L) (ref 0) boundl = 0l);
assert (f 64 (const 0x80000000FFFFFFFFL) (ref 0) boundl = 0x3FFFFFFFl);
assert (f 64 (const 0x8000000100000000L) (ref 0) boundl = 0x40000000l);
assert (f 64 (const 0xFFFFFFFFFFFFFFFBL) (ref 0) boundl = 0x7FFFFFFEl);
assert (try_get (f 64 (const 0xFFFFFFFFFFFFFFFCL) (ref 0)) boundl);
assert (try_get (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0)) boundl);;

(* 2**31 *)
let boundL = 0x80000000L in
(* int64 *)
let f bits32 bits64 = int64 ~bits32 ~bits64 in
assert (f (const 0l) nocall (ref 0) boundL = 0L);
assert (f (const 0x7FFFFFFFl) nocall (ref 0) boundL = 0x3FFFFFFFL);
assert (f (const 0x80000000l) nocall (ref 0) boundL = 0x40000000L);
assert (f (const 0xFFFFFFFFl) nocall (ref 0) boundL = 0x7FFFFFFFL);
(* int64_from_int64_bits *)
let f width bits = int64_from_int64_bits ~width ~bits in
assert (f 31 (const 0L) (ref 0) boundL = 0L);
assert (f 31 (const 0x3FFFFFFFL) (ref 0) boundL = 0x3FFFFFFFL);
assert (f 31 (const 0x40000000L) (ref 0) boundL = 0x40000000L);
assert (f 31 (const 0x7FFFFFFFL) (ref 0) boundL = 0x7FFFFFFFL);
assert (f 64 (const 0L) (ref 0) boundL = 0L);
assert (f 64 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) boundL = 0x3FFFFFFFL);
assert (f 64 (const 0x8000000000000000L) (ref 0) boundL = 0x40000000L);
assert (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0) boundL = 0x7FFFFFFFL);
if Sys.word_size > 32 then (
	let bound = Int64.to_int boundL in
	(* int *)
	let f bits32 bits64 = int ~bits32 ~bits64 in
	assert (f (const 0l) nocall (ref 0) bound = 0);
	assert (f (const 0x7FFFFFFFl) nocall (ref 0) bound = 0x3FFFFFFF);
	assert (f (const 0x80000000l) nocall (ref 0) bound = 0x40000000);
	assert (f (const 0xFFFFFFFFl) nocall (ref 0) bound = 0x7FFFFFFF);
	(* int_from_int64_bits *)
	let f width bits = int_from_int64_bits ~width ~bits in
	assert (f 31 (const 0L) (ref 0) bound = 0);
	assert (f 31 (const 0x3FFFFFFFL) (ref 0) bound = 0x3FFFFFFF);
	assert (f 31 (const 0x40000000L) (ref 0) bound = 0x40000000);
	assert (f 31 (const 0x7FFFFFFFL) (ref 0) bound = 0x7FFFFFFF);
	assert (f 64 (const 0L) (ref 0) bound = 0);
	assert (f 64 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) bound = 0x3FFFFFFF);
	assert (f 64 (const 0x8000000000000000L) (ref 0) bound = 0x40000000);
	assert (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0) bound = 0x7FFFFFFF)
);;

(* 2**32 *)
let boundL = 0x100000000L in
(* int64 *)
let f bits32 bits64 = int64 ~bits32 ~bits64 in
assert (f (const 0l) nocall (ref 0) boundL = 0L);
assert (f (const 0x7FFFFFFFl) nocall (ref 0) boundL = 0x7FFFFFFFL);
assert (f (const 0x80000000l) nocall (ref 0) boundL = 0x80000000L);
assert (f (const 0xFFFFFFFFl) nocall (ref 0) boundL = 0xFFFFFFFFL);
(* int64_from_int64_bits *)
let f width bits = int64_from_int64_bits ~width ~bits in
assert (f 32 (const 0L) (ref 0) boundL = 0L);
assert (f 32 (const 0x7FFFFFFFL) (ref 0) boundL = 0x7FFFFFFFL);
assert (f 32 (const 0x80000000L) (ref 0) boundL = 0x80000000L);
assert (f 32 (const 0xFFFFFFFFL) (ref 0) boundL = 0xFFFFFFFFL);
assert (f 64 (const 0L) (ref 0) boundL = 0L);
assert (f 64 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) boundL = 0x7FFFFFFFL);
assert (f 64 (const 0x8000000000000000L) (ref 0) boundL = 0x80000000L);
assert (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0) boundL = 0xFFFFFFFFL);
if Sys.word_size > 32 then (
	let bound = Int64.to_int boundL in
	(* int *)
	let f bits32 bits64 = int ~bits32 ~bits64 in
	assert (f (const 0l) nocall (ref 0) bound = 0);
	assert (f (const 0x7FFFFFFFl) nocall (ref 0) bound = 0x7FFFFFFF);
	assert (f (const 0x80000000l) nocall (ref 0) bound = 1 lsl 31);
	assert (f (const 0xFFFFFFFFl) nocall (ref 0) bound = 1 lsl 32 - 1);
	(* int_from_int64_bits *)
	let f width bits = int_from_int64_bits ~width ~bits in
	assert (f 32 (const 0L) (ref 0) bound = 0);
	assert (f 32 (const 0x7FFFFFFFL) (ref 0) bound = 0x7FFFFFFF);
	assert (f 32 (const 0x80000000L) (ref 0) bound = 1 lsl 31);
	assert (f 32 (const 0xFFFFFFFFL) (ref 0) bound = 1 lsl 32 - 1);
	assert (f 64 (const 0L) (ref 0) bound = 0);
	assert (f 64 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) bound = 0x7FFFFFFF);
	assert (f 64 (const 0x8000000000000000L) (ref 0) bound = 1 lsl 31);
	assert (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0) bound = 1 lsl 32 - 1)
);;

(* 2**33 *)
let boundL = 0x200000000L in
(* int64 *)
let f bits32 bits64 = int64 ~bits32 ~bits64 in
assert (f nocall (const 0L) (ref 0) boundL = 0L);
assert (f nocall (const 0x7FFFFFFFFFFFFFFFL) (ref 0) boundL = 0xFFFFFFFFL);
assert (f nocall (const 0x8000000000000000L) (ref 0) boundL = 0x100000000L);
assert (f nocall (const 0xFFFFFFFFFFFFFFFFL) (ref 0) boundL = 0x1FFFFFFFFL);
(* int64_from_int64_bits *)
let f width bits = int64_from_int64_bits ~width ~bits in
assert (f 33 (const 0L) (ref 0) boundL = 0L);
assert (f 33 (const 0xFFFFFFFFL) (ref 0) boundL = 0xFFFFFFFFL);
assert (f 33 (const 0x100000000L) (ref 0) boundL = 0x100000000L);
assert (f 33 (const 0x1FFFFFFFFL) (ref 0) boundL = 0x1FFFFFFFFL);
assert (f 64 (const 0L) (ref 0) boundL = 0L);
assert (f 64 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) boundL = 0xFFFFFFFFL);
assert (f 64 (const 0x8000000000000000L) (ref 0) boundL = 0x100000000L);
assert (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0) boundL = 0x1FFFFFFFFL);
if Sys.word_size > 32 then (
	let bound = Int64.to_int boundL in
	(* int *)
	let f bits32 bits64 = int ~bits32 ~bits64 in
	assert (f nocall (const 0L) (ref 0) bound = 0);
	assert (f nocall (const 0x7FFFFFFFFFFFFFFFL) (ref 0) bound = 1 lsl 32 - 1);
	assert (f nocall (const 0x8000000000000000L) (ref 0) bound = 1 lsl 32);
	assert (f nocall (const 0xFFFFFFFFFFFFFFFFL) (ref 0) bound = 1 lsl 33 - 1);
	(* int_from_int64_bits *)
	let f width bits = int_from_int64_bits ~width ~bits in
	assert (f 33 (const 0L) (ref 0) bound = 0);
	assert (f 33 (const 0xFFFFFFFFL) (ref 0) bound = 1 lsl 32 - 1);
	assert (f 33 (const 0x100000000L) (ref 0) bound = 1 lsl 32);
	assert (f 33 (const 0x1FFFFFFFFL) (ref 0) bound = 1 lsl 33 - 1);
	assert (f 64 (const 0L) (ref 0) bound = 0);
	assert (f 64 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) bound = 1 lsl 32 - 1);
	assert (f 64 (const 0x8000000000000000L) (ref 0) bound = 1 lsl 32);
	assert (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0) bound = 1 lsl 33 - 1)
);;

(* 2**62 - 1, max of int on 64bit architecture *)
let boundL = 0x3FFFFFFFFFFFFFFFL in
assert (Sys.word_size <> 64 || Int64.to_int boundL = max_int);
if Sys.word_size > 32 then (
	let bound = Int64.to_int boundL in
	(* int *)
	let f bits32 bits64 = int ~bits32 ~bits64 in
	assert (f nocall (const 0L) (ref 0) bound = 0);
	assert (f nocall (const 0x7FFFFFFFFFFFFFFFL) (ref 0) bound = 1 lsl 61 - 1);
	assert (f nocall (const 0x8000000000000000L) (ref 0) bound = 1 lsl 61);
	assert (f nocall (const 0xFFFFFFFFFFFFFFFBL) (ref 0) bound = max_int - 1);
	assert (try_get (f nocall (const 0xFFFFFFFFFFFFFFFCL) (ref 0)) bound);
	assert (try_get (f nocall (const 0xFFFFFFFFFFFFFFFFL) (ref 0)) bound);
	(* int_from_int64_bits *)
	let f width bits = int_from_int64_bits ~width ~bits in
	assert (f 63 (const 0L) (ref 0) bound = 0);
	assert (f 63 (const 0x3FFFFFFFFFFFFFFFL) (ref 0) bound = 1 lsl 61 - 1);
	assert (f 63 (const 0x4000000000000000L) (ref 0) bound = 1 lsl 61);
	assert (f 63 (const 0x7FFFFFFFFFFFFFFDL) (ref 0) bound = max_int - 1);
	assert (try_get (f 63 (const 0x7FFFFFFFFFFFFFFEL) (ref 0)) bound);
	assert (try_get (f 63 (const 0x7FFFFFFFFFFFFFFFL) (ref 0)) bound);
	assert (f 64 (const 0L) (ref 0) bound = 0);
	assert (f 64 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) bound = 1 lsl 61 - 1);
	assert (f 64 (const 0x8000000000000000L) (ref 0) bound = 1 lsl 61);
	assert (f 64 (const 0xFFFFFFFFFFFFFFFBL) (ref 0) bound = max_int - 1);
	assert (try_get (f 64 (const 0xFFFFFFFFFFFFFFFCL) (ref 0)) bound);
	assert (try_get (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0)) bound)
);;

(* 2**63 - 1, max of int64 *)
let boundL = 0x7FFFFFFFFFFFFFFFL in
assert (boundL = Int64.max_int);
(* int64 *)
let f bits32 bits64 = int64 ~bits32 ~bits64 in
assert (f nocall (const 0L) (ref 0) boundL = 0L);
assert (
	f nocall (const 0x7FFFFFFFFFFFFFFFL) (ref 0) boundL = 0x3FFFFFFFFFFFFFFFL
);
assert (
	f nocall (const 0x8000000000000000L) (ref 0) boundL = 0x4000000000000000L
);
assert (
	f nocall (const 0xFFFFFFFFFFFFFFFDL) (ref 0) boundL = 0x7FFFFFFFFFFFFFFEL
);
assert (try_get (f nocall (const 0xFFFFFFFFFFFFFFFEL) (ref 0)) boundL);
assert (try_get (f nocall (const 0xFFFFFFFFFFFFFFFFL) (ref 0)) boundL);
(* int64_from_int64_bits *)
let f width bits = int64_from_int64_bits ~width ~bits in
assert (f 64 (const 0L) (ref 0) boundL = 0L);
assert (f 64 (const 0x7FFFFFFFFFFFFFFFL) (ref 0) boundL = 0x3FFFFFFFFFFFFFFFL);
assert (f 64 (const 0x8000000000000000L) (ref 0) boundL = 0x4000000000000000L);
assert (f 64 (const 0xFFFFFFFFFFFFFFFDL) (ref 0) boundL = 0x7FFFFFFFFFFFFFFEL);
assert (try_get (f 64 (const 0xFFFFFFFFFFFFFFFEL) (ref 0)) boundL);
assert (try_get (f 64 (const 0xFFFFFFFFFFFFFFFFL) (ref 0)) boundL);;

prerr_endline "ok";;
