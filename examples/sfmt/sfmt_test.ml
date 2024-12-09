(*
@file  test.c
@brief test program for 32-bit and 64-bit output of SFMT.

@author Mutsuo Saito (Hiroshima-univ)

Copyright (C) 2012 Mutsuo Saito, Makoto Matsumoto, Hiroshima
University and The University of Tokyo.
All rights reserved.

The new BSD License is applied to this software, see LICENSE.txt
*)
(* OCaml version by YT *)

module Sfmt = Sfmt_19937;;

let block_size = 100000;;
let block_size64 = 50000;;
let count = 1000;;

let array1 =
	Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout block_size;;
let array2 = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 10000;;

let array1_64 =
	Bigarray.Array1.create Bigarray.int64 Bigarray.c_layout (block_size / 2);;
let array2_64 = Bigarray.Array1.create Bigarray.int64 Bigarray.c_layout 5000;;

let check32 () =
	let array32 = array1 in
	let array32_2 = array2 in
	let ini = [| 0x1234l; 0x5678l; 0x9ABCl; 0xDEF0l |] in
	if Sfmt.min_int32_array_length > 10000 then (
		Printf.printf "array size too small!\n";
		exit 1
	);
	Printf.printf "%s\n32 bit generated randoms\n" (Sfmt.get_id_string ());
	Printf.printf "init_gen_rand__________\n";
	(* 32 bit generation *)
	let sfmt = Sfmt.make_int32 1234l in
	Sfmt.fill_int32_bigarray sfmt array32 0 10000;
	Sfmt.fill_int32_bigarray sfmt array32_2 0 10000;
	let sfmt = Sfmt.make_int32 1234l in
	for i = 0 to pred 10000 do
		if i < 1000 then (
			Printf.printf "%10lu " array32.{i};
			if i mod 5 = 4 then (
				Printf.printf "\n"
			)
		);
		let r32 = Sfmt.bits32 sfmt in
		if r32 <> array32.{i} then (
			Printf.printf "\nmismatch at %d array32:%lx gen:%lx\n" i array32.{i} r32;
			exit 1
		)
	done;
	for i = 0 to pred 700 do
		let r32 = Sfmt.bits32 sfmt in
		if r32 <> array32_2.{i} then (
			Printf.printf "\nmismatch at %d array32_2:%lx gen:%lx\n" i array32_2.{i} r32;
			exit 1
		)
	done;
	Printf.printf "\n";
	let sfmt = Sfmt.make_int32_array ini in
(*
#if defined(DEBUG)
	printf("first init_by_array\n");
	for (int i = 0; i < 2; i++) {
		for (int j = 0; j < 4; j++) {
			printf("%08"PRIx32" ", sfmt.state[i].u[j]);
		}
		printf("\n");
	}
#endif
*)
	Printf.printf "init_by_array__________\n";
	Sfmt.fill_int32_bigarray sfmt array32 0 10000;
	Sfmt.fill_int32_bigarray sfmt array32_2 0 10000;
	let sfmt = Sfmt.make_int32_array ini in
(*
#if defined(DEBUG)
	printf("second init_by_array\n");
	for (int i = 0; i < 2; i++) {
		for (int j = 0; j < 4; j++) {
			printf("%08"PRIx32" ", sfmt.state[i].u[j]);
		}
		printf("\n");
	}
#endif
*)
	for i = 0 to pred 10000 do
		if i < 1000 then (
			Printf.printf "%10lu " array32.{i};
			if i mod 5 = 4 then (
				Printf.printf "\n"
			)
		);
		let r32 = Sfmt.bits32 sfmt in
		if r32 <> array32.{i} then (
			Printf.printf "\nmismatch at %d array32:%lx gen:%lx\n" i array32.{i} r32;
			exit 1
		)
	done;
	for i = 0 to pred 700 do
		let r32 = Sfmt.bits32 sfmt in
		if r32 <> array32_2.{i} then (
			Printf.printf "\nmismatch at %d array32_2:%lx gen:%lx\n" i array32_2.{i} r32;
			exit 1
		)
	done;;

let clock () = (Unix.times ()).Unix.tms_utime;;

let speed32 () =
	let min = ref max_float in
	let array32 = array1 in
	if Sfmt.min_int32_array_length > block_size then (
		Printf.printf "array size too small!\n";
		exit 1
	);
	(* 32 bit generation *)
	let sfmt = Sfmt.make_int32 1234l in
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred count do
			Sfmt.fill_int32_bigarray sfmt array32 0 block_size;
		done;
		let clo = clock () -. clo in
		if clo < !min then (
			min := clo
		)
	done;
	Printf.printf "32 bit BLOCK:%.0f" (!min *. 1000.);
	Printf.printf "ms for %u randoms generation\n" (block_size * count);
	min := max_float;
	let sfmt = Sfmt.make_int32 1234l in
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred (block_size * count) do
			let _: int32 = Sfmt.bits32 sfmt in ()
		done;
		let clo = clock () -. clo in
		if clo < !min then (
			min := clo
		)
	done;
	Printf.printf "32 bit SEQUE:%.0f" (!min *. 1000.);
	Printf.printf "ms for %u randoms generation\n" (block_size * count);;

let check64 () =
	let ini = [| 5l; 4l; 3l; 2l; 1l |] in
	let array64 = array1_64 in
	let array64_2 = array2_64 in
	if Sfmt.min_int64_array_length > 5000 then (
		Printf.printf "array size too small!\n";
		exit 1
	);
	Printf.printf "%s\n64 bit generated randoms\n" (Sfmt.get_id_string ());
	Printf.printf "init_gen_rand__________\n";
	(* 64 bit generation *)
	let sfmt = Sfmt.make_int32 4321l in
	Sfmt.fill_int64_bigarray sfmt array64 0 5000;
	Sfmt.fill_int64_bigarray sfmt array64_2 0 5000;
	let sfmt = Sfmt.make_int32 4321l in
	for i = 0 to pred 5000 do
		if i < 1000 then (
			Printf.printf "%20Lu " array64.{i};
			if i mod 3 = 2 then (
				Printf.printf "\n"
			)
		);
		let r = Sfmt.bits64 sfmt in
		if r <> array64.{i} then (
			Printf.printf "\nmismatch at %d array64:%Lx gen:%Lx\n" i array64.{i} r;
			exit 1
		)
	done;
	Printf.printf "\n";
	for i = 0 to pred 700 do
		let r = Sfmt.bits64 sfmt in
		if r <> array64_2.{i} then (
			Printf.printf "\nmismatch at %d array64_2:%Lx gen:%Lx\n" i array64_2.{i} r;
			exit 1
		)
	done;
	Printf.printf "init_by_array__________\n";
	(* 64 bit generation *)
	let sfmt = Sfmt.make_int32_array ini in
	Sfmt.fill_int64_bigarray sfmt array64 0 5000;
	Sfmt.fill_int64_bigarray sfmt array64_2 0 5000;
	let sfmt = Sfmt.make_int32_array ini in
	for i = 0 to pred 5000 do
		if i < 1000 then (
			Printf.printf "%20Lu " array64.{i};
			if i mod 3 = 2 then (
				Printf.printf "\n"
			)
		);
		let r = Sfmt.bits64 sfmt in
		if r <> array64.{i} then (
			Printf.printf "\nmismatch at %d array64:%Lx gen:%Lx\n" i array64.{i} r;
			exit 1
		)
	done;
	Printf.printf "\n";
	for i = 0 to pred 700 do
		let r = Sfmt.bits64 sfmt in
		if r <> array64_2.{i} then (
			Printf.printf "\nmismatch at %d array64_2:%Lx gen:%Lx\n" i array64_2.{i} r;
			exit 1
		)
	done;;

let speed64 () =
	let min = ref max_float in
	let array64 = array1_64 in
	if Sfmt.min_int64_array_length > block_size64 then (
		Printf.printf "array size too small!\n";
		exit 1
	);
	(* 64 bit generation *)
	let sfmt = Sfmt.make_int32 1234l in
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred count do
			Sfmt.fill_int64_bigarray sfmt array64 0 block_size64
		done;
		let clo = clock () -. clo in
		if clo < !min then (
			min := clo
		)
	done;
	Printf.printf "64 bit BLOCK:%.0f" (!min *. 1000.);
	Printf.printf "ms for %u randoms generation\n" (block_size64 * count);
	min := max_float;
	let sfmt = Sfmt.make_int32 1234l in
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred (block_size64 * count) do
			let _: int64 = Sfmt.bits64 sfmt in ()
		done;
		let clo = clock () -. clo in
		if clo < !min then (
			min := clo
		)
	done;
	Printf.printf "64 bit SEQUE:%.0f" (!min *. 1000.);
	Printf.printf "ms for %u randoms generation\n" (block_size64 * count);;

let speed = ref false in
let bit64 = ref false in
let bit32 = ref false in
for i = 1 to Array.length Sys.argv - 1 do
	match Sys.argv.(i) with
	| "-s" -> speed := true
	| "-b64" -> bit64 := true
	| "-b32" -> bit32 := true
	| _ -> exit 2
done;
if not (!speed || !bit32 || !bit64) then (
	Printf.printf "usage:\n%s [-s | -b32 | -b64]\n" Sys.argv.(0);
	exit 0
);
if !speed then (
	let old_control = Gc.get () in
	Gc.set {old_control with Gc.max_overhead = 1000000}; (* disable GC *)
	speed32 ();
	speed64 ();
	Gc.set old_control
);
if !bit32 then (
	check32 ()
);
if !bit64 then (
	check64 ()
);;
