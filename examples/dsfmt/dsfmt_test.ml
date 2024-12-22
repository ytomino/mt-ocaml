(*
Copyright (c) 2007, 2008, 2009 Mutsuo Saito, Makoto Matsumoto
and Hiroshima University.
Copyright (c) 2011, 2002 Mutsuo Saito, Makoto Matsumoto, Hiroshima
University and The University of Tokyo.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.
    * Neither the name of the Hiroshima University nor the names of
      its contributors may be used to endorse or promote products
      derived from this software without specific prior written
      permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
(* OCaml version by YT *)

module Dsfmt = Dsfmt_19937;;

let to_open_close r = 1. -. r;;
let to_open_open r =
	Int64.float_of_bits (Int64.logor (Int64.bits_of_float (r +. 1.)) 1L) -. 1.;;
let to_close1_open2 r = r +. 1.;;

let num_rands = 50000;;
let tic_mag = 1;;
let tic_count = 2000;;

let dummy = Array.Floatarray.create num_rands;;

type genrand_t = unit -> float;;
type st_genrand_t = Dsfmt.t -> float;;
type fill_array_t = unit -> floatarray -> int -> unit;;
type st_fill_array_t = Dsfmt.t -> floatarray -> int -> unit;;

let dsfmt_global_data = ref (Dsfmt.make_int32 0l);;

(* not inline wrapper functions for check() *)
let sst_genrand_close_open dsfmt = Dsfmt.float_bits52 dsfmt;;
let sst_genrand_open_close dsfmt = to_open_close (Dsfmt.float_bits52 dsfmt);;
let sst_genrand_open_open dsfmt = to_open_open (Dsfmt.float_bits52 dsfmt);;
let sst_genrand_close1_open2 dsfmt =
	to_close1_open2 (Dsfmt.float_bits52 dsfmt);;

let s_genrand_close_open () = sst_genrand_close_open !dsfmt_global_data;;
let s_genrand_open_close () = sst_genrand_open_close !dsfmt_global_data;;
let s_genrand_open_open () = sst_genrand_open_open !dsfmt_global_data;;
let s_genrand_close1_open2 () = sst_genrand_close1_open2 !dsfmt_global_data;;

let sst_fill_array_close_open dsfmt array size =
	Dsfmt.fill_floatarray52 dsfmt array 0 size;;
let sst_fill_array_open_close dsfmt array size =
	Dsfmt.fill_floatarray52 dsfmt array 0 size;
	for i = 0 to pred size do
		Array.Floatarray.set array i (to_open_close (Array.Floatarray.get array i))
	done;;
let sst_fill_array_open_open dsfmt array size =
	Dsfmt.fill_floatarray52 dsfmt array 0 size;
	for i = 0 to pred size do
		Array.Floatarray.set array i (to_open_open (Array.Floatarray.get array i))
	done;;
let sst_fill_array_close1_open2 dsfmt array size =
	Dsfmt.fill_floatarray52 dsfmt array 0 size;
	for i = 0 to pred size do
		Array.Floatarray.set array i (to_close1_open2 (Array.Floatarray.get array i))
	done;;

let s_fill_array_close_open () = sst_fill_array_close_open !dsfmt_global_data;;
let s_fill_array_open_close () = sst_fill_array_open_close !dsfmt_global_data;;
let s_fill_array_open_open () = sst_fill_array_open_open !dsfmt_global_data;;
let s_fill_array_close1_open2 () =
	sst_fill_array_close1_open2 !dsfmt_global_data;;

let check (range_str: string) (genrand: genrand_t) (fill_array: fill_array_t)
	(st_genrand: st_genrand_t) (st_fill_array: st_fill_array_t) (seed: int32)
	(print_size: int) =
	let little = Array.Floatarray.create ((Dsfmt.dsfmt_n + 1) * 2) in
	let array = dummy in
	let plittle = little in
	let lsize = Dsfmt.dsfmt_n * 2 + 2 in
	Printf.printf "generated randoms %s\n" range_str;
	dsfmt_global_data := Dsfmt.make_int32 seed;
	fill_array () plittle lsize;
	fill_array () array 5000;
	dsfmt_global_data := Dsfmt.make_int32 seed;
	let dsfmt = Dsfmt.make_int32 seed in
	for i = 0 to pred lsize do
		let r = genrand () in
		let r_u = Int64.bits_of_float r in
		let r_st = st_genrand dsfmt in
		let r_st_u = Int64.bits_of_float r_st in
		let plittle_i = Array.Floatarray.get plittle i in
		let plittle_i_u = Int64.bits_of_float plittle_i in
		if r_u <> r_st_u || r_u <> plittle_i_u then (
			Printf.printf
				("\n%s mismatch i = %d: r = %1.15f(%08Lx), "
					^^ "st = %1.15f(%08Lx)"
					^^ "array = %1.15f(%08Lx)\n"
				)
				range_str i r r_u r_st r_st_u plittle_i plittle_i_u;
			exit 1
		);
		if i < print_size then (
			Printf.printf "%1.15f " (Array.Floatarray.get plittle i);
			if i mod 4 = 3 then (
				Printf.printf "\n"
			)
		)
	done;
	for i = 0 to pred 5000 do
		let r = genrand () in
		let r_u = Int64.bits_of_float r in
		let array_i = Array.Floatarray.get array i in
		let array_i_u = Int64.bits_of_float array_i in
		if r_u <> array_i_u then (
			Printf.printf
				("\n%s mismatch i = %d: r = %1.15f(%08Lx), "
					^^ "array = %1.15f(%08Lx)\n"
				)
				range_str (i + lsize) r r_u array_i array_i_u;
			exit 1
		);
		if i + lsize < print_size then (
			Printf.printf "%1.15f " array_i;
			if (i + lsize) mod 4 = 3 then (
				Printf.printf "\n"
			)
		)
	done;
	let dsfmt = Dsfmt.make_int32 seed in
	st_fill_array dsfmt plittle lsize;
	st_fill_array dsfmt array 5000;
	let dsfmt = Dsfmt.make_int32 seed in
	for i = 0 to pred lsize do
		let r_st = st_genrand dsfmt in
		let r_st_u = Int64.bits_of_float r_st in
		let plittle_i = Array.Floatarray.get plittle i in
		let plittle_i_u = Int64.bits_of_float plittle_i in
		if r_st_u <> plittle_i_u then (
		Printf.printf
			("\n%s mismatch i = %d: st = %1.15f(%08Lx), "
				^^ "array = %1.15f(%08Lx)\n"
			)
			range_str i r_st r_st_u plittle_i plittle_i_u;
			exit 1
		)
	done;
	for i = 0 to pred 5000 do
		let r_st = st_genrand dsfmt in
		let r_st_u = Int64.bits_of_float r_st in
		let array_i = Array.Floatarray.get array i in
		let array_i_u = Int64.bits_of_float array_i in
		if r_st_u <> array_i_u then (
			Printf.printf
				("\n%s mismatch i = %d: r = %1.15f(%08Lx), "
					^^ "array = %1.15f(%08Lx)\n"
				)
				range_str (i + lsize) r_st r_st_u array_i array_i_u;
			exit 1
		)
	done;;

let check_ar (range_str: string) (genrand: genrand_t)
	(fill_array: fill_array_t) (st_genrand: st_genrand_t)
	(st_fill_array: st_fill_array_t) (print_size: int) =
	let little = Array.Floatarray.create ((Dsfmt.dsfmt_n + 1) * 2) in
	let array = dummy in
	let plittle = little in
	let lsize = Dsfmt.dsfmt_n * 2 + 2 in
	let ar = [| 1l; 2l; 3l; 4l |] in
	Printf.printf "generated randoms %s\n" range_str;
	dsfmt_global_data := Dsfmt.make_int32_array ar;
	fill_array () plittle lsize;
	fill_array () array 5000;
	dsfmt_global_data := Dsfmt.make_int32_array ar;
	let dsfmt = Dsfmt.make_int32_array ar in
	for i = 0 to pred lsize do
		let r = genrand () in
		let r_u = Int64.bits_of_float r in
		let r_st = st_genrand dsfmt in
		let r_st_u = Int64.bits_of_float r_st in
		let plittle_i = Array.Floatarray.get plittle i in
		let plittle_i_u = Int64.bits_of_float plittle_i in
		if r_u <> r_st_u || r_u <> plittle_i_u then (
			Printf.printf
				("\n%s mismatch i = %d: r = %1.15f(%08Lx), "
					^^ "st = %1.15f(%08Lx)"
					^^ "array = %1.15f(%08Lx)\n"
				)
				range_str i r r_u r_st r_st_u plittle_i plittle_i_u;
			exit 1
		);
		if i < print_size then (
			Printf.printf "%1.15f " plittle_i;
			if i mod 4 = 3 then (
				Printf.printf "\n"
			)
		)
	done;
	for i = 0 to pred 5000 do
		let r = genrand () in
		let r_u = Int64.bits_of_float r in
		let array_i = Array.Floatarray.get array i in
		let array_i_u = Int64.bits_of_float array_i in
		if r_u <> array_i_u then (
			Printf.printf
				("\n%s mismatch i = %d: r = %1.15f(%08Lx), "
					^^ "array = %1.15f(%08Lx)\n"
				)
				range_str (i + lsize) r r_u array_i array_i_u;
			exit 1
		);
		if i + lsize < print_size then (
			Printf.printf "%1.15f " array_i;
			if (i + lsize) mod 4 = 3 then (
				Printf.printf "\n"
			)
		)
	done;
	let dsfmt = Dsfmt.make_int32_array ar in
	st_fill_array dsfmt plittle lsize;
	st_fill_array dsfmt array 5000;
	let dsfmt = Dsfmt.make_int32_array ar in
	for i = 0 to pred lsize do
		let r_st = st_genrand dsfmt in
		let r_st_u = Int64.bits_of_float r_st in
		let plittle_i = Array.Floatarray.get plittle i in
		let plittle_i_u = Int64.bits_of_float plittle_i in
		if r_st_u <> plittle_i_u then (
			Printf.printf
				("\n%s mismatch i = %d: st = %1.15f(%08Lx), "
					^^ "array = %1.15f(%08Lx)\n"
				)
				range_str i r_st r_st_u plittle_i plittle_i_u;
			exit 1
		)
	done;
	for i = 0 to pred 5000 do
		let r_st = st_genrand dsfmt in
		let r_st_u = Int64.bits_of_float r_st in
		let array_i = Array.Floatarray.get array i in
		let array_i_u = Int64.bits_of_float array_i in
		if r_st_u <> array_i_u then (
			Printf.printf
				("\n%s mismatch i = %d: r = %1.15f(%08Lx), "
					^^ "array = %1.15f(%08Lx)\n"
				)
				range_str (i + lsize) r_st r_st_u array_i array_i_u;
			exit 1
		)
	done;;

let clock () = (Unix.times ()).Unix.tms_utime;;

let test_co () =
	let array = dummy in
(*
#if 0
	dsfmt_gv_init_gen_rand(1234);
	sum = 0;
	for (i = 0; i < 10; i++) {
		clo = clock();
		for (j = 0; j < TIC_COUNT; j++) {
			dsfmt_gv_fill_array_close_open(array, NUM_RANDS);
		}
		clo = clock() - clo;
		sum += clo;
	}
	printf("GL BLOCK [0, 1) AVE:%4"PRIu64"ms.\n",
		(sum * 100) / CLOCKS_PER_SEC);
#endif
*)
	let dsfmt = Dsfmt.make_int32 1234l in
	let sum = ref 0. in
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred tic_count do
			Dsfmt.fill_floatarray52 dsfmt array 0 num_rands
		done;
		let clo = clock () -. clo in
		sum := !sum +. clo
	done;
	Printf.printf "ST BLOCK [0, 1) AVE:%4Lums.\n" (Int64.of_float (!sum *. 100.))
	[@@ocaml.inline never];;

let test_oc () =
	let array = dummy in
(*
#if 0
	dsfmt_gv_init_gen_rand(1234);
	sum = 0;
	for (i = 0; i < 10; i++) {
		clo = clock();
		for (j = 0; j < TIC_COUNT; j++) {
			dsfmt_gv_fill_array_open_close(array, NUM_RANDS);
		}
		clo = clock() - clo;
		sum += clo;
	}
	printf("GL BLOCK (0, 1] AVE:%4"PRIu64"ms.\n",
		(sum * 100) / CLOCKS_PER_SEC);
#endif
*)
	let dsfmt = Dsfmt.make_int32 1234l in
	let sum = ref 0. in
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred tic_count do
			sst_fill_array_open_close dsfmt array num_rands
		done;
		let clo = clock () -. clo in
		sum := !sum +. clo
	done;
	Printf.printf "ST BLOCK (0, 1] AVE:%4Lums.\n" (Int64.of_float (!sum *. 100.))
	[@@ocaml.inline never];;

let test_oo () =
	let array = dummy in
(*
#if 0
	dsfmt_gv_init_gen_rand(1234);
	sum = 0;
	for (i = 0; i < 10; i++) {
		clo = clock();
		for (j = 0; j < TIC_COUNT; j++) {
			dsfmt_gv_fill_array_open_open(array, NUM_RANDS);
		}
		clo = clock() - clo;
		sum += clo;
	}
	printf("GL BLOCK (0, 1) AVE:%4"PRIu64"ms.\n",
		(sum * 100) / CLOCKS_PER_SEC);
#endif
*)
	let dsfmt = Dsfmt.make_int32 1234l in
	let sum = ref 0. in
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred tic_count do
			sst_fill_array_open_open dsfmt array num_rands
		done;
		let clo = clock () -. clo in
		sum := !sum +. clo
	done;
	Printf.printf "ST BLOCK (0, 1) AVE:%4Lums.\n" (Int64.of_float (!sum *. 100.))
	[@@ocaml.inline never];;

let test_12 () =
	let array = dummy in
(*
#if 0
	dsfmt_gv_init_gen_rand(1234);
	sum = 0;
	for (i = 0; i < 10; i++) {
		clo = clock();
		for (j = 0; j < TIC_COUNT; j++) {
			dsfmt_gv_fill_array_close1_open2(array, NUM_RANDS);
		}
		clo = clock() - clo;
		sum += clo;
	}
	printf("GL BLOCK [1, 2) AVE:%4"PRIu64"ms.\n",
		(sum * 100) / CLOCKS_PER_SEC);
#endif
*)
	let dsfmt = Dsfmt.make_int32 1234l in
	let sum = ref 0. in
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred tic_count do
			sst_fill_array_close1_open2 dsfmt array num_rands
		done;
		let clo = clock () -. clo in
		sum := !sum +. clo
	done;
	Printf.printf "ST BLOCK [1, 2) AVE:%4Lums.\n" (Int64.of_float (!sum *. 100.))
	[@@ocaml.inline never];;

let test_seq_co () =
	let array = dummy in
	let total = ref 0. in
(*
#if 0
	dsfmt_gv_init_gen_rand(1234);
	sum = 0;
	r = 0;
	for (i = 0; i < 10; i++) {
		clo = clock();
		for (j = 0; j < TIC_COUNT; j++) {
			for (k = 0; k < NUM_RANDS; k++) {
				r += dsfmt_gv_genrand_close_open();
			}
		}
		clo = clock() - clo;
		sum += clo;
	}
	total = r;
	printf("GL SEQ [0, 1) 1 AVE:%4"PRIu64"ms.\n",
		(sum * 100)  / CLOCKS_PER_SEC);
	
	sum = 0;
	for (i = 0; i < 10; i++) {
		clo = clock();
		for (j = 0; j < TIC_COUNT; j++) {
			for (k = 0; k < NUM_RANDS; k++) {
				array[k] = dsfmt_gv_genrand_close_open();
			}
		}
		clo = clock() - clo;
		sum += clo;
	}
	for (k = 0; k < NUM_RANDS; k++) {
		total += array[k];
	}
	printf("GL SEQ [0, 1) 2 AVE:%4"PRIu64"ms.\n",
		(sum * 100)  / CLOCKS_PER_SEC);
#endif
*)
	let dsfmt = Dsfmt.make_int32 1234l in
	let sum = ref 0. in
	let r = ref 0. in
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred tic_count do
			for k = 0 to pred num_rands do
				r := !r +. Dsfmt.float_bits52 dsfmt
			done
		done;
		let clo = clock () -. clo in
		sum := !sum +. clo
	done;
	total := (* !total +. *) !r;
	Printf.printf "ST SEQ [0, 1) 1 AVE:%4Lums.\n" (Int64.of_float (!sum *. 100.));
	
	sum := 0.;
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred tic_count do
			for k = 0 to pred num_rands do
				Array.Floatarray.set array k (Dsfmt.float_bits52 dsfmt)
			done
		done;
		let clo = clock() -. clo in
		sum := !sum +. clo
	done;
	for k = 0 to pred num_rands do
		total := !total +. Array.Floatarray.get array k
	done;
	Printf.printf "ST SEQ [0, 1) 2 AVE:%4Lums.\n" (Int64.of_float (!sum *. 100.));
	
	Printf.printf "total = %f\n" !total
	[@@ocaml.inline never];;

let test_seq_oc () =
	let array = dummy in
	let total = ref 0. in
(*
#if 0
	dsfmt_gv_init_gen_rand(1234);
	sum = 0;
	r = 0;
	for (i = 0; i < 10; i++) {
		clo = clock();
		for (j = 0; j < TIC_COUNT; j++) {
			for (k = 0; k < NUM_RANDS; k++) {
				r += dsfmt_gv_genrand_open_close();
			}
		}
		clo = clock() - clo;
		sum += clo;
	}
	total = r;
	printf("GL SEQ (0, 1] 1 AVE:%4"PRIu64"ms.\n",
		(sum * 100)  / CLOCKS_PER_SEC);
	sum = 0;
	for (i = 0; i < 10; i++) {
		clo = clock();
		for (j = 0; j < TIC_COUNT; j++) {
			for (k = 0; k < NUM_RANDS; k++) {
				array[k] = dsfmt_gv_genrand_open_close();
			}
		}
		clo = clock() - clo;
		sum += clo;
	}
	for (k = 0; k < NUM_RANDS; k++) {
		total += array[k];
	}
	printf("GL SEQ (0, 1] 2 AVE:%4"PRIu64"ms.\n",
		(sum * 100)  / CLOCKS_PER_SEC);
#endif
*)
	let dsfmt = Dsfmt.make_int32 1234l in
	let sum = ref 0. in
	let r = ref 0. in
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred tic_count do
			for k = 0 to pred num_rands do
				r := !r +. to_open_close (Dsfmt.float_bits52 dsfmt)
			done
		done;
		let clo = clock () -. clo in
		sum := !sum +. clo
	done;
	total := !total +. !r;
	Printf.printf "ST SEQ (0, 1] 1 AVE:%4Lums.\n" (Int64.of_float (!sum *. 100.));
	sum := 0.;
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred tic_count do
			for k = 0 to pred num_rands do
				Array.Floatarray.set array k (to_open_close (Dsfmt.float_bits52 dsfmt))
			done
		done;
		let clo = clock() -. clo in
		sum := !sum +. clo
	done;
	for k = 0 to pred num_rands do
		total := !total +. Array.Floatarray.get array k
	done;
	Printf.printf "ST SEQ (0, 1] 2 AVE:%4Lums.\n" (Int64.of_float (!sum *. 100.));
	Printf.printf "total = %f\n" !total
	[@@ocaml.inline never];;

let test_seq_oo () =
	let array = dummy in
	let total = ref 0. in
(*
#if 0
	dsfmt_gv_init_gen_rand(1234);
	sum = 0;
	r = 0;
	for (i = 0; i < 10; i++) {
		clo = clock();
		for (j = 0; j < TIC_COUNT; j++) {
			for (k = 0; k < NUM_RANDS; k++) {
				r += dsfmt_gv_genrand_open_open();
			}
		}
		clo = clock() - clo;
		sum += clo;
	}
	total = r;
	printf("GL SEQ (0, 1) 1 AVE:%4"PRIu64"ms.\n",
		(sum * 100)  / CLOCKS_PER_SEC);
	sum = 0;
	for (i = 0; i < 10; i++) {
		clo = clock();
		for (j = 0; j < TIC_COUNT; j++) {
			for (k = 0; k < NUM_RANDS; k++) {
				array[k] = dsfmt_gv_genrand_open_open();
			}
		}
		clo = clock() - clo;
		sum += clo;
	}
	for (k = 0; k < NUM_RANDS; k++) {
		total += array[k];
	}
	printf("GL SEQ (0, 1) 2 AVE:%4"PRIu64"ms.\n",
		(sum * 100)  / CLOCKS_PER_SEC);
#endif
*)
	let dsfmt = Dsfmt.make_int32 1234l in
	let sum = ref 0. in
	let r = ref 0. in
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred tic_count do
			for k = 0 to pred num_rands do
				r := !r +. to_open_open (Dsfmt.float_bits52 dsfmt)
			done
		done;
		let clo = clock () -. clo in
		sum := !sum +. clo
	done;
	total := !total +. !r;
	Printf.printf "ST SEQ (0, 1) 1 AVE:%4Lums.\n" (Int64.of_float (!sum *. 100.));
	sum := 0.;
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred tic_count do
			for k = 0 to pred num_rands do
				Array.Floatarray.set array k (to_open_open (Dsfmt.float_bits52 dsfmt))
			done
		done;
		let clo = clock() -. clo in
		sum := !sum +. clo
	done;
	for k = 0 to pred num_rands do
		total := !total +. Array.Floatarray.get array k
	done;
	Printf.printf "ST SEQ (0, 1) 2 AVE:%4Lums.\n" (Int64.of_float (!sum *. 100.));
	Printf.printf "total = %f\n" !total
	[@@ocaml.inline never];;

let test_seq_12 () =
	let array = dummy in
	let total = ref 0. in
(*
#if 0
	dsfmt_gv_init_gen_rand(1234);
	sum = 0;
	r = 0;
	for (i = 0; i < 10; i++) {
		clo = clock();
		for (j = 0; j < TIC_COUNT; j++) {
			for (k = 0; k < NUM_RANDS; k++) {
				r += dsfmt_gv_genrand_close1_open2();
			}
		}
		clo = clock() - clo;
		sum += clo;
	}
	total = r;
	printf("GL SEQ [1, 2) 1 AVE:%4"PRIu64"ms.\n",
		(sum * 100)  / CLOCKS_PER_SEC);
	sum = 0;
	for (i = 0; i < 10; i++) {
		clo = clock();
		for (j = 0; j < TIC_COUNT; j++) {
			for (k = 0; k < NUM_RANDS; k++) {
				array[k] = dsfmt_gv_genrand_close1_open2();
			}
		}
		clo = clock() - clo;
		sum += clo;
	}
	for (k = 0; k < NUM_RANDS; k++) {
		total += array[k];
	}
	printf("GL SEQ [1, 2) 2 AVE:%4"PRIu64"ms.\n",
		(sum * 100)  / CLOCKS_PER_SEC);
#endif
*)
	let dsfmt = Dsfmt.make_int32 1234l in
	let sum = ref 0. in
	let r = ref 0. in
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred tic_count do
			for k = 0 to pred num_rands do
				r := !r +. to_close1_open2 (Dsfmt.float_bits52 dsfmt)
			done
		done;
		let clo = clock () -. clo in
		sum := !sum +. clo
	done;
	total := !total +. !r;
	Printf.printf "ST SEQ [1, 2) 1 AVE:%4Lums.\n" (Int64.of_float (!sum *. 100.));
	sum := 0.;
	for i = 0 to pred 10 do
		let clo = clock () in
		for j = 0 to pred tic_count do
			for k = 0 to pred num_rands do
				Array.Floatarray.set array k (to_close1_open2 (Dsfmt.float_bits52 dsfmt))
			done
		done;
		let clo = clock() -. clo in
		sum := !sum +. clo
	done;
	for k = 0 to pred num_rands do
		total := !total +. Array.Floatarray.get array k
	done;
	Printf.printf "ST SEQ [1, 2) 2 AVE:%4Lums.\n" (Int64.of_float (!sum *. 100.));
	Printf.printf "total = %f\n" !total
	[@@ocaml.inline never];;

if Array.length Sys.argv >= 2 && Sys.argv.(1) = "-s" then (
	Printf.printf "consumed time for generating %d randoms.\n"
		(num_rands * tic_count);
	let old_control = Gc.get () in
	Gc.set {old_control with Gc.max_overhead = 1000000}; (* disable GC *)
	test_co ();
	test_oc ();
	test_oo ();
	test_12 ();
	test_seq_co ();
	test_seq_oc ();
	test_seq_oo ();
	test_seq_12 ();
	Gc.set old_control
) else (
	Printf.printf "%s\n" (Dsfmt.get_id_string ());
	Printf.printf "init_gen_rand(0) ";
	check "[1, 2)" s_genrand_close1_open2 s_fill_array_close1_open2
		sst_genrand_close1_open2 sst_fill_array_close1_open2 0l 1000;
	for i = 0 to pred 20 do
		Printf.printf "init_gen_rand(%d) " i;
		match i mod 4 with
		| 0 ->
			check "[0, 1)" s_genrand_close_open s_fill_array_close_open
				sst_genrand_close_open sst_fill_array_close_open (Int32.of_int i) 12
		| 1 ->
			check "(0, 1]" s_genrand_open_close s_fill_array_open_close
				sst_genrand_open_close sst_fill_array_open_close (Int32.of_int i) 12
		| 2 ->
			check "(0, 1)" s_genrand_open_open s_fill_array_open_open
				sst_genrand_open_open sst_fill_array_open_open (Int32.of_int i) 12
		| _ -> (* 3 *)
			check "[1, 2)" s_genrand_close1_open2 s_fill_array_close1_open2
				sst_genrand_close1_open2 sst_fill_array_close1_open2 (Int32.of_int i) 12
	done;
	Printf.printf "init_by_array {1, 2, 3, 4} ";
	check_ar "[1, 2)" s_genrand_close1_open2 s_fill_array_close1_open2
		sst_genrand_close1_open2 sst_fill_array_close1_open2 1000
);;