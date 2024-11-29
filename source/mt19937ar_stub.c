#include "mt19937ar.c"

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#define ENTRY(NAME) mlmt_##NAME

CAMLprim value ENTRY(mt19937ar_make_int32)(value val_seed)
{
	CAMLparam1(val_seed);
	init_genrand(Int32_val(val_seed));
	CAMLreturn(Val_unit);
}

CAMLprim value ENTRY(mt19937ar_make_int32_array)(value val_seed)
{
	CAMLparam1(val_seed); /* int32 array */
	size_t key_length = caml_array_length(val_seed);
	unsigned long init_key[key_length];
	for(size_t i = 0; i < key_length; ++i){
		init_key[i] = Int32_val(Field(val_seed, i));
	}
	init_by_array(init_key, key_length);
	CAMLreturn(Val_unit);
}

CAMLprim value ENTRY(mt19937ar_bits31)(value val_state)
{
	CAMLparam1(val_state);
	CAMLlocal1(val_result);
	val_result = Val_int(genrand_int31());
	CAMLreturn(val_result);
}

CAMLprim value ENTRY(mt19937ar_bits32)(value val_state)
{
	CAMLparam1(val_state);
	CAMLlocal1(val_result);
	val_result = caml_copy_int32(genrand_int32());
	CAMLreturn(val_result);
}

CAMLprim value ENTRY(mt19937ar_float_bits32)(value val_state)
{
	CAMLparam1(val_state);
	CAMLlocal1(val_result);
	val_result = caml_copy_double(genrand_real2());
	CAMLreturn(val_result);
}
