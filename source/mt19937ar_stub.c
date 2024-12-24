#include "unexport.h"
#include "mt19937ar.c"

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <assert.h>
#include <string.h>

static struct custom_operations mt19937ar_ops = {
	.identifier = "jp.halfmoon.panathenaia.mt.mt19937ar",
	.finalize = custom_finalize_default,
	.compare = custom_compare_default,
	.hash = custom_hash_default,
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default,
	.compare_ext = custom_compare_ext_default,
	.fixed_length = custom_fixed_length_default};

static inline struct mt19937ar *mt19937ar_val(value x)
{
	return Data_custom_val(x);
}

#define ENTRY(NAME) mlmt_##NAME

CAMLprim value ENTRY(mt19937ar_make_int32)(value val_seed)
{
	CAMLparam1(val_seed);
	CAMLlocal1(val_result);
	val_result = caml_alloc_custom(&mt19937ar_ops, sizeof(struct mt19937ar), 0, 1);
	init_genrand(mt19937ar_val(val_result), Int32_val(val_seed));
	CAMLreturn(val_result);
}

CAMLprim value ENTRY(mt19937ar_make_int32_array)(value val_seed)
{
	CAMLparam1(val_seed); /* int32 array */
	CAMLlocal1(val_result);
	val_result = caml_alloc_custom(&mt19937ar_ops, sizeof(struct mt19937ar), 0, 1);
	size_t key_length = caml_array_length(val_seed);
	uint32_t init_key[key_length];
	for(size_t i = 0; i < key_length; ++i){
		init_key[i] = Int32_val(Field(val_seed, i));
	}
	init_by_array(mt19937ar_val(val_result), init_key, key_length);
	CAMLreturn(val_result);
}

CAMLprim value ENTRY(mt19937ar_copy)(value val_source)
{
	CAMLparam1(val_source);
	CAMLlocal1(val_result);
	val_result = caml_alloc_custom(&mt19937ar_ops, sizeof(struct mt19937ar), 0, 1);
	memcpy(
		mt19937ar_val(val_result), mt19937ar_val(val_source),
		sizeof(struct mt19937ar));
	CAMLreturn(val_result);
}

CAMLprim value ENTRY(mt19937ar_bits31)(value val_state)
{
	CAMLparam1(val_state);
	CAMLlocal1(val_result);
	val_result = Val_int(genrand_int31(mt19937ar_val(val_state)));
	CAMLreturn(val_result);
}

CAMLprim int32_t ENTRY(mt19937ar_bits32_unboxed)(value val_state)
{
	return genrand_int32(mt19937ar_val(val_state));
}

CAMLprim value ENTRY(mt19937ar_bits32)(value val_state)
{
	CAMLparam1(val_state);
	CAMLlocal1(val_result);
	val_result = caml_copy_int32(ENTRY(mt19937ar_bits32_unboxed)(val_state));
	CAMLreturn(val_result);
}

CAMLprim double ENTRY(mt19937ar_float_bits32_unboxed)(value val_state)
{
	return genrand_real2(mt19937ar_val(val_state));
}

CAMLprim value ENTRY(mt19937ar_float_bits32)(value val_state)
{
	CAMLparam1(val_state);
	CAMLlocal1(val_result);
	val_result =
		caml_copy_double(ENTRY(mt19937ar_float_bits32_unboxed)(val_state));
	CAMLreturn(val_result);
}

CAMLprim value ENTRY(mt19937ar_unsafe_import)(value val_source)
{
	CAMLparam1(val_source); /* int32 array * int */
	CAMLlocal2(val_result, val_mt);
	val_mt = Field(val_source, 0);
	intnat mti = Int_val(Field(val_source, 1));
	assert(caml_array_length(val_mt) == N && mti >= 0 && mti <= N);
	val_result = caml_alloc_custom(&mt19937ar_ops, sizeof(struct mt19937ar), 0, 1);
	struct mt19937ar *result = mt19937ar_val(val_result);
	for(size_t i = 0; i < N; ++i){
		result->mt[i] = Int32_val(Field(val_mt, i));
	}
	result->mti = mti;
	CAMLreturn(val_result);
}

static value mt19937ar_export_each(char const *e)
{
	return caml_copy_int32((uint32_t)(uintptr_t)e);
}

CAMLprim value ENTRY(mt19937ar_export)(value val_source)
{
	CAMLparam1(val_source);
	CAMLlocal2(val_result, val_mt);
	char const *mt[N + 1];
	struct mt19937ar const *source = mt19937ar_val(val_source);
	for(size_t i = 0; i < N; ++i){
		mt[i] = (char const *)(uintptr_t)source->mt[i];
	}
	mt[N] = NULL;
	int mti = source->mti;
	val_mt = caml_alloc_array(mt19937ar_export_each, mt);
	val_result = caml_alloc_tuple(2);
	Store_field(val_result, 0, val_mt);
	Store_field(val_result, 1, Val_int(mti));
	CAMLreturn(val_result);
}
