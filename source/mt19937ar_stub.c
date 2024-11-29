#define STATIC static
#if defined(__GNUC__) || defined(__clang__)
#define UNUSED __attribute__((unused))
#else
#define UNUSED
#endif

#include "mt19937ar.c"

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

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

CAMLprim value ENTRY(mt19937ar_bits31)(value val_state)
{
	CAMLparam1(val_state);
	CAMLlocal1(val_result);
	val_result = Val_int(genrand_int31(mt19937ar_val(val_state)));
	CAMLreturn(val_result);
}

CAMLprim value ENTRY(mt19937ar_bits32)(value val_state)
{
	CAMLparam1(val_state);
	CAMLlocal1(val_result);
	val_result = caml_copy_int32(genrand_int32(mt19937ar_val(val_state)));
	CAMLreturn(val_result);
}

CAMLprim value ENTRY(mt19937ar_float_bits32)(value val_state)
{
	CAMLparam1(val_state);
	CAMLlocal1(val_result);
	val_result = caml_copy_double(genrand_real2(mt19937ar_val(val_state)));
	CAMLreturn(val_result);
}
