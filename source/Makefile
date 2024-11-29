include Makefile.variables

MLI=mt19937ar.mli
MLSRC=$(MLI:.mli=.ml)
MLINIT=
CSRC=mt19937ar_stub.c

LIBNAME=mt
STUBLIBNAME=ml$(LIBNAME)

DEBUG=
ASSERT=

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g) $(if $(ASSERT),,-noassert) -w +a
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
OCAML_INCLUDE_FLAGS=
CFLAGS=$(and $(filter-out 0,$(DEBUG)),-g$(DEBUG)) $(if $(ASSERT),,-DNDEBUG) \
       -Wall -Wextra
C_INCLUDE_FLAGS=

CCLIB=
LDLIB=

include Makefile.rules

$(BUILDDIR)/mt19937ar_stub.o: private override C_INCLUDE_FLAGS+=-Imt19937ar
$(BUILDDIR)/mt19937ar_stub.o: mt19937ar/mt19937ar.c mt19937ar/mt19937ar.h
