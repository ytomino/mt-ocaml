include Makefile.variables

MLI=mt19937ar.mli $(BUILDDIR)/sfmt_19937.mli
MLSRC=$(MLI:.mli=.ml)
MLINIT=
CSRC=mt19937ar_stub.c $(BUILDDIR)/sfmt_19937_stub.c

LIBNAME=mt
STUBLIBNAME=ml$(LIBNAME)

DEBUG=
ASSERT=

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g) $(if $(ASSERT),,-noassert) -w +a
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
OCAML_INCLUDE_FLAGS=
CFLAGS=$(and $(filter-out 0,$(DEBUG)),-g$(DEBUG)) $(if $(ASSERT),,-DNDEBUG) \
       -std=gnu11 -Wall -Wextra
C_INCLUDE_FLAGS=

CCLIB=
LDLIB=

include Makefile.rules

$(BUILDDIR)/mt19937ar_stub.o: private override C_INCLUDE_FLAGS+=-Imt19937ar
$(BUILDDIR)/mt19937ar_stub.o: mt19937ar/mt19937ar.c mt19937ar/mt19937ar.h

$(BUILDDIR)/sfmt_19937.mli: ../source/sfmt.mli.in | $(BUILDDIR)
	sed 's/\$${SFMT_MEXP}/19937/g' $< > $@

$(BUILDDIR)/sfmt_19937.ml: ../source/sfmt.ml.in | $(BUILDDIR)
	sed 's/\$${SFMT_MEXP}/19937/g' $< > $@

$(BUILDDIR)/sfmt_19937_stub.c: ../source/sfmt_stub.c.in | $(BUILDDIR)
	sed 's/\$${SFMT_MEXP}/19937/g' $< > $@

$(BUILDDIR)/sfmt_19937_stub.o: private override C_INCLUDE_FLAGS+=-Isfmt
$(BUILDDIR)/sfmt_19937_stub.o\
: sfmt/SFMT.c sfmt/SFMT.h sfmt/SFMT-common.h sfmt/SFMT-params.h \
  sfmt/SFMT-params19937.h
