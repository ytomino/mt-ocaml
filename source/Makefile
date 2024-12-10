include Makefile.variables

MLI=mt19937ar.mli $(BUILDDIR)/sfmt_19937.mli $(BUILDDIR)/sfmt_216091.mli
MLSRC=$(MLI:.mli=.ml)
MLINIT=
CSRC=mt19937ar_stub.c $(BUILDDIR)/sfmt_19937_stub.c \
     $(BUILDDIR)/sfmt_216091_stub.c

LIBNAME=mt
STUBLIBNAME=ml$(LIBNAME)

DEBUG=
ASSERT=

SIMD=$(and $(filter amd64-% x86_64-%,$(TARGET)),SSE2)

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
$(BUILDDIR)/mt19937ar_stub.o \
: mt19937ar/mt19937ar.c mt19937ar/mt19937ar.h unexport.h

$(BUILDDIR)/sfmt_%.mli: ../source/sfmt.mli.in | $(BUILDDIR)
	sed 's/\$${SFMT_MEXP}/$*/g' $< > $@

$(BUILDDIR)/sfmt_%.ml: ../source/sfmt.ml.in | $(BUILDDIR)
	sed 's/\$${SFMT_MEXP}/$*/g' $< > $@

$(BUILDDIR)/sfmt_%_stub.c: ../source/sfmt_stub.c.in | $(BUILDDIR)
	sed 's/\$${SFMT_MEXP}/$*/g' $< > $@

$(BUILDDIR)/sfmt_%_stub.o: private override C_INCLUDE_FLAGS+=-I. -Isfmt
$(BUILDDIR)/sfmt_%_stub.o \
: sfmt/SFMT.c sfmt/SFMT.h sfmt/SFMT-common.h sfmt/SFMT-params.h unexport.h
ifeq ($(SIMD),SSE2)
$(BUILDDIR)/sfmt_%_stub.o: private override CFLAGS+=-msse2 -DHAVE_SSE2
$(BUILDDIR)/sfmt_%_stub.o: sfmt/SFMT-sse2.h
endif

$(BUILDDIR)/sfmt_19937_stub.o: sfmt/SFMT-params19937.h
$(BUILDDIR)/sfmt_216091_stub.o: sfmt/SFMT-params216091.h
