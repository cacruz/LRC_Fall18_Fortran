
DEBUG_FLAG    =
CC_OPTIMIZE   = -O2
OPTIMIZE_FLAG = -O2
LDR_OPTS      =

MAKE       = make

MV         = mv

#FC = /opt/local/bin/gfortran-mp-7

FC         = gfortran
FFLAGS     = -c  $(DEBUG_FLAG) $(OPTIMIZE_FLAG)

LDR        = $(FC)
LDFLAGS    = $(LDR_OPTS)

FSOURCE    = *.F90

all: testingComplex.ex testingInteger.ex testingReal.ex \
     testingCharacter.ex testingLogical.ex

testingCharacter.ex: testingCharacter.o
	$(FC) -o testingCharacter.ex testingCharacter.o

testingReal.ex: testingReal.o
	$(FC) -o testingReal.ex testingReal.o

testingLogical.ex: testingLogical.o
	$(FC) -o testingLogical.ex testingLogical.o

testingInteger.ex: testingInteger.o
	$(FC) -o testingInteger.ex testingInteger.o

testingComplex.ex: testingComplex.o
	$(FC) -o testingComplex.ex testingComplex.o


clean:
	rm -f *.o *.mod *.ex

###################################################
#
###################################################

.SUFFIXES:
.SUFFIXES: .o .c .f .F90 .h

.c.o:
	$(CC) $(CFLAGS) $(DEPEND_INC) $*.c

.F90.o: $(FSOURCE)
	$(FC) $(FFLAGS) $(DEPEND_INC) $(LINC) $?
