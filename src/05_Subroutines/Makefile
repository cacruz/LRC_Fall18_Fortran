DEBUG_FLAG    =
CC_OPTIMIZE   = -O2
OPTIMIZE_FLAG = -O2
LDR_OPTS      =

MAKE       = make

MV         = mv

FC         = gfortran
FFLAGS     = -c  $(DEBUG_FLAG) $(OPTIMIZE_FLAG)

LDR        = $(FC)
LDFLAGS    = $(LDR_OPTS)

FSOURCE    = *.F90

all: exampleSubroutine.ex exampleFunction.ex

exampleSubroutine.ex: exampleSubroutine.o
	$(FC) -o exampleSubroutine.ex exampleSubroutine.o

exampleFunction.ex: exampleFunction.o
	$(FC) -o exampleFunction.ex exampleFunction.o

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

