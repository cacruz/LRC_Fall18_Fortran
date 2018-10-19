
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

all: writeUstream.ex readUstream.ex exampleAsyncIO.ex

writeUstream.ex: writeUstream.o
	$(FC) -o writeUstream.ex writeUstream.o

readUstream.ex: readUstream.o
	$(FC) -o readUstream.ex readUstream.o

exampleAsyncIO.ex: exampleAsyncIO.o
	$(FC) -o exampleAsyncIO.ex exampleAsyncIO.o

clean:
	rm -f *.o *.mod *.ex *.demo

###################################################
#
###################################################

.SUFFIXES:
.SUFFIXES: .o .c .f .F90 .h

.c.o:
	$(CC) $(CFLAGS) $(DEPEND_INC) $*.c

.F90.o: $(FSOURCE)
	$(FC) $(FFLAGS) $(DEPEND_INC) $(LINC) $?
