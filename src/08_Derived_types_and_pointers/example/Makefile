DEBUG_FLAG    = -g 
OPTIMIZE_FLAG = -O2
LDR_OPTS      =

MAKE       = make

MV         = mv

FC         = gfortran
FFLAGS     = -c  $(DEBUG_FLAG) $(OPTIMIZE_FLAG)

LDR        = $(FC)
LDFLAGS    = $(LDR_OPTS)

FSOURCE    = *.F90

OBJS = listMod.o supportingRoutines.o listProgram.o

all: listProgram.ex

listProgram.ex: $(OBJS)
	$(FC) -o listProgram.ex $(OBJS)

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

