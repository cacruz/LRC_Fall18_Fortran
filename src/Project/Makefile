
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



SRCS       = definedConstantsMod.F90 calendarMod.F90 derivedTypeStationMod.F90 derivedTypeStationProgram.F90 classStationMod.F90 classStationProgram.F90

OBJS1      = definedConstantsMod.o calendarMod.o derivedTypeStationMod.o derivedTypeStationProgram.o 
OBJS2      = definedConstantsMod.o calendarMod.o classStationMod.o classStationProgram.o


FSOURCE    = *.F90

all: derivedTypeStationProgram.ex classStationProgram.ex

derivedTypeStationProgram.ex:   $(OBJS1) 
	$(FC) -o derivedTypeStationProgram.ex $(OBJS1) 

classStationProgram.ex:   $(OBJS2) 
	$(FC) -o classStationProgram.ex $(OBJS2) 

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
