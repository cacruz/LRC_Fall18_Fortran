all: CircleOperations.exe GenericSwap.exe  swap2.exe swapProgram.exe

SRCS:=$(wildcard *.F90)
OBJS:=$(addsuffix .o, $(basename $(SRCS)))

CircleOperations.exe: CircleMod.o CircleOperations.o
	gfortran $^ -o $@

GenericSwap.exe: GenericSwap.o
	gfortran $^ -o $@

swap2.x: GenericSwap2.F90
	gfortran -o $@ $^

swapProgram.exe: swapMod.o swapProgram.o
	gfortran $^ -o $@

%.o: %.F90
	gfortran -c $^

clean:
	rm -f *.o *.mod *.exe 
