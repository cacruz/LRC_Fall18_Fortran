all: circle.x swap2.x

%.o %.mod: %.F90
	gfortran -c $<

circle.x: CircleMod.o CircleOperations.o
	gfortran -o $@ $^

swap2.x: GenericSwap2.F90
	gfortran -o $@ $^

.PHONY: clean
clean:
	rm -f *.mod *.o *.x
