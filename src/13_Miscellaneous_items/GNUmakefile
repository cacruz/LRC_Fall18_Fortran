all: examples.x

examples.x: examples.F90
	gfortran -o $@ $^

.PHONY: clean
clean:
	rm -f *.o *.x
