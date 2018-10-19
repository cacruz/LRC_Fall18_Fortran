all: exercise.x examples.x

exercise.x: exercise.F90
	gfortran -o $@ $^

examples.x: examples.F90
	gfortran -o $@ $^

.PHONY: clean
clean:
	rm -f *.o *.x
