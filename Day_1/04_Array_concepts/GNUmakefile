all: arrays_exercise.x arrays_examples.x

arrays_exercise.x: arrays_exercise.F90
	gfortran -o $@ $^

arrays_examples.x: arrays_examples.F90
	gfortran -o $@ $^

.PHONY: clean
clean:
	rm -f *.o *.x
