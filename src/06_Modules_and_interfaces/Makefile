BIN=CircleOperations.exe
SRCS:=$(wildcard *.F90)
OBJS:=$(addsuffix .o, $(basename $(SRCS)))
$(BIN): $(OBJS)
	gfortran $^ -o $@
%.o: %.F90
	gfortran -c $^
clean:
	rm -f $(OBJS) $(BIN)
