all: test_procedure.x test_data.x

FC=gfortran
FFLAGS=-I.

SRCS= shapes_mod.F90 polygon_mod.F90 rectangle_mod.F90 square_mod.F90 circle_mod.F90
OBJS= shapes_mod.o polygon_mod.o rectangle_mod.o square_mod.o circle_mod.o

#SRCS:=$(wildcard *.F90)
#OALL:=$(addsuffix .o, $(basename $(SRCS)))
#OBJS:=$(filter-out $(OALL),test_procedure.o)

%.o %.mod: %.F90
	$(FC) -c $< $(FFLAGS)
clean:
	rm -f *.mod *.o *.x

test_procedure.x: test_procedure.o $(OBJS)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

test_data.x: test_data.o $(OBJS)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

test_data.o:       $(OBJS)
test_procedure.o:  $(OBJS)
polygon_mod.o:     shapes_mod.o 
rectangle_mod.o:   polygon_mod.o
circle_mod.o:      polygon_mod.o 
square_mod.o:      rectangle_mod.o
