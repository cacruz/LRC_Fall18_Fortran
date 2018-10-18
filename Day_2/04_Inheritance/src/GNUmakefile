all: test_shapes.x

FC=gfortran
FFLAGS=-I.

SRCS:=$(wildcard *.F90)
OALL:=$(addsuffix .o, $(basename $(SRCS)))
OBJS:=$(filter-out $(OALL),test_shapes.o)

%.o %.mod: %.F90
	$(FC) -c $< $(FFLAGS)
clean:
	rm -f *.mod *.o *.x

test_shapes.x: $(OALL)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)
test_shapes.o:  $(OBJS)
polygon_mod.o:  shapes_mod.o 
ellipse_mod.o:  shapes_mod.o
rectangle_mod.o:  polygon_mod.o
circle_mod.o:  ellipse_mod.o
square_mod.o:  rectangle_mod.o
