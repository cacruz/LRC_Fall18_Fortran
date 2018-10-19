FC = gfortran
CC = gcc
SRCS_F90= $(wildcard *.F90)
SRCS_c= $(wildcard *.c)
OBJS_ALL = $(SRCS_F90:.F90=.o)  \
           $(SRCS_c:.c=.o)

global.x: $(OBJS_ALL)
	    ${FC} -o $@ $(OBJS_ALL)

%.o:    %.F90
	$(FC) -c $*.F90

%.o:    %.c
	$(CC) -c $*.c

clean:
	rm -f *.o *.mod global.x
