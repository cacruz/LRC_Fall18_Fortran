FC = gfortran
CC = gcc
SRCS_F90= $(wildcard *.F90)
SRCS_c= $(wildcard *.c)
OBJS_ALL = $(SRCS_F90:.F90=.o)  \
           $(SRCS_c:.c=.o)

example1.x: $(OBJS_ALL)
	    ${FC} -o $@ $(OBJS_ALL)

%.o:    %.F90
	$(FC) -c $*.F90

%.o:    %.c
	$(CC) -c $*.c

Ftn_C_Driver.o: Ftn_C_Mod.o

clean:
	rm -f *.o *.mod example1.x
