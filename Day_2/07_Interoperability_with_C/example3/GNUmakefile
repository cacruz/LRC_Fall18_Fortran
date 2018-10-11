UNAME = $(shell uname)
FC = gfortran 

SRCS_F90= $(wildcard *.F90)
OBJS_ALL = $(SRCS_F90:.F90=.o) 

example3.x: $(OBJS_ALL)
	    ${FC} -o $@ $(OBJS_ALL) -ldl

%.o:    %.F90
	$(FC) -c -fPIC $*.F90
ifeq ($(UNAME),Darwin)
mysub.dylib: mysub.o
	gcc -dynamiclib -o mysub.dylib mysub.o -L/usr/local/gfortran/lib -lgfortran
else
mysub.so: mysub.o
	ld -shared mysub.o -o $@
endif

clean:
	rm -f *.o *.mod *.dylib *.so example3.x

ifeq ($(UNAME),Darwin)
dlfcn_driver.o:iso_c_utilities_mod.o dlfcn_mod.o mysub.dylib
else
dlfcn_driver.o:iso_c_utilities_mod.o dlfcn_mod.o mysub.so
endif
dlfcn_mod.o:iso_c_utilities_mod.o
