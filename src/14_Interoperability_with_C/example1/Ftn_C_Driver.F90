! Example of a Fortran code calling a C function
program Ftn_C_Driver

   use iso_c_binding, only: c_int, c_float
   use Ftn_C

   integer (c_int) :: count = 5
   real (c_float), allocatable :: send(:)
   real (c_float), allocatable :: recv(:)

   allocate( send(count), recv(count) )
   send = 10.0
   call C_Library_Function(send, count, recv)

   print *, " Result is"
   print *, recv
   print * 
   print *, "Correct result should be  10.0 20.0 30.0 40.0 50.0"

end program Ftn_C_Driver
