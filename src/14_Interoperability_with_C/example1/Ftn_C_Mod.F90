module Ftn_C
   interface
      subroutine C_Library_Function(sendbuf, count, recvbuf) bind(c, name='C_Library_Function')
         use iso_c_binding
         implicit none
         real (c_float) :: sendbuf(*)
         integer (c_int), value :: count
         real (c_float) :: recvbuf(*)
      end subroutine C_Library_Function
   end interface
end module Ftn_C
