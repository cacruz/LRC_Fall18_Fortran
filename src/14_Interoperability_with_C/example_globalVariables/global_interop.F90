program main
   USE ISO_C_BINDING

   common /COM/ r, s
   real(C_FLOAT) :: r, s
   BIND(C) :: /COM/

   interface
      subroutine initCommonBlock() BIND(C, NAME='initCom')
         use ISO_C_BINDING
      end subroutine initCommonBlock
   end interface

   
   call initCommonBlock()

   print*,'found:     ',r, s
   print*,'should be: ',3., 4.

end program main
