! file shared.f90
subroutine mysub(x) bind(c,name="mysub")
   use iso_c_binding
   real(c_double), value :: x
   write(*,*) "mysub: x=",x
end subroutine 
