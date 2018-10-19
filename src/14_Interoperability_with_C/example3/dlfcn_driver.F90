! file dlfcn.f90
program dlfcn_driver
   use iso_c_binding
   use iso_c_utilities
   use dlfcn
   implicit none
   
   ! local variables:
   character(kind=c_char,len=1024) :: dll_name, sub_name
   type(c_ptr) :: handle=c_null_ptr
   type(c_funptr) :: funptr=c_null_funptr
   integer(c_int) :: status

   ! the dynamic subroutine has a simple interface:
   abstract interface
      subroutine mysub(x) bind(c)
         use iso_c_binding
         real(c_double), value :: x
      end subroutine
   end interface
   procedure(mysub), pointer :: dll_sub ! dynamically-linked procedure
   
   write(*,*) "enter the name of the dl and the name of the dl subroutine:"
   read(*,"(a)") dll_name ! enter "shared.so"
   read(*,"(a)") sub_name ! enter "mysub"
   
   ! open the dl:
   handle=dlopen(trim(dll_name)//c_null_char, ior(rtld_now, rtld_global))
      ! the use of ior is not really proper...wait till fortran 2008  
   if(.not.c_associated(handle)) then
      write(*,*) "error in dlopen: ", c_f_string(dlerror())
      stop
   end if
   
   ! find the subroutine in the dl:
   funptr=dlsym(handle,trim(sub_name)//c_null_char)
   if(.not.c_associated(funptr)) then
      write(*,*) "error in dlsym: ", c_f_string(dlerror())
      stop
   end if
   ! now convert the c function pointer to a fortran procedure pointer
   call c_f_procpointer(cptr=funptr, fptr=dll_sub)
   ! finally, invoke the dynamically-linked subroutine:
   call dll_sub(1.0_c_double)
   
   ! now close the dl:
   status=dlclose(handle)
   if(status/=0) then
      write(*,*) "error in dlclose: ", c_f_string(dlerror())
      stop
   end if

end program
