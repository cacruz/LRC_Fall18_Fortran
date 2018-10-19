subroutine simulation(arrays) bind(c)
   use iso_c_binding

   implicit none

   type, bind(c) :: pass
      integer (c_int) :: lenc, lenf
      type (c_ptr) :: c, f
   end type pass

   type (pass), intent(inout) :: arrays
   real (c_float), allocatable, target, save :: eta(:)
   real (c_float), pointer :: c_array(:)
   integer :: i

! associate c_array with an array allocated in C
   call c_f_pointer (arrays%c, c_array, (/arrays%lenc/) )
   print *, c_array
! allocate an array and make it available in C
   arrays%lenf = 3
   allocate (eta(arrays%lenf))
   do i = 1,arrays%lenf
      eta(i) = 10.*i
   enddo
   arrays%f = c_loc(eta)
end subroutine simulation
