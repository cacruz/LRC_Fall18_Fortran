module my_mod
   implicit none
   private
   real, allocatable, dimension(:) :: arr  ! scope of arr is my_mod
   public my_sub

contains

   subroutine my_sub(arr) ! arr is a dummy argument - not the same as arr above
      real, pointer :: arr(:)
      allocate(arr(4))
      arr = 2.0
   end subroutine my_sub

end module my_mod

program test_mod
   use my_mod
   implicit none
   real, pointer, dimension(:) :: arr
   call my_sub(arr)
   print *, arr
end program test_mod
