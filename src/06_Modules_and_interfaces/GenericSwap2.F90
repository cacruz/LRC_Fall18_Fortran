module generic_swap
   implicit none
   private
   public swap
   interface swap
      module procedure swap_i, swap_str
   end interface swap
   
contains
   
   subroutine swap_str(a, b)
      character(len=*)  :: a, b
      character(len=len(a))  :: temp
      temp = a 
      a = b 
      b = temp
   end subroutine swap_str
   
   subroutine swap_i(a, b)
      integer, intent (inout) :: a, b
      integer                 :: temp
      temp = a 
      a = b 
      b = temp
   end subroutine swap_i
   
end module generic_swap

program GenericSwap2
   use generic_swap
   implicit none
   integer        :: i, j
!   character(len=:), allocatable :: str1, str2 ! <<< Fortran 2003+
   character(len=2), allocatable :: str1, str2

   i = 1   
   j = 2      
   str1 = 'a '
   str2 = ' b'

   print *, i, j, '<',str1,'>',' <',str2,'>'
   call swap(i, j)
   call swap(str1, str2)
   print *, i, j, '<',str1,'>',' <',str2,'>'

end program GenericSwap2


