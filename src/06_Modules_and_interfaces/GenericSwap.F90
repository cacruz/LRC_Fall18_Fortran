program GenericSwap
   implicit none
   integer        :: i, j
   character      :: c, d

   !Add the proper generic interface block below to enable this code
   interface swap
      subroutine swap_i(a, b)
        implicit none
        integer, intent (inout) :: a, b
     end subroutine swap_i

     subroutine swap_c(a, b)
       implicit none
       character, intent (inout) :: a, b
     end subroutine swap_c
   end interface

   i = 1   
   j = 2      
   c = 'a'
   d = 'b'

   print *, i, j, c, d
   call swap(i, j) 
   call swap(c, d)
   print *, i, j, c, d

end program 


subroutine swap_i(a, b)
  implicit none
  integer, intent (inout) :: a, b
  integer                 :: temp
  temp = a 
  a = b 
  b = temp
end subroutine swap_i

subroutine swap_c(a, b)
  implicit none
  character, intent (inout) :: a, b
  character                 :: temp
  temp = a 
  a = b 
  b = temp
end subroutine swap_c
