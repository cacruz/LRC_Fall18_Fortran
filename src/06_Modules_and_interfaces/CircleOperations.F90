program CircleOperations
   use CircleMod   
   implicit none
   real :: rad
 
   write(*,'(A)') "Enter radius of circle"
   read(*,*) rad

   call setRadius(rad)

   write(*,100) "Area of circle is", areaCircle()
100 format (A, 2x, F11.2) 

end program CircleOperations