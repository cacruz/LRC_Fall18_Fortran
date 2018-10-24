program CircleOperations
   use CircleMod   
   implicit none
   real :: rad
 
   print *, "Enter radius of circle"
   read *, rad

   call setRadius(rad)

   print *, "Area of circle is", areaCircle() 

end program CircleOperations