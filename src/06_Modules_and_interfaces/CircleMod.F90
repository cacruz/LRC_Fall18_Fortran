module CircleMod
   implicit none
 
   private
   public  areaCircle, setRadius
   real, parameter :: PI = 3.1415927
   real :: radius

CONTAINS
   subroutine setRadius(r)
      implicit none
      real, intent(in) :: r
      radius = r
   end subroutine
      
   real function areaCircle()
      implicit none
      areaCircle = PI * radius**2
   end function areaCircle
end module CircleMod